use crate::lexicon::{
    Intern, Interner, Lexeme, Symbol, TokenAlias, TokenIdx, TokenList, TokenSpan,
};
use crate::locator::Locator;
use crate::span::Span;
use chumsky::prelude::*;
use generational_indextree::NodeEdge;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt::{Debug, Formatter};

pub type NodeIdx = generational_indextree::NodeId;

// Note: we need those bounds on the trait itself to deal with the
// incorrect bounds generated by derive (https://github.com/rust-lang/rust/issues/26925).
pub trait Grammar: Clone + Default + Debug {
    type Lex: Lexeme;
    type Kind: Copy + Clone + PartialEq + Debug;
}

#[derive(Clone)]
pub enum SyntaxTrunk<G: Grammar> {
    Leaf(TokenAlias<G::Lex>),
    Tree(G::Kind),
    Error,
}

impl<G: Grammar> Debug for SyntaxTrunk<G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxTrunk::Leaf(t) => write!(f, "Leaf[{:?}]", t.kind()),
            SyntaxTrunk::Tree(k) => write!(f, "Tree[{k:?}]"),
            SyntaxTrunk::Error => write!(f, "Error"),
        }
    }
}

impl<G: Grammar> Copy for SyntaxTrunk<G> {}

/// A trait for internally mutable node state.
pub trait Core: Default + Clone + Debug {}

impl<C: Default + Clone + Debug> Core for C {}

/// The intermediary node type to build a syntax tree from a parser.
///
/// Note: once Chumsky supports stateful combinators, it should be possible to build a
/// generational index tree in one pass, and get rid of this type.
#[derive(Clone, Debug)]
pub struct ParseNode<G: Grammar>(SyntaxTrunk<G>, Option<Vec<ParseNode<G>>>);

impl<G: Grammar> ParseNode<G> {
    pub fn from_leaf(token: TokenAlias<G::Lex>) -> Self {
        ParseNode(SyntaxTrunk::Leaf(token), None)
    }

    pub fn from_tree(kind: G::Kind, children: Vec<ParseNode<G>>) -> Self {
        ParseNode(SyntaxTrunk::Tree(kind), Some(children))
    }

    pub fn from_error() -> Self {
        ParseNode(SyntaxTrunk::Error, None)
    }

    pub fn to_tree<T: Core>(self, tree: &mut SyntaxTree<T, G>, parent: Option<NodeIdx>) -> NodeIdx {
        let ParseNode(trunk, children) = self;
        let node = tree.new_node(SyntaxNode::new(trunk, T::default()));
        if let Some(parent) = parent {
            tree.append(parent, node);
        }
        for child in children.into_iter().flatten() {
            child.to_tree(tree, Some(node));
        }
        node
    }
}

// TODO: move the core data structure to an arena to avoid wasting space on every single node.
#[derive(Clone, Debug)]
pub struct SyntaxNode<T: Core, G: Grammar>(SyntaxTrunk<G>, RefCell<T>);

impl<T: Core, G: Grammar> SyntaxNode<T, G> {
    pub fn new(trunk: SyntaxTrunk<G>, core: T) -> Self {
        SyntaxNode(trunk, RefCell::new(core))
    }

    pub fn trunk(&self) -> &SyntaxTrunk<G> {
        &self.0
    }

    pub fn core_ref(&self) -> Ref<T> {
        self.1.borrow()
    }

    pub fn core_mut(&self) -> RefMut<T> {
        self.1.borrow_mut()
    }
}

type TreeArena<T, G> = generational_indextree::Arena<SyntaxNode<T, G>>;

pub struct SyntaxTree<T: Core, G: Grammar> {
    tokens: TokenList<G::Lex>,
    tree: TreeArena<T, G>,
    root: Option<NodeIdx>,
}

impl<T: Core, G: Grammar> Interner for SyntaxTree<T, G> {
    fn register<S: AsRef<str>>(&mut self, s: S) -> Symbol {
        self.tokens.register(s)
    }

    fn resolve(&self, sym: Symbol) -> &str {
        self.tokens.resolve(sym)
    }
}

impl<T: Core, G: Grammar> Debug for SyntaxTree<T, G> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(root) = self.root {
            NodeRef::from(self, root).fmt(f)
        } else {
            write!(f, "[empty tree]")
        }
    }
}

impl<T: Core, G: Grammar> SyntaxTree<T, G> {
    pub fn new(loc: Locator) -> Self {
        SyntaxTree {
            tokens: TokenList::new(loc),
            tree: TreeArena::default(),
            root: Option::default(),
        }
    }

    pub fn locator(&self) -> &Locator {
        self.tokens.locator()
    }

    pub fn import(tokens: TokenList<G::Lex>, parse: ParseNode<G>) -> Self {
        let mut tree = SyntaxTree::new(tokens.locator().clone());
        let root = parse.to_tree(&mut tree, None);
        tree.tokens = tokens;
        tree.root = Some(root);
        tree
    }

    pub fn root(&self) -> NodeRef<T, G> {
        NodeRef::from(self, self.root.unwrap())
    }

    pub fn detach(&self, id: NodeIdx) -> SyntaxTree<T, G> {
        let mut tree = SyntaxTree::new(self.tokens.locator().clone());
        let mut parents: Vec<NodeIdx> = Vec::new();
        let mut root: Option<NodeIdx> = None;

        self.traverse(id).for_each(|edge| {
            match edge {
                NodeEdge::Start(id) => {
                    let node = self.node(id);

                    let trunk = match node.trunk() {
                        SyntaxTrunk::Leaf(t) => {
                            let kind = t.kind();
                            let index = t.index();

                            let (token, span) = self.tokens.get(index);

                            let new_value = token.value().copy(self, &mut tree);

                            let new_token = <<G as Grammar>::Lex as Lexeme>::new(kind, new_value);

                            let new_index = tree.push((new_token, span.clone()));

                            SyntaxTrunk::Leaf(TokenAlias::new(kind, new_index))
                        }
                        t => *t,
                    };

                    let core = node.core_ref().clone();

                    let syntax = SyntaxNode::new(trunk, core);

                    let id = tree.new_node(syntax);

                    if let Some(parent) = parents.last() {
                        tree.append(*parent, id)
                    }

                    parents.push(id);
                }
                NodeEdge::End(_) => {
                    root = parents.pop();
                }
            };
        });

        tree
    }

    pub fn token(&self, id: TokenIdx) -> &TokenSpan<G::Lex> {
        self.tokens.get(id)
    }

    fn node(&self, id: NodeIdx) -> &SyntaxNode<T, G> {
        self.tree.get(id).unwrap().get()
    }

    fn children(&self, id: NodeIdx) -> impl Iterator<Item = NodeIdx> + '_ {
        id.children(&self.tree)
    }

    fn reverse_children(&self, id: NodeIdx) -> impl Iterator<Item = NodeIdx> + '_ {
        id.reverse_children(&self.tree)
    }

    fn descendants(&self, id: NodeIdx) -> impl Iterator<Item = NodeIdx> + '_ {
        id.descendants(&self.tree)
    }

    fn traverse(&self, id: NodeIdx) -> impl Iterator<Item = NodeEdge> + '_ {
        id.traverse(&self.tree)
    }

    fn push(&mut self, t: TokenSpan<G::Lex>) -> TokenIdx {
        self.tokens.push(t)
    }

    fn new_node(&mut self, n: SyntaxNode<T, G>) -> NodeIdx {
        self.tree.new_node(n)
    }

    fn append(&mut self, parent: NodeIdx, child: NodeIdx) {
        parent.append(child, &mut self.tree);
    }
}

#[derive(Debug)]
pub struct TokenRef<'a, G: Grammar> {
    tokens: &'a TokenList<G::Lex>,
    idx: TokenIdx,
}

impl<'a, G: Grammar> TokenRef<'a, G> {
    fn from(tokens: &'a TokenList<G::Lex>, idx: TokenIdx) -> Self {
        TokenRef { tokens, idx }
    }

    fn token(&self) -> &'a <G as Grammar>::Lex {
        &self.tokens.get(self.idx).0
    }

    pub fn span(&self) -> &Span {
        &self.tokens.get(self.idx).1
    }

    pub fn value(&self) -> &'a <<G as Grammar>::Lex as Lexeme>::Value {
        self.token().value()
    }

    pub fn kind(&self) -> <<G as Grammar>::Lex as Lexeme>::Kind {
        self.token().kind()
    }
}

pub struct NodeRef<'a, T: Core, G: Grammar> {
    tree: &'a SyntaxTree<T, G>,
    idx: NodeIdx,
}

// Note: for some reason the derive macro is not doing the right thing for Clone/Copy.
impl<'a, T: Core, G: Grammar> Clone for NodeRef<'a, T, G> {
    fn clone(&self) -> Self {
        NodeRef {
            tree: self.tree,
            idx: self.idx,
        }
    }
}

impl<'a, T: Core, G: Grammar> Copy for NodeRef<'a, T, G> {}

#[derive(Debug)]
pub enum NodeCursor<'a, T: Core, G: Grammar> {
    Start(NodeRef<'a, T, G>),
    End(NodeRef<'a, T, G>),
}

impl<'a, T: Core, G: Grammar> NodeRef<'a, T, G> {
    pub fn from(tree: &'a SyntaxTree<T, G>, idx: NodeIdx) -> Self {
        NodeRef { tree, idx }
    }

    pub fn tree(&self) -> &'a SyntaxTree<T, G> {
        self.tree
    }

    pub fn index(&self) -> NodeIdx {
        self.idx
    }

    pub fn syntax(&self) -> &'a SyntaxNode<T, G> {
        self.tree.node(self.idx)
    }

    pub fn token(&self) -> TokenRef<'a, G> {
        match self.syntax().trunk() {
            SyntaxTrunk::Leaf(t) => TokenRef::from(&self.tree.tokens, t.index()),
            _ => panic!("a node must be a leaf to link to a token"),
        }
    }

    pub fn len(&self) -> usize {
        self.tree.children(self.idx).count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn children(&self) -> impl Iterator<Item = NodeRef<'a, T, G>> + 'a {
        self.tree
            .children(self.idx)
            .map(|id| NodeRef::from(self.tree, id))
    }

    pub fn reverse_children(&self) -> impl Iterator<Item = NodeRef<'a, T, G>> + 'a {
        self.tree
            .reverse_children(self.idx)
            .map(|id| NodeRef::from(self.tree, id))
    }

    pub fn first(&self) -> NodeRef<'a, T, G> {
        self.nth(0)
    }

    pub fn nth(&self, n: usize) -> NodeRef<'a, T, G> {
        let Some(node) = self.children().nth(n) else { panic!("expected node at {n}") };
        node
    }

    pub fn descendants(&self) -> impl Iterator<Item = NodeRef<'a, T, G>> + 'a {
        self.tree
            .descendants(self.idx)
            .map(|id| NodeRef::from(self.tree, id))
    }

    pub fn traverse(&self) -> impl Iterator<Item = NodeCursor<'a, T, G>> + 'a {
        self.tree.traverse(self.idx).map(|edge| match edge {
            NodeEdge::Start(id) => NodeCursor::Start(NodeRef::from(self.tree, id)),
            NodeEdge::End(id) => NodeCursor::End(NodeRef::from(self.tree, id)),
        })
    }

    /// Returns the span of text from first to last token, if any.
    pub fn span(&self) -> Option<Span> {
        if let (Some(start), Some(end)) = (self.start(), self.end()) {
            let s = start.span();
            let e = end.span();
            Some(Span::new(s.locator().clone(), s.start()..e.end()))
        } else {
            None
        }
    }

    /// Returns the index of the first token, if any.
    pub fn start(&self) -> Option<TokenRef<'a, G>> {
        match self.syntax().trunk() {
            SyntaxTrunk::Leaf(t) => Some(TokenRef::from(&self.tree.tokens, t.index())),
            _ => self.children().find_map(|c| c.start()),
        }
    }

    /// Returns the index of the last token, if any.
    pub fn end(&self) -> Option<TokenRef<'a, G>> {
        match self.syntax().trunk() {
            SyntaxTrunk::Leaf(t) => Some(TokenRef::from(&self.tree.tokens, t.index())),
            _ => self.reverse_children().find_map(|c| c.end()),
        }
    }

    pub fn detach(&self) -> SyntaxTree<T, G> {
        self.tree.detach(self.idx)
    }

    pub fn as_str(&self) -> &'a str {
        self.token().value().as_str(self.tree)
    }
}

impl<'a, T: Core, G: Grammar> Debug for NodeRef<'a, T, G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} ({:?})",
            self.syntax().trunk(),
            self.syntax().core_ref()
        )?;
        if !self.is_empty() {
            write!(f, " -> ")?;
            f.debug_list().entries(self.children()).finish()?;
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! syntax_nodes {
    ( $grammar:ident, $( $node:ident ),+ ) => {
        #[allow(dead_code)]
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        pub enum SyntaxKind {
            $( $node ),+
        }

        $(
            #[allow(dead_code)]
            #[derive(Clone, Copy, Debug)]
            pub struct $node<'a, T: Core>(NodeRef<'a, T, $grammar>);

            #[allow(dead_code)]
            impl<'a, T: Core> $node<'a, T>
            {
                pub fn cast(node: NodeRef<'a, T, $grammar>) -> Option<Self> {
                    match node.syntax().trunk() {
                        SyntaxTrunk::Tree(SyntaxKind::$node) => Some($node(node)),
                        _ => None,
                    }
                }

                pub fn node(&self) -> NodeRef<'a, T, $grammar> {
                   self.0
                }
            }
        )+
    }
}

#[macro_export]
macro_rules! terminal_node {
    ( $grammar:ident, $node:ident, $pat:pat  ) => {
        #[allow(dead_code)]
        #[derive(Debug)]
        pub struct $node<'a, T: Core>(NodeRef<'a, T, $grammar>);

        #[allow(dead_code)]
        impl<'a, T: Core> $node<'a, T> {
            pub fn cast(node: NodeRef<'a, T, $grammar>) -> Option<Self> {
                match node.syntax().trunk() {
                    SyntaxTrunk::Leaf(t) if matches!(t.kind(), $pat) => Some(Self(node)),
                    _ => None,
                }
            }

            pub fn node(&self) -> NodeRef<'a, T, $grammar> {
                self.0
            }
        }
    };
}

pub fn tree_one<'a, P, G>(
    p: P,
    k: G::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = P::Error> + Clone + 'a
where
    P: Parser<TokenAlias<G::Lex>, ParseNode<G>> + Clone + 'a,
    G: Grammar + 'a,
{
    // The returned parser is boxed otherwise the Rust compiler
    // cannot deal with the depth of the generated types.
    p.map(move |n| ParseNode::from_tree(k, vec![n])).boxed()
}

pub fn tree_maybe<'a, P, G>(
    p: P,
    k: G::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = P::Error> + Clone + 'a
where
    P: Parser<TokenAlias<G::Lex>, Option<ParseNode<G>>> + Clone + 'a,
    G: Grammar + 'a,
{
    // The returned parser is boxed otherwise the Rust compiler
    // cannot deal with the depth of the generated types.
    p.map(move |n| ParseNode::from_tree(k, if let Some(n) = n { vec![n] } else { vec![] }))
        .boxed()
}

pub fn tree_many<'a, P, G>(
    p: P,
    k: G::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = P::Error> + Clone + 'a
where
    P: Parser<TokenAlias<G::Lex>, Vec<ParseNode<G>>> + Clone + 'a,
    G: Grammar + 'a,
{
    // The returned parser is boxed otherwise the Rust compiler
    // cannot deal with the depth of the generated types.
    p.map(move |v| ParseNode::from_tree(k, v)).boxed()
}

pub fn tree_skip<'a, P, G>(
    p: P,
    k: G::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = P::Error> + Clone + 'a
where
    P: Parser<TokenAlias<G::Lex>, Vec<ParseNode<G>>> + Clone + 'a,
    G: Grammar + 'a,
{
    // The returned parser is boxed otherwise the Rust compiler
    // cannot deal with the depth of the generated types.
    p.map(move |mut v| {
        if v.len() == 1 {
            v.pop().unwrap()
        } else {
            ParseNode::from_tree(k, v)
        }
    })
    .boxed()
}

pub fn just_token<E, G>(
    kind: <<G as Grammar>::Lex as Lexeme>::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = E> + Clone
where
    E: chumsky::Error<TokenAlias<G::Lex>>,
    G: Grammar,
{
    filter::<TokenAlias<G::Lex>, _, _>(move |t| t.kind() == kind).map(ParseNode::from_leaf)
}

pub fn but_token<E, G>(
    kind: <<G as Grammar>::Lex as Lexeme>::Kind,
) -> impl Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = E> + Clone
where
    E: chumsky::Error<TokenAlias<G::Lex>>,
    G: Grammar,
{
    filter::<TokenAlias<G::Lex>, _, _>(move |t| t.kind() != kind).map(ParseNode::from_leaf)
}

#[macro_export]
macro_rules! match_token {
    ($($p:pat $(if $guard:expr)?),+ $(,)?) => ({
        chumsky::primitive::filter_map(move |span, x: TokenAlias<_>| match x.kind() {
            $($p $(if $guard)? => ::core::result::Result::Ok(ParseNode::from_leaf(x))),+,
            _ => ::core::result::Result::Err(
                chumsky::error::Error::expected_input_found(
                    span, ::core::option::Option::None, ::core::option::Option::Some(x)
                )
            ),
        })
    });
}

/// The syntax analyzer error type.
pub type ParserError<L> = Simple<TokenAlias<L>, Span>;

/// Perform syntax analysis over a list of tokens, yielding a concrete syntax tree.
#[allow(clippy::type_complexity)]
pub fn analyze<G, P, T>(
    tokens: TokenList<G::Lex>,
    parser: P,
) -> (Option<SyntaxTree<T, G>>, Vec<ParserError<G::Lex>>)
where
    G: Grammar,
    P: Parser<TokenAlias<G::Lex>, ParseNode<G>, Error = ParserError<<G as Grammar>::Lex>>,
    T: Core,
{
    let (root, errs) = parser.parse_recovery(tokens.stream());
    let tree = root.map(|r| SyntaxTree::import(tokens, r));
    (tree, errs)
}
