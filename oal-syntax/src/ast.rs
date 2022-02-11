use crate::{Pair, Rule};
use std::rc::Rc;

pub type Literal = Rc<str>;
pub type Ident = Rc<str>;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Tag {
    Primitive,
    Relation,
    Object,
    Uri,
    Any,
    Var(usize),
}

impl Tag {
    pub fn is_variable(&self) -> bool {
        if let Tag::Var(_) = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Prim(Prim),
    Rel(Rel),
    Uri(Uri),
    Block(Block),
    Var(Ident),
    Op(VariadicOp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub tag: Option<Tag>,
    pub expr: Expr,
}

impl From<Expr> for TypedExpr {
    fn from(e: Expr) -> Self {
        TypedExpr { tag: None, expr: e }
    }
}

impl From<Pair<'_>> for TypedExpr {
    fn from(p: Pair<'_>) -> Self {
        match p.as_rule() {
            Rule::prim_type => Expr::Prim(p.into()).into(),
            Rule::rel_type => Expr::Rel(p.into()).into(),
            Rule::uri_type => Expr::Uri(p.into()).into(),
            Rule::block_type => Expr::Block(p.into()).into(),
            Rule::paren_type => p.into_inner().next().unwrap().into(),
            Rule::ident => Expr::Var(p.as_str().into()).into(),
            Rule::join_type | Rule::any_type | Rule::sum_type => {
                let op = VariadicOp::from(p);
                if op.exprs.len() == 1 {
                    op.exprs.first().unwrap().clone()
                } else {
                    Expr::Op(op).into()
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Doc {
    pub stmts: Vec<Stmt>,
}

impl From<Pair<'_>> for Doc {
    fn from(p: Pair) -> Self {
        let stmts = p
            .into_inner()
            .flat_map(|p| match p.as_rule() {
                Rule::stmt => Some(p.into()),
                _ => None,
            })
            .collect();
        Doc { stmts }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Decl {
    pub var: Ident,
    pub body: TypedExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Res {
    pub rel: TypedExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Res(Res),
}

impl From<Pair<'_>> for Stmt {
    fn from(p: Pair) -> Self {
        let p = p.into_inner().next().unwrap();
        match p.as_rule() {
            Rule::decl => {
                let mut p = p.into_inner();
                let var = p.nth(1).unwrap().as_str().into();
                let next_pair = p.next().unwrap();
                let expr = if next_pair.as_rule() == Rule::type_kw {
                    // TODO: parse the type annotation into a type tag
                    let _ann = next_pair;
                    p.next().unwrap()
                } else {
                    next_pair
                };
                let expr = expr.into();
                Stmt::Decl(Decl { var, body: expr })
            }
            Rule::res => Stmt::Res(Res {
                rel: p.into_inner().nth(1).unwrap().into(),
            }),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Method {
    Get,
    Put,
}

impl From<Pair<'_>> for Method {
    fn from(p: Pair) -> Self {
        match p.into_inner().next().unwrap().as_rule() {
            Rule::get_kw => Method::Get,
            Rule::put_kw => Method::Put,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rel {
    pub uri: Box<TypedExpr>,
    pub methods: Vec<Method>,
    pub range: Box<TypedExpr>,
}

impl From<Pair<'_>> for Rel {
    fn from(p: Pair) -> Self {
        let mut inner = p.into_inner();

        let uri: Box<_> = TypedExpr::from(inner.next().unwrap()).into();

        let methods: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|p| p.into())
            .collect();

        let range: Box<_> = TypedExpr::from(inner.next().unwrap()).into();

        Rel {
            uri,
            methods,
            range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UriSegment {
    Literal(Literal),
    Template(Prop),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Uri {
    pub spec: Vec<UriSegment>,
}

impl Uri {
    pub fn is_empty(&self) -> bool {
        self.spec.is_empty()
    }
}

impl<'a> IntoIterator for &'a Uri {
    type Item = &'a UriSegment;
    type IntoIter = core::slice::Iter<'a, UriSegment>;

    fn into_iter(self) -> Self::IntoIter {
        self.spec.iter()
    }
}

impl From<Pair<'_>> for Uri {
    fn from(p: Pair) -> Self {
        let p = p.into_inner().next().unwrap();
        let spec: Vec<_> = match p.as_rule() {
            Rule::uri_spec => p
                .into_inner()
                .map(|p| match p.as_rule() {
                    Rule::uri_tpl => UriSegment::Template(p.into_inner().next().unwrap().into()),
                    Rule::uri_lit => {
                        UriSegment::Literal(p.into_inner().next().unwrap().as_str().into())
                    }
                    _ => unreachable!(),
                })
                .collect(),
            _ => Default::default(),
        };
        Uri { spec }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Prop {
    pub key: Ident,
    pub val: TypedExpr,
}

impl From<Pair<'_>> for Prop {
    fn from(p: Pair) -> Self {
        let mut inner = p.into_inner();
        let key = inner.next().unwrap().as_str().into();
        let val = inner.next().unwrap().into();
        Prop { key, val }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub props: Vec<Prop>,
}

impl From<Pair<'_>> for Block {
    fn from(p: Pair) -> Self {
        let props = p.into_inner().map(|p| p.into()).collect();
        Block { props }
    }
}

impl Block {
    pub fn iter(&self) -> std::slice::Iter<Prop> {
        self.props.iter()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    Join,
    Any,
    Sum,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariadicOp {
    pub op: Operator,
    pub exprs: Vec<TypedExpr>,
}

impl From<Pair<'_>> for VariadicOp {
    fn from(p: Pair) -> Self {
        let op = match p.as_rule() {
            Rule::join_type => Operator::Join,
            Rule::any_type => Operator::Any,
            Rule::sum_type => Operator::Sum,
            _ => unreachable!(),
        };
        let exprs = p
            .into_inner()
            .map(|p| p.into_inner().next().unwrap().into())
            .collect();
        VariadicOp { op, exprs }
    }
}

impl VariadicOp {
    pub fn iter(&self) -> std::slice::Iter<TypedExpr> {
        self.exprs.iter()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Prim {
    Num,
    Str,
    Bool,
}

impl From<Pair<'_>> for Prim {
    fn from(p: Pair) -> Self {
        match p.into_inner().next().unwrap().as_rule() {
            Rule::num_kw => Prim::Num,
            Rule::str_kw => Prim::Str,
            Rule::bool_kw => Prim::Bool,
            _ => unreachable!(),
        }
    }
}
