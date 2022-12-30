use enum_map::EnumMap;
use indexmap::IndexMap;
use oal_syntax::atom;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq)]
pub enum UriSegment {
    Literal(atom::Text),
    Variable(Box<Property>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Uri {
    pub path: Vec<UriSegment>,
    pub params: Option<Object>,
    pub example: Option<String>,
}

impl Uri {
    pub fn pattern(&self) -> String {
        self.pattern_with(|p| format!("{{{}}}", p.name.untagged()))
    }

    pub fn pattern_with<F>(&self, f: F) -> String
    where
        F: Fn(&Property) -> String,
    {
        const SEGMENT_LENGTH_HINT: usize = 10;

        let mut b = String::with_capacity(self.path.len() * SEGMENT_LENGTH_HINT);
        for s in self.path.iter() {
            b.push('/');
            match s {
                UriSegment::Literal(l) => b.push_str(l),
                UriSegment::Variable(t) => b.push_str(f(t).as_str()),
            }
        }
        b
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub item: Schema,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariadicOp {
    pub op: atom::Operator,
    pub schemas: Vec<Schema>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Schema {
    pub expr: SchemaExpr,
    pub desc: Option<String>,
    pub title: Option<String>,
    pub required: Option<bool>,
    pub examples: Option<HashMap<String, String>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrimNumber {
    pub minimum: Option<f64>,
    pub maximum: Option<f64>,
    pub multiple_of: Option<f64>,
    pub example: Option<f64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimString {
    pub pattern: Option<String>,
    pub enumeration: Vec<String>,
    pub example: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimBoolean {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct PrimInteger {
    pub minimum: Option<i64>,
    pub maximum: Option<i64>,
    pub multiple_of: Option<i64>,
    pub example: Option<i64>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SchemaExpr {
    Num(PrimNumber),
    Str(PrimString),
    Bool(PrimBoolean),
    Int(PrimInteger),
    Rel(Box<Relation>),
    Uri(Uri),
    Array(Box<Array>),
    Object(Object),
    Op(VariadicOp),
    Ref(atom::Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    pub name: atom::Ident,
    pub schema: Schema,
    /// The property description when used as a parameter
    pub desc: Option<String>,
    /// Whether the property is required when used as a parameter
    pub required: Option<bool>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Object {
    pub props: Vec<Property>,
}

pub type MediaType = String;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Content {
    pub schema: Option<Box<Schema>>,
    pub status: Option<atom::HttpStatus>,
    pub media: Option<MediaType>,
    pub headers: Option<Object>,
    pub desc: Option<String>,
    pub examples: Option<HashMap<String, String>>,
}

impl From<Schema> for Content {
    fn from(s: Schema) -> Self {
        let desc = s.desc.clone();
        let schema = Some(s.into());
        let status = None;
        let media = None;
        let headers = None;
        let examples = Default::default();
        Content {
            schema,
            status,
            media,
            headers,
            desc,
            examples,
        }
    }
}

pub type Ranges = IndexMap<(Option<atom::HttpStatus>, Option<MediaType>), Content>;

#[derive(Clone, Debug, PartialEq)]
pub struct Transfer {
    pub methods: EnumMap<atom::Method, bool>,
    pub domain: Content,
    pub ranges: Ranges,
    pub params: Option<Object>,
    pub desc: Option<String>,
    pub summary: Option<String>,
    pub tags: Vec<String>,
    pub id: Option<String>,
}

pub type Transfers = EnumMap<atom::Method, Option<Transfer>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Relation {
    pub uri: Uri,
    pub xfers: Transfers,
}

impl From<Uri> for Relation {
    fn from(uri: Uri) -> Self {
        Relation {
            uri,
            xfers: Transfers::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Reference {
    Schema(Schema),
}

pub type PathPattern = String;
pub type Relations = IndexMap<PathPattern, Relation>;
pub type References = IndexMap<atom::Ident, Reference>;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Spec {
    pub rels: Relations,
    pub refs: References,
}
