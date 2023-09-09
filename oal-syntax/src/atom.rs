use crate::errors::{Error, Result};
use enum_map::Enum;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU16;
use std::rc::Rc;

/// Text syntax token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Text(Rc<str>);

impl From<&str> for Text {
    fn from(s: &str) -> Self {
        Text(s.into())
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<Text> for String {
    fn from(text: Text) -> Self {
        text.as_ref().to_owned()
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0.as_ref(), f)
    }
}

impl PartialEq<&str> for Text {
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl PartialEq<Text> for &str {
    fn eq(&self, other: &Text) -> bool {
        *self == other.as_ref()
    }
}

/// Identifier syntax token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(Rc<str>);

impl Ident {
    pub fn is_reference(&self) -> bool {
        self.0.as_ref().starts_with('@')
    }
    pub fn is_value(&self) -> bool {
        !self.is_reference()
    }
    pub fn untagged(&self) -> String {
        if self.is_reference() {
            self.0.strip_prefix('@').unwrap().to_owned()
        } else {
            self.0.as_ref().to_owned()
        }
    }
}

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        Ident(s.into())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s.into())
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        ident.as_ref().to_owned()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0.as_ref(), f)
    }
}

impl PartialEq<&str> for Ident {
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        *self == other.as_ref()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum HttpStatusRange {
    Info,
    Success,
    Redirect,
    ClientError,
    ServerError,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum HttpStatus {
    Code(NonZeroU16),
    Range(HttpStatusRange),
}

impl TryFrom<u64> for HttpStatus {
    type Error = Error;

    fn try_from(v: u64) -> Result<Self> {
        if (100..=599).contains(&v) {
            Ok(HttpStatus::Code(unsafe {
                NonZeroU16::new_unchecked(v.try_into().unwrap())
            }))
        } else {
            Err(Error::Domain)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Primitive {
    Number,
    String,
    Boolean,
    Integer,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Enum)]
pub enum Method {
    Get,
    Put,
    Post,
    Patch,
    Delete,
    Options,
    Head,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Method::Get => write!(f, "GET"),
            Method::Put => write!(f, "PUT"),
            Method::Post => write!(f, "POST"),
            Method::Patch => write!(f, "PATCH"),
            Method::Delete => write!(f, "DELETE"),
            Method::Options => write!(f, "OPTIONS"),
            Method::Head => write!(f, "HEAD"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariadicOperator {
    Join,
    Any,
    Sum,
    Range,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Optional,
    Required,
}
