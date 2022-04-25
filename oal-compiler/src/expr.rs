use crate::tag::{Tag, Tagged};
use oal_syntax::ast::Expr;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    tag: Option<Tag>,
    inner: Expr<TypedExpr>,
}

impl Tagged for TypedExpr {
    fn tag(&self) -> Option<&Tag> {
        self.tag.as_ref()
    }

    fn set_tag(&mut self, t: Tag) {
        self.tag = Some(t)
    }

    fn unwrap_tag(&self) -> Tag {
        self.tag.as_ref().unwrap().clone()
    }

    fn with_tag(mut self, t: Tag) -> Self {
        self.set_tag(t);
        self
    }
}

impl From<Expr<TypedExpr>> for TypedExpr {
    fn from(e: Expr<TypedExpr>) -> Self {
        TypedExpr {
            tag: None,
            inner: e,
        }
    }
}

impl AsRef<Expr<TypedExpr>> for TypedExpr {
    fn as_ref(&self) -> &Expr<TypedExpr> {
        &self.inner
    }
}

impl AsMut<Expr<TypedExpr>> for TypedExpr {
    fn as_mut(&mut self) -> &mut Expr<TypedExpr> {
        &mut self.inner
    }
}
