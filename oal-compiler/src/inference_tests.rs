use crate::errors::Error;
use crate::expr::TypedExpr;
use crate::inference::{constrain, substitute, tag_type, TagSeq, TypeConstraint};
use crate::scan::Scan;
use crate::scope::Env;
use crate::tag::{Tag, Tagged};
use crate::transform::Transform;
use crate::Program;
use oal_syntax::ast::{Expr, Lambda, Statement};
use oal_syntax::parse;

#[test]
fn tag_var_decl() {
    let mut d: Program = parse("let id1 = num;".into()).expect("parsing failed");

    assert_eq!(d.stmts.len(), 1);

    d.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    if let Statement::Decl(decl) = d.stmts.first().unwrap() {
        assert_eq!(decl.expr.unwrap_tag(), Tag::Primitive);
    } else {
        panic!("expected declaration");
    }
}

#[test]
fn tag_array_decl() {
    let mut d: Program = parse("let id1 = [num];".into()).expect("parsing failed");

    assert_eq!(d.stmts.len(), 1);

    d.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    if let Statement::Decl(decl) = d.stmts.first().unwrap() {
        assert_eq!(decl.expr.unwrap_tag(), Tag::Array);
    } else {
        panic!("expected declaration");
    }
}

#[test]
fn tag_lambda_decl() {
    let mut d: Program = parse("let f x y z = num;".into()).expect("parsing failed");

    d.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    assert_eq!(d.stmts.len(), 1);

    let s = d.stmts.first().unwrap();

    if let Statement::Decl(decl) = s {
        assert_eq!(decl.name.as_ref(), "f");
        assert_eq!(decl.expr.unwrap_tag(), Tag::Var(3));
        if let Expr::Lambda(Lambda { bindings, .. }) = decl.expr.as_ref() {
            let tags: Vec<_> = bindings
                .iter()
                .filter_map(|a| match a.tag() {
                    Some(Tag::Var(n)) => Some(*n),
                    _ => None,
                })
                .collect();
            assert_eq!(tags, vec![0, 1, 2]);
        } else {
            panic!("expected lambda expression");
        }
    } else {
        panic!("expected declaration");
    }
}

#[test]
fn constraint_var() {
    let code = r#"
        let id1 = {} & {};
        let id2 = id1 | {};
    "#;
    let mut d: Program = parse(code.into()).expect("parsing failed");

    d.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    let cnt = &mut TypeConstraint::new();

    d.scan(cnt, &mut Env::new(), constrain)
        .expect("constraining failed");

    assert_eq!(cnt.len(), 8);
}

#[test]
fn constraint_lambda() {
    let mut d: Program = parse("let f x y z = num;".into()).expect("parsing failed");

    d.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    let cnt = &mut TypeConstraint::new();

    d.scan(cnt, &mut Env::new(), constrain)
        .expect("constraining failed");

    assert_eq!(cnt.len(), 2);
}

#[test]
fn unify_simple() {
    let mut c = TypeConstraint::new();

    c.push(Tag::Var(0), Tag::Primitive);
    c.push(Tag::Var(2), Tag::Var(1));
    c.push(Tag::Var(1), Tag::Var(0));

    let u = c.unify().expect("unification failed");

    let t = u.substitute(&Tag::Var(2));

    assert_eq!(t, Tag::Primitive);
}

fn check_tags(acc: &mut (), env: &mut Env<TypedExpr>, e: &TypedExpr) -> crate::errors::Result<()> {
    e.as_ref().scan(acc, env, check_tags)?;
    match e.tag() {
        None => Err(Error::new("missing tag").with_expr(e.as_ref())),
        Some(Tag::Var(_)) => Err(Error::new("remaining tag variable")
            .with_expr(e.as_ref())
            .with_tag(e.tag())),
        Some(_) => Ok(()),
    }
}

#[test]
fn unify_lambda() {
    let code = r#"
        let f x y z = num;
        let a = f num {} uri;
    "#;
    let mut prg: Program = parse(code.into()).expect("parsing failed");

    prg.transform(&mut TagSeq::new(), &mut Env::new(), tag_type)
        .expect("tagging failed");

    let cnt = &mut TypeConstraint::new();

    prg.scan(cnt, &mut Env::new(), constrain)
        .expect("constraining failed");

    let subst = &mut cnt.unify().expect("unification failed");

    prg.transform(subst, &mut Env::new(), substitute)
        .expect("substitution failed");

    prg.scan(&mut (), &mut Env::new(), check_tags)
        .expect("substitution incomplete");
}
