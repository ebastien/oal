use oal_compiler::spec;
use oal_syntax::atom;
use std::collections::{hash_map, HashMap, HashSet};
use std::fmt::{Error, Write};
use std::hash::{Hash, Hasher};

#[derive(Debug)]
struct Uri(spec::Uri);

impl PartialEq for Uri {
    fn eq(&self, other: &Self) -> bool {
        self.0
            .path
            .iter()
            .zip(other.0.path.iter())
            .all(|p| match p {
                (spec::UriSegment::Literal(a), spec::UriSegment::Literal(b)) => a == b,
                (spec::UriSegment::Variable(_), spec::UriSegment::Variable(_)) => true,
                _ => false,
            })
    }
}

impl Eq for Uri {}

#[derive(Hash)]
enum UriSegment {
    Literal(atom::Text),
    Variable,
}

impl From<&spec::UriSegment> for UriSegment {
    fn from(s: &spec::UriSegment) -> Self {
        match s {
            spec::UriSegment::Literal(l) => UriSegment::Literal(l.clone()),
            spec::UriSegment::Variable(_) => UriSegment::Variable,
        }
    }
}

impl Hash for Uri {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for s in self.0.path.iter() {
            UriSegment::from(s).hash(state);
        }
    }
}

impl From<spec::Uri> for Uri {
    fn from(u: spec::Uri) -> Self {
        Uri(u)
    }
}

impl Uri {
    fn inner(&self) -> &spec::Uri {
        &self.0
    }
}

#[test]
fn test_uri_equality() {
    use std::collections::hash_map::DefaultHasher;

    let p1 = spec::Property {
        name: "p1".into(),
        schema: spec::Schema {
            expr: spec::SchemaExpr::Bool(spec::PrimBoolean {}),
            desc: None,
            title: None,
            required: None,
            examples: None,
        },
        desc: None,
        required: None,
    };

    let p2 = spec::Property {
        name: "p2".into(),
        schema: spec::Schema {
            expr: spec::SchemaExpr::Bool(spec::PrimBoolean {}),
            desc: None,
            title: None,
            required: None,
            examples: None,
        },
        desc: None,
        required: None,
    };

    let u1 = Uri::from(spec::Uri {
        path: vec![
            spec::UriSegment::Literal("l1".into()),
            spec::UriSegment::Variable(p1.into()),
            spec::UriSegment::Literal("l2".into()),
        ],
        params: None,
        example: None,
    });

    let u2 = Uri::from(spec::Uri {
        path: vec![
            spec::UriSegment::Literal("l1".into()),
            spec::UriSegment::Variable(p2.into()),
            spec::UriSegment::Literal("l2".into()),
        ],
        params: None,
        example: None,
    });

    let hasher = &mut DefaultHasher::new();
    u1.hash(hasher);
    let h1 = hasher.finish();

    let hasher = &mut DefaultHasher::new();
    u2.hash(hasher);
    let h2 = hasher.finish();

    assert_eq!(u1, u2);
    assert_eq!(h1, h2);
}

type RelationSet = HashSet<(atom::Text, Uri, Option<atom::Method>)>;
type UriSet = HashMap<Uri, HashSet<atom::Method>>;

fn iter_relations(
    spec: &spec::Spec,
    rels: &mut RelationSet,
    name: Option<atom::Text>,
    schema: &spec::Schema,
) {
    match &schema.expr {
        spec::SchemaExpr::Object(obj) => {
            for prop in obj.props.iter() {
                iter_relations(spec, rels, Some(prop.name.clone()), &prop.schema);
            }
        }
        spec::SchemaExpr::Array(arr) => {
            iter_relations(spec, rels, name, &arr.item);
        }
        spec::SchemaExpr::Op(op) => {
            for operand in op.schemas.iter() {
                iter_relations(spec, rels, name.clone(), operand);
            }
        }
        spec::SchemaExpr::Ref(r) => {
            let spec::Reference::Schema(s) = spec.refs.get(r).expect("reference should exist");
            iter_relations(spec, rels, name, s);
        }
        spec::SchemaExpr::Uri(uri) => {
            if let Some(name) = name {
                let uri = Uri::from(uri.clone());
                rels.insert((name, uri, None));
            }
        }
        spec::SchemaExpr::Rel(rel) => {
            if let Some(name) = name {
                for (method, xfer) in rel.xfers.iter() {
                    if xfer.is_some() {
                        let uri = Uri::from(rel.uri.clone());
                        rels.insert((name.clone(), uri, Some(method)));
                    }
                }
            }
        }
        _ => (),
    }
}

fn collect_relations(spec: &spec::Spec) -> RelationSet {
    let mut rels = RelationSet::new();
    for rel in spec.rels.iter() {
        for (_, xfer) in rel.xfers.iter() {
            if let Some(xfer) = xfer {
                if let Some(domain) = xfer.domain.schema.as_ref() {
                    iter_relations(spec, &mut rels, None, domain.as_ref());
                }
                for range in xfer.ranges.values() {
                    if let Some(range) = range.schema.as_ref() {
                        iter_relations(spec, &mut rels, None, range.as_ref());
                    }
                }
            }
        }
    }
    rels
}

fn collect_uris(spec: &spec::Spec) -> UriSet {
    let mut uris = UriSet::new();
    for rel in spec.rels.iter() {
        for (method, xfer) in rel.xfers.iter() {
            if xfer.is_some() {
                match uris.entry(rel.uri.clone().into()) {
                    hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().insert(method);
                    }
                    hash_map::Entry::Vacant(e) => {
                        e.insert(HashSet::from([method]));
                    }
                };
            }
        }
    }
    uris
}

pub struct Builder<'a> {
    spec: &'a spec::Spec,
    rels: RelationSet,
    uris: UriSet,
}

impl<'a> Builder<'a> {
    pub fn new(spec: &'a spec::Spec) -> Self {
        Builder {
            spec,
            rels: collect_relations(spec),
            uris: collect_uris(spec),
        }
    }

    pub fn into_document(self) -> Result<String, Error> {
        let mut w = String::new();
        writeln!(&mut w, "# Media-type reference")?;
        writeln!(&mut w, "## Resources")?;
        writeln!(&mut w, "| URI | Methods |")?;
        writeln!(&mut w, "| --- | ------- |")?;
        for (uri, methods) in self.uris.iter() {
            let mut m = methods.iter().map(|m| format!("{}", m)).collect::<Vec<_>>();
            m.sort();
            writeln!(&mut w, "| {} | {} |", uri.inner().pattern(), m.join(", "))?;
        }
        writeln!(&mut w, "## Relations")?;
        writeln!(&mut w, "| URI | Context | Method | Description |")?;
        writeln!(&mut w, "| --- | ------- | ------ | ----------- |")?;
        for (rel, uri, method) in self.rels.iter() {
            let m = method.map_or("".to_owned(), |m| format!("{}", m));
            writeln!(&mut w, "| {} | {} | {} |  |", uri.inner().pattern(), rel, m)?;
        }
        Ok(w)
    }
}
