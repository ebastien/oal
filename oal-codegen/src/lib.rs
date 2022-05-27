use indexmap::{indexmap, IndexMap};
use oal_compiler::spec;
use oal_syntax::ast;
use openapiv3::{
    ArrayType, Info, MediaType, ObjectType, OpenAPI, Operation, Parameter, ParameterData,
    ParameterSchemaOrContent, PathItem, Paths, ReferenceOr, RequestBody, Response, Responses,
    Schema, SchemaData, SchemaKind, Server, StatusCode, StringType, Type, VariantOrUnknownOrEmpty,
};

#[derive(Default)]
pub struct Builder {
    spec: Option<spec::Spec>,
    base: Option<OpenAPI>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder::default()
    }

    pub fn with_base(mut self, base: OpenAPI) -> Self {
        self.base = Some(base);
        self
    }

    pub fn with_spec(mut self, spec: spec::Spec) -> Self {
        self.spec = Some(spec);
        self
    }

    pub fn into_openapi(self) -> OpenAPI {
        let paths = self.all_paths();
        let mut definition = if let Some(base) = self.base {
            base
        } else {
            self.default_base()
        };
        definition.paths = paths;
        definition
    }

    fn default_base(&self) -> OpenAPI {
        OpenAPI {
            openapi: "3.0.3".into(),
            info: Info {
                title: "OpenAPI definition".into(),
                version: "0.1.0".into(),
                ..Default::default()
            },
            servers: vec![Server {
                url: "/".to_owned(),
                ..Default::default()
            }],
            ..Default::default()
        }
    }

    fn media_type(&self) -> String {
        "application/json".to_owned()
    }

    fn uri_pattern(&self, uri: &spec::Uri) -> String {
        uri.pattern()
    }

    fn prim_type(&self, prim: &ast::Primitive) -> Type {
        match prim {
            ast::Primitive::Num => Type::Number(Default::default()),
            ast::Primitive::Str => Type::String(Default::default()),
            ast::Primitive::Bool => Type::Boolean {},
        }
    }

    fn prim_schema(&self, prim: &ast::Primitive) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::Type(self.prim_type(prim)),
        }
    }

    fn rel_schema(&self, rel: &spec::Relation) -> Schema {
        self.uri_schema(&rel.uri)
    }

    fn uri_schema(&self, uri: &spec::Uri) -> Schema {
        let pattern = if uri.path.is_empty() {
            None
        } else {
            Some(self.uri_pattern(uri).into())
        };
        Schema {
            schema_data: SchemaData {
                example: pattern,
                ..Default::default()
            },
            schema_kind: SchemaKind::Type(Type::String(StringType {
                format: VariantOrUnknownOrEmpty::Unknown("uri-reference".into()),
                ..Default::default()
            })),
        }
    }

    fn join_schema(&self, schemas: &[spec::Schema]) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::AllOf {
                all_of: schemas
                    .iter()
                    .map(|s| ReferenceOr::Item(self.schema(s)))
                    .collect(),
            },
        }
    }

    fn object_type(&self, obj: &spec::Object) -> Type {
        Type::Object(ObjectType {
            properties: obj
                .props
                .iter()
                .map(|p| {
                    let ident = p.name.as_ref().into();
                    let expr = ReferenceOr::Item(self.schema(&p.schema).into());
                    (ident, expr)
                })
                .collect(),
            ..Default::default()
        })
    }

    fn object_schema(&self, obj: &spec::Object) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::Type(self.object_type(obj)),
        }
    }

    fn array_schema(&self, array: &spec::Array) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::Type(Type::Array(ArrayType {
                items: Some(ReferenceOr::Item(self.schema(&array.item).into())),
                min_items: None,
                max_items: None,
                unique_items: false,
            })),
        }
    }

    fn sum_schema(&self, schemas: &[spec::Schema]) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::OneOf {
                one_of: schemas
                    .iter()
                    .map(|s| ReferenceOr::Item(self.schema(s)))
                    .collect(),
            },
        }
    }

    fn any_schema(&self, schemas: &[spec::Schema]) -> Schema {
        Schema {
            schema_data: Default::default(),
            schema_kind: SchemaKind::AnyOf {
                any_of: schemas
                    .iter()
                    .map(|s| ReferenceOr::Item(self.schema(s)))
                    .collect(),
            },
        }
    }

    fn schema(&self, s: &spec::Schema) -> Schema {
        let mut sch = match &s.expr {
            spec::Expr::Prim(prim) => self.prim_schema(prim),
            spec::Expr::Rel(rel) => self.rel_schema(rel),
            spec::Expr::Uri(uri) => self.uri_schema(uri),
            spec::Expr::Object(obj) => self.object_schema(obj),
            spec::Expr::Array(array) => self.array_schema(array),
            spec::Expr::Op(operation) => match operation.op {
                ast::Operator::Join => self.join_schema(&operation.schemas),
                ast::Operator::Sum => self.sum_schema(&operation.schemas),
                ast::Operator::Any => self.any_schema(&operation.schemas),
                ast::Operator::Range => unreachable!(),
            },
        };
        sch.schema_data.description = s.desc.clone();
        sch.schema_data.title = s.title.clone();
        sch
    }

    fn prop_param_data(&self, prop: &spec::Property) -> ParameterData {
        ParameterData {
            name: prop.name.as_ref().into(),
            description: prop.desc.clone(),
            required: true,
            deprecated: None,
            format: ParameterSchemaOrContent::Schema(ReferenceOr::Item(self.schema(&prop.schema))),
            example: None,
            examples: Default::default(),
            explode: None,
            extensions: Default::default(),
        }
    }

    fn prop_path_param(&self, prop: &spec::Property) -> Parameter {
        Parameter::Path {
            parameter_data: self.prop_param_data(prop),
            style: Default::default(),
        }
    }

    fn prop_query_param(&self, prop: &spec::Property) -> Parameter {
        Parameter::Query {
            parameter_data: self.prop_param_data(prop),
            allow_reserved: false,
            style: Default::default(),
            allow_empty_value: None,
        }
    }

    fn xfer_params(&self, xfer: &spec::Transfer) -> Vec<ReferenceOr<Parameter>> {
        xfer.params
            .iter()
            .flat_map(|o| {
                o.props
                    .iter()
                    .map(|p| ReferenceOr::Item(self.prop_query_param(p)))
            })
            .collect()
    }

    fn uri_params(&self, uri: &spec::Uri) -> Vec<ReferenceOr<Parameter>> {
        let path = uri.path.iter().flat_map(|s| match s {
            spec::UriSegment::Variable(p) => Some(ReferenceOr::Item(self.prop_path_param(p))),
            _ => None,
        });
        let query = uri.params.iter().flat_map(|o| {
            o.props
                .iter()
                .map(|p| ReferenceOr::Item(self.prop_query_param(p)))
        });
        path.chain(query).collect()
    }

    fn domain_request(&self, domain: &spec::Content) -> Option<ReferenceOr<RequestBody>> {
        let media = domain.media.clone().unwrap_or_else(|| self.media_type());
        domain.schema.as_ref().map(|schema| {
            ReferenceOr::Item(RequestBody {
                content: indexmap! { media => MediaType {
                    schema: Some(ReferenceOr::Item(self.schema(schema))),
                    ..Default::default()
                }},
                description: domain.desc.clone(),
                ..Default::default()
            })
        })
    }

    fn transfer_request(&self, xfer: &spec::Transfer) -> Option<ReferenceOr<RequestBody>> {
        self.domain_request(&xfer.domain)
    }

    fn transfer_responses(&self, xfer: &spec::Transfer) -> Responses {
        let mut default = None;
        let mut responses = IndexMap::new();

        for ((status, media), content) in xfer.ranges.iter() {
            let response = if let Some(s) = status {
                responses
                    .entry(StatusCode::Code((*s).into()))
                    .or_insert(ReferenceOr::Item(Response::default()))
            } else {
                default.insert(ReferenceOr::Item(Response::default()))
            };
            if let ReferenceOr::Item(res) = response {
                if let Some(schema) = content.schema.as_ref() {
                    let media_type = media.clone().unwrap_or_else(|| self.media_type());
                    let media_schema = MediaType {
                        schema: Some(ReferenceOr::Item(self.schema(schema))),
                        ..Default::default()
                    };
                    res.content.insert(media_type, media_schema);
                }
                res.description = content.desc.clone().unwrap_or_else(|| "".to_owned());
            } else {
                unreachable!();
            }
        }

        Responses {
            default,
            responses,
            ..Default::default()
        }
    }

    fn relation_path_item(&self, rel: &spec::Relation) -> PathItem {
        let mut path_item = PathItem {
            parameters: self.uri_params(&rel.uri),
            ..Default::default()
        };

        let xfers = rel
            .xfers
            .iter()
            .filter_map(|(m, x)| x.as_ref().map(|x| (m, x)));

        for (method, xfer) in xfers {
            let op = Operation {
                summary: xfer.summary.clone(),
                parameters: self.xfer_params(xfer),
                request_body: self.transfer_request(xfer),
                responses: self.transfer_responses(xfer),
                ..Default::default()
            };

            match method {
                ast::Method::Get => path_item.get = Some(op),
                ast::Method::Put => path_item.put = Some(op),
                ast::Method::Post => path_item.post = Some(op),
                ast::Method::Patch => path_item.patch = Some(op),
                ast::Method::Delete => path_item.delete = Some(op),
                ast::Method::Options => path_item.options = Some(op),
                ast::Method::Head => path_item.head = Some(op),
            }
        }

        path_item
    }

    fn all_paths(&self) -> Paths {
        let paths = if let Some(spec) = &self.spec {
            spec.rels
                .iter()
                .map(|(pattern, rel)| {
                    (
                        pattern.clone(),
                        ReferenceOr::Item(self.relation_path_item(rel)),
                    )
                })
                .collect()
        } else {
            Default::default()
        };
        Paths {
            paths,
            extensions: Default::default(),
        }
    }
}

impl From<Builder> for OpenAPI {
    fn from(b: Builder) -> Self {
        b.into_openapi()
    }
}
