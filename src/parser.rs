use anyhow::Result;
use pest::Parser;
use pest_derive::Parser;
use std::str::FromStr;

use crate::ast::{
    CallDef, EnumDef, Field, PrimitiveType, ServiceDef, ServiceItem, TopLevelItem, TypeDef,
    TypeExpr, UrlParts,
};

macro_rules! span_error {
    ($pair:expr, $($arg:tt)*) => {
        {
            let span = $pair.as_span();
            let line_col = span.start_pos().line_col();
            let line_num = line_col.0;
            let col_num = line_col.1;

            anyhow::bail!(
                "Line {}, Column {}: {}\n  {:?}",
                line_num,
                col_num,
                format!($($arg)*),
                span.as_str(),
            )
        }
    };
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ScarletParser;

pub fn parse_scarlet_file(input: &str) -> Result<Vec<TopLevelItem>> {
    let pairs = ScarletParser::parse(Rule::file, input)?;

    let mut items = Vec::new();

    for pair in pairs {
        for item in pair.into_inner() {
            match item.as_rule() {
                Rule::enum_def => {
                    items.push(TopLevelItem::Enum(parse_enum(item)?));
                }
                Rule::type_def => {
                    items.push(TopLevelItem::Type(parse_type(item)?));
                }
                Rule::service_def => {
                    items.push(TopLevelItem::Service(parse_service(item)?));
                }
                Rule::EOI => {} // End of input
                _ => {}
            }
        }
    }

    Ok(items)
}

fn parse_enum(pair: pest::iterators::Pair<Rule>) -> Result<EnumDef> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let enum_values = inner.next().unwrap();
    let values = enum_values
        .into_inner()
        .map(|p| p.as_str().trim_matches('"').to_string())
        .collect();

    Ok(EnumDef { name, values })
}

fn parse_type(pair: pest::iterators::Pair<Rule>) -> Result<TypeDef> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let type_expr = parse_type_expr(inner.next().unwrap())?;

    Ok(TypeDef { name, type_expr })
}

fn parse_type_expr_inner(pair: pest::iterators::Pair<Rule>) -> Result<TypeExpr> {
    match pair.as_rule() {
        Rule::array_type => {
            let inner = parse_type_expr_inner(pair.into_inner().next().unwrap())?;
            Ok(TypeExpr::Primitive(PrimitiveType::Array(Box::new(inner))))
        }
        Rule::object_type => {
            let mut fields = Vec::new();
            for inner in pair.into_inner() {
                if let Rule::field_list = inner.as_rule() {
                    for field_pair in inner.into_inner() {
                        let mut field_inner = field_pair.into_inner();
                        let field_name = field_inner.next().unwrap().as_str().to_string();
                        let field_type = parse_type_expr(field_inner.next().unwrap())?;
                        fields.push(Field {
                            name: field_name,
                            type_expr: field_type,
                        });
                    }
                }
            }
            Ok(TypeExpr::Object { fields })
        }
        Rule::primitive_type => {
            let primitive = PrimitiveType::from_str(pair.as_str())?;
            Ok(TypeExpr::Primitive(primitive))
        }
        Rule::identifier => Ok(TypeExpr::Reference(pair.as_str().to_string())),
        Rule::type_reference => Ok(TypeExpr::Reference(parse_type_reference(pair)?)),
        r => {
            span_error!(
                pair,
                "Expected an object, primitive, or identifier but got: {:?}",
                r
            )
        }
    }
}

fn parse_type_expr(pair: pest::iterators::Pair<Rule>) -> Result<TypeExpr> {
    match pair.as_rule() {
        Rule::type_expr => {
            let inner = pair.into_inner().next().unwrap();
            parse_type_expr_inner(inner)
        }
        r => {
            span_error!(pair, "Expected a type expression but got {:?}", r)
        }
    }
}

fn parse_service(pair: pest::iterators::Pair<Rule>) -> Result<ServiceDef> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let mut items = Vec::new();

    for item in inner {
        match item.as_rule() {
            Rule::service_body => {
                let body_item = item.into_inner().next().unwrap();
                match body_item.as_rule() {
                    Rule::type_def => {
                        items.push(ServiceItem::Type(parse_type(body_item)?));
                    }
                    Rule::call_def => {
                        items.push(ServiceItem::Call(parse_call(body_item)?));
                    }
                    Rule::enum_def => {
                        items.push(ServiceItem::Enum(parse_enum(body_item)?));
                    }
                    _ => {
                        span_error!(
                            body_item,
                            "Unexpected body for service: {:?}",
                            body_item.as_rule()
                        )
                    }
                }
            }
            Rule::type_def => {
                items.push(ServiceItem::Type(parse_type(item)?));
            }
            Rule::call_def => {
                items.push(ServiceItem::Call(parse_call(item)?));
            }
            r => {
                span_error!(
                    item,
                    "Expected a service body, type definition, or call definition but got: {:?}",
                    r
                );
            }
        }
    }

    Ok(ServiceDef { name, items })
}

fn parse_type_reference(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    let mut parts = Vec::new();
    for identifier in pair.into_inner() {
        parts.push(identifier.as_str());
    }
    Ok(parts.join("."))
}

fn parse_call(pair: pest::iterators::Pair<Rule>) -> Result<CallDef> {
    let err_cloned = pair.clone();
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let mut method = None;
    let mut url = None;
    let mut request = None;
    let mut response = None;

    for field in inner {
        match field.as_rule() {
            Rule::call_field => {
                let mut field_inner = field.into_inner();
                let field_type = field_inner.next().unwrap();
                match field_type.as_rule() {
                    Rule::method_field => {
                        let method_value = field_type.into_inner().next().unwrap();
                        method = Some(method_value.as_str().parse()?);
                    }
                    Rule::url_field => {
                        let url_path = field_type.into_inner().next().unwrap();
                        let mut url_parts = Vec::new();

                        for segment in url_path.into_inner() {
                            match segment.as_rule() {
                                Rule::url_segment => {
                                    let inner_segment = segment.into_inner().next().unwrap();
                                    match inner_segment.as_rule() {
                                        Rule::static_segment => {
                                            url_parts.push(UrlParts::Static(
                                                inner_segment.as_str().to_string(),
                                            ));
                                        }
                                        Rule::path_param => {
                                            let mut param_inner = inner_segment.into_inner();
                                            let param_name =
                                                param_inner.next().unwrap().as_str().to_string();
                                            let param_type =
                                                param_inner.next().unwrap().as_str().to_string();

                                            // Store the parameter info
                                            url_parts.push(UrlParts::Param(
                                                param_name,
                                                PrimitiveType::from_str(&param_type)?,
                                            ));
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }
                        url = Some(url_parts);
                    }
                    Rule::request_field => {
                        let type_ref = field_type.into_inner().next().unwrap(); // get the type_reference
                        request = Some(parse_type_expr(type_ref)?);
                    }
                    Rule::response_field => {
                        let type_ref = field_type.into_inner().next().unwrap(); // get the type_reference
                        response = Some(parse_type_expr(type_ref)?);
                    }
                    r => {
                        span_error!(
                            field_type,
                            "Unexpected a method, url, request, or response but got: {:?}",
                            r
                        )
                    }
                }
            }
            f => {
                span_error!(field, "Expected a call field but got: {:?}", f);
            }
        }
    }

    if method.is_none() || url.is_none() {
        span_error!(err_cloned, "method and url are required fields");
    }

    Ok(CallDef {
        name,
        method: method.unwrap(),
        url: url.unwrap(),
        request,
        response,
    })
}
