use anyhow::Result;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Enum(EnumDef),
    Type(TypeDef),
    Service(ServiceDef),
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub values: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    String,
    Number,
    Boolean,
    Null,
    Array(Box<TypeExpr>),
}

impl FromStr for PrimitiveType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "string" => Ok(PrimitiveType::String),
            "number" => Ok(PrimitiveType::Number),
            "boolean" => Ok(PrimitiveType::Boolean),
            "null" => Ok(PrimitiveType::Null),
            _ => anyhow::bail!("Unknown primitive type: {}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Object { fields: Vec<Field> },
    Primitive(PrimitiveType),
    Reference(String),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, Clone)]
pub struct ServiceDef {
    pub name: String,
    pub items: Vec<ServiceItem>,
}

#[derive(Debug, Clone)]
pub enum ServiceItem {
    Type(TypeDef),
    Call(CallDef),
}

#[derive(Debug, Clone)]
pub enum HttpMethod {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
}

impl FromStr for HttpMethod {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "GET" => Ok(HttpMethod::GET),
            "POST" => Ok(HttpMethod::POST),
            "PUT" => Ok(HttpMethod::PUT),
            "DELETE" => Ok(HttpMethod::DELETE),
            "PATCH" => Ok(HttpMethod::PATCH),
            "HEAD" => Ok(HttpMethod::HEAD),
            "OPTIONS" => Ok(HttpMethod::OPTIONS),
            _ => anyhow::bail!("Unknown HTTP method: {}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallDef {
    pub name: String,
    pub method: Option<HttpMethod>,
    pub url: Option<String>,
    pub request: Option<TypeExpr>,
    pub response: Option<TypeExpr>,
}
