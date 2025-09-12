use crate::{
    RequestLanguages, ResponseLanguages,
    ast::{CallDef, EnumDef, ServiceDef, TopLevelItem, TypeDef, TypeExpr},
};
use anyhow::Result;
use heck::{ToLowerCamelCase, ToPascalCase, ToSnakeCase};

pub trait GenRequests {
    fn gen_type(&self, ty: &TypeDef) -> Result<String>;
    fn gen_enum(&self, ty: &EnumDef) -> Result<String>;
    fn gen_service(&self, service: &ServiceDef) -> Result<String>;
    fn gen_call(&self, call: &CallDef) -> Result<String>;
}

pub trait GenResponse {
    fn gen_type(&self, ty: &TypeDef) -> Result<String>;
    fn gen_enum(&self, ty: &EnumDef) -> Result<String>;
    fn gen_service(&self, service: &ServiceDef) -> Result<String>;
}

struct RustGen;

impl RustGen {
    pub fn new() -> Self {
        Self {}
    }

    fn gen_enum_with_indent(&self, ty: &EnumDef, indent_level: usize) -> Result<String> {
        let name = &ty.name;
        let opts = &ty.values;
        let indent = "\t".repeat(indent_level);
        let field_indent = "\t".repeat(indent_level + 1);
        let mut out = String::new();
        out.push_str(&format!("{}pub enum {} {{\n", indent, name));
        for opt in opts {
            out.push_str(&format!("{}{},\n", field_indent, opt.to_pascal_case()));
        }
        out.push_str(&format!("{}}}\n", indent));
        Ok(out)
    }

    fn gen_type_with_indent(&self, ty: &TypeDef, indent_level: usize) -> Result<String> {
        let name = &ty.name;
        let indent = "\t".repeat(indent_level);
        let mut out = String::new();
        match &ty.type_expr {
            TypeExpr::Object { .. } => {
                out.push_str(&format!("{}pub struct {} {{\n", indent, name));
                let res = self.gen_type_expr(&ty.type_expr, indent_level + 1)?;
                out.push_str(&res);
                out.push_str(&format!("{}}}\n", indent));
            }
            _ => {
                let res = self.gen_type_expr(&ty.type_expr, indent_level)?;
                out.push_str(&res);
            }
        };
        Ok(out)
    }

    pub fn gen_type_expr(&self, ty_expr: &TypeExpr, indent_level: usize) -> Result<String> {
        let indent = "\t".repeat(indent_level);
        match ty_expr {
            TypeExpr::Object { fields } => {
                let mut out = String::new();
                for field in fields {
                    let field_name = &field.name;
                    let field_type = &field.type_expr;
                    let gen_ty = self.gen_type_expr(&field_type, indent_level)?;
                    out.push_str(&format!("{indent}{field_name}: {gen_ty},\n"));
                }
                Ok(out)
            }
            TypeExpr::Primitive(primitive_type) => match primitive_type {
                crate::ast::PrimitiveType::String => Ok("String".to_string()),
                crate::ast::PrimitiveType::Number => Ok("i128".to_string()),
                crate::ast::PrimitiveType::Boolean => Ok("bool".to_string()),
                crate::ast::PrimitiveType::Array(type_expr) => {
                    let inner = self.gen_type_expr(&type_expr, indent_level)?;
                    Ok(format!("Vec<{inner}>"))
                }
            },
            TypeExpr::Reference(ty_ref) => {
                let path: Vec<&str> = ty_ref.split('.').collect();
                let last = path.last().ok_or(anyhow::anyhow!(
                    "Expected at least one slot in type reference"
                ))?;
                Ok(last.to_string())
            }
        }
    }
}

impl GenResponse for RustGen {
    fn gen_type(&self, ty: &TypeDef) -> Result<String> {
        self.gen_type_with_indent(ty, 0)
    }

    fn gen_enum(&self, ty: &EnumDef) -> Result<String> {
        self.gen_enum_with_indent(ty, 0)
    }

    fn gen_service(&self, service: &ServiceDef) -> Result<String> {
        // just gen the types flatly
        let mut out = String::new();
        let mut ty_count = 0;
        out.push_str(&format!("pub mod {} {{\n", &service.name.to_snake_case()));
        for item in service.items.iter() {
            match item {
                crate::ast::ServiceItem::Type(type_def) => {
                    let res = self.gen_type_with_indent(type_def, 1)?;
                    out.push_str(&res);
                    ty_count += 1;
                }
                crate::ast::ServiceItem::Call(_) => {}
                crate::ast::ServiceItem::Enum(enum_def) => {
                    let res = self.gen_enum_with_indent(enum_def, 1)?;
                    out.push_str(&res);
                    ty_count += 1;
                }
            }
        }
        out.push_str("}\n");
        if ty_count > 0 {
            Ok(out)
        } else {
            Ok(String::new())
        }
    }
}

pub struct TypescriptGen;

impl TypescriptGen {
    pub fn new() -> Self {
        Self {}
    }

    pub fn gen_type_expr(&self, ty_expr: &TypeExpr) -> Result<String> {
        match ty_expr {
            TypeExpr::Object { fields } => {
                let mut out = String::new();
                for field in fields {
                    let field_name = &field.name;
                    let field_type = &field.type_expr;
                    let gen_ty = self.gen_type_expr(&field_type)?;
                    out.push_str(&format!("\t{field_name}: {gen_ty},\n"));
                }
                Ok(out)
            }
            TypeExpr::Primitive(primitive_type) => match primitive_type {
                crate::ast::PrimitiveType::String => Ok("string".to_string()),
                crate::ast::PrimitiveType::Number => Ok("number".to_string()),
                crate::ast::PrimitiveType::Boolean => Ok("boolean".to_string()),
                crate::ast::PrimitiveType::Array(type_expr) => {
                    let inner = self.gen_type_expr(&type_expr)?;
                    Ok(format!("{inner}[]"))
                }
            },
            TypeExpr::Reference(ty_ref) => {
                let path: Vec<&str> = ty_ref.split('.').collect();
                let last = path.last().ok_or(anyhow::anyhow!(
                    "Expected at least one slot in type reference"
                ))?;
                Ok(last.to_string())
            }
        }
    }

    pub fn base_gen_type(&self, ty: &TypeDef) -> Result<String> {
        let name = &ty.name;
        let mut out = String::new();
        match &ty.type_expr {
            TypeExpr::Object { .. } => {
                // new decl
                out.push_str(&format!("export type {} = {{\n", name));
                let res = self.gen_type_expr(&ty.type_expr)?;
                out.push_str(&res);
                out.push_str("}\n");
            }
            _ => {
                let res = self.gen_type_expr(&ty.type_expr)?;
                out.push_str(&res);
            }
        };
        Ok(out)
    }

    pub fn base_gen_enum(&self, ty: &EnumDef) -> Result<String> {
        let name = &ty.name;
        let opts = &ty.values;
        let mut out = String::new();
        out.push_str(&format!("export type {} = ", name));
        for (i, opt) in opts.iter().enumerate() {
            if i > 0 {
                out.push_str(" | ");
            }
            out.push_str(&format!("\"{opt}\""));
        }
        out.push_str(";\n");
        Ok(out)
    }

    fn generate_fetch_body(
        &self,
        method: &str,
        url: &str,
        has_request: bool,
        has_response: bool,
    ) -> String {
        let mut lines = Vec::new();

        lines.push(format!("\t\tconst response = await fetch('{}', {{", url));
        lines.push(format!("\t\t\tmethod: '{}',", method));
        lines.push("\t\t\theaders: { 'Content-Type': 'application/json' },".to_string());

        if has_request {
            lines.push("\t\t\tbody: JSON.stringify(body)".to_string());
        }

        lines.push("\t\t});".to_string());
        lines.push("\t\tif (response.status !== 200) {".to_string());
        lines.push("\t\t\tthrow new Error('FAIL')".to_string());
        lines.push("\t\t} else {".to_string());

        if has_response {
            lines.push("\t\t\tlet res = await response.json();".to_string());
            lines.push("\t\t\treturn res;".to_string());
        } else {
            lines.push("\t\t\treturn;".to_string());
        }

        lines.push("\t\t}".to_string());

        lines.join("\n")
    }
}

impl GenRequests for TypescriptGen {
    fn gen_type(&self, ty: &TypeDef) -> Result<String> {
        self.base_gen_type(ty)
    }

    fn gen_call(&self, call_def: &CallDef) -> Result<String> {
        let method = &call_def.method;
        let url = &call_def.url;

        let mut method_out = String::new();
        method_out.push_str(&format!("\t{}(", &call_def.name.to_lower_camel_case()));

        let has_request = call_def.request.is_some();
        let has_response = call_def.response.is_some();

        // Add parameter if there's a request body
        if let Some(request_type) = &call_def.request {
            let arg_ty = self.gen_type_expr(request_type)?;
            method_out.push_str(&format!("body: {}", arg_ty));
        }
        method_out.push(')');

        // Add return type
        if let Some(response_type) = &call_def.response {
            let return_ty = self.gen_type_expr(response_type)?;
            method_out.push_str(&format!(": Promise<{}> {{\n", return_ty));
        } else {
            method_out.push_str(": Promise<void> {\n");
        }

        // Generate the fetch body using the helper method
        let fetch_body =
            self.generate_fetch_body(&method.to_string(), url, has_request, has_response);

        method_out.push_str(&fetch_body);
        method_out.push_str("\n\t}\n");

        Ok(method_out)
    }

    fn gen_service(&self, service: &ServiceDef) -> Result<String> {
        let mut ty_out = String::new();
        let mut service_out = String::new();
        service_out.push_str(&format!(
            "export class {}Service {{\n",
            service.name.to_pascal_case()
        ));

        for item in service.items.iter() {
            match item {
                crate::ast::ServiceItem::Type(type_def) => {
                    let res = GenResponse::gen_type(self, type_def)?;
                    ty_out.push_str(&res);
                }
                crate::ast::ServiceItem::Call(call_def) => {
                    let res = self.gen_call(call_def)?;
                    service_out.push_str(&res);
                }
                crate::ast::ServiceItem::Enum(enum_def) => {
                    let res = GenResponse::gen_enum(self, enum_def)?;
                    ty_out.push_str(&res);
                }
            }
        }

        service_out.push_str("}\n");
        ty_out.push_str(&service_out);
        Ok(ty_out)
    }

    fn gen_enum(&self, ty: &EnumDef) -> Result<String> {
        self.base_gen_enum(ty)
    }
}

impl GenResponse for TypescriptGen {
    fn gen_type(&self, ty: &TypeDef) -> Result<String> {
        self.base_gen_type(ty)
    }

    fn gen_enum(&self, ty: &EnumDef) -> Result<String> {
        self.base_gen_enum(ty)
    }

    fn gen_service(&self, service: &ServiceDef) -> Result<String> {
        // just gen the types in a mod
        let mut out = String::new();
        let mut ty_count = 0;
        for item in service.items.iter() {
            match item {
                crate::ast::ServiceItem::Type(type_def) => {
                    let res = GenResponse::gen_type(self, type_def)?;
                    out.push_str(&res);
                    ty_count += 1;
                }
                crate::ast::ServiceItem::Call(_) => {}
                crate::ast::ServiceItem::Enum(enum_def) => {
                    let res = GenResponse::gen_enum(self, enum_def)?;
                    out.push_str(&res);
                    ty_count += 1;
                }
            }
        }
        if ty_count > 0 {
            Ok(out)
        } else {
            Ok(String::new())
        }
    }
}

impl RequestLanguages {
    pub fn into_gen(self) -> Box<dyn GenRequests> {
        match self {
            RequestLanguages::Typescript => Box::new(TypescriptGen::new()),
        }
    }
}

impl ResponseLanguages {
    fn into_gen(self) -> Box<dyn GenResponse> {
        match self {
            ResponseLanguages::Rust => Box::new(RustGen::new()),
            ResponseLanguages::Typescript => Box::new(TypescriptGen::new()),
        }
    }
}

pub struct CodeGenerator {
    request_gen: Box<dyn GenRequests>,
    response_gen: Box<dyn GenResponse>,
}

pub struct CodeGeneratorOut {
    request: String,
    response: String,
}

impl CodeGeneratorOut {
    pub fn request(&self) -> &String {
        &self.request
    }
    pub fn response(&self) -> &String {
        &self.response
    }
}

impl CodeGenerator {
    pub fn new(request_language: RequestLanguages, response_language: ResponseLanguages) -> Self {
        Self {
            request_gen: request_language.into_gen(),
            response_gen: response_language.into_gen(),
        }
    }

    pub fn gen_from(&self, items: Vec<TopLevelItem>) -> Result<CodeGeneratorOut> {
        let mut request_collection: Vec<String> = Vec::new();
        let mut response_collection: Vec<String> = Vec::new();
        for item in items {
            match item {
                TopLevelItem::Enum(enum_def) => {
                    request_collection.push(self.request_gen.gen_enum(&enum_def)?);
                    response_collection.push(self.response_gen.gen_enum(&enum_def)?);
                }
                TopLevelItem::Type(type_def) => {
                    request_collection.push(self.request_gen.gen_type(&type_def)?);
                    response_collection.push(self.response_gen.gen_type(&type_def)?);
                }
                TopLevelItem::Service(service_def) => {
                    request_collection.push(self.request_gen.gen_service(&service_def)?);
                    response_collection.push(self.response_gen.gen_service(&service_def)?);
                }
            }
        }
        Ok(CodeGeneratorOut {
            request: request_collection.join("\n"),
            response: response_collection.join("\n"),
        })
    }
}
