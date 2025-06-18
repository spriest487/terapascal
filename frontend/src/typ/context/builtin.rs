use crate::ast::Access;
use crate::ast::FunctionDeclKind;
use crate::ast::IdentPath;
use crate::ast::Path;
use crate::ast::Visibility;
use crate::typ;
use crate::typ::{ast, TypeName};
use crate::typ::ast::SELF_PARAM_NAME;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeParam;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use crate::Ident;
use crate::IntConstant;
use linked_hash_map::LinkedHashMap;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::*;

thread_local! {
    pub static BUILTIN_SPAN: Span = Span {
        file: Arc::new(PathBuf::from(BUILTIN_FILENAME)),
        start: Location::zero(),
        end: Location::zero(),
    };
}

pub const SYSTEM_UNIT_NAME: &str = "System";

pub const NIL_NAME: &str = "nil";

pub const NOTHING_TYPE_NAME: &str = "Nothing";
pub const ANY_TYPE_NAME: &str = "Object";

pub const TYPEINFO_TYPE_NAME: &str = "TypeInfo";
pub const TYPEINFO_NAME_METHOD: &str = "Name";
pub const TYPEINFO_METHODS_METHOD: &str = "Methods";

pub const METHOD_TYPE_NAME: &str = "MethodInfo";
pub const METHOD_NAME_METHOD: &str = "Name";

pub const FUNCINFO_TYPE_NAME: &str = "FunctionInfo";
pub const FUNCINFO_NAME_METHOD: &str = "Name";

pub const COMPARABLE_IFACE_NAME: &str = "IComparable";
pub const COMPARABLE_COMPARE_NAME: &str = "Compare";

// TODO: we can probably eventually replace this with an "Object" interface which all class? types
// implicitly implement, and has GetType() etc equivalents
pub const DISPLAYABLE_IFACE_NAME: &str = "IToString";
pub const DISPLAYABLE_TOSTRING_METHOD: &str = "ToString";

pub const STRING_TYPE_NAME: &str = "String";
// const STRING_CHARS_FIELD: &str = "chars";
// const STRING_LEN_FIELD: &str = "len";
pub const STRING_CHAR_TYPE: Primitive = Primitive::UInt8;

pub const OPTION_TYPE_NAME: &str = "Option";
pub const OPTION_TYPE_PARAM_NAME: &str = "T";
pub const OPTION_NONE_CASE: usize = 0;
pub const OPTION_SOME_CASE: usize = 1;

pub const STRING_CONCAT_FUNC_NAME: &str = "StringConcat";

pub const BUILTIN_FILENAME: &str = "<builtin>";

pub fn get_mem_sig() -> FunctionSig {
    FunctionSig {
        result_ty: Type::Primitive(Primitive::UInt8).ptr(),
        params: vec![FunctionSigParam {
            ty: Type::Primitive(Primitive::Int32),
            modifier: None,
        }],
        type_params: None,
    }
}

pub fn free_mem_sig() -> FunctionSig {
    FunctionSig {
        result_ty: Type::Nothing,
        params: vec![FunctionSigParam {
            ty: Type::Primitive(Primitive::UInt8).ptr(),
            modifier: None,
        }],
        type_params: None,
    }
}

pub fn builtin_span() -> Span {
    BUILTIN_SPAN.with(|span| span.clone())
}

pub fn builtin_ident(name: &str) -> Ident {
    Ident::new(name, builtin_span())
}

pub fn builtin_unit_path(name: &str) -> IdentPath {
    let builtin_span = builtin_span();

    IdentPath::new(
        Ident::new(name, builtin_span.clone()),
        vec![Ident::new(SYSTEM_UNIT_NAME, builtin_span)]
    )
}

pub fn builtin_string_name() -> Symbol {
    let builtin_span = builtin_span();

    let system_ident = Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone());
    let string_ident = Ident::new(STRING_TYPE_NAME, builtin_span.clone());

    Symbol {
        full_path: Path::from(system_ident).child(string_ident),
        type_args: None,
        type_params: None,
    }
}

pub fn is_system_string_name(sym: &Symbol) -> bool {
    sym.type_params.is_none()
        && sym.full_path.len() == 2
        && sym.full_path.as_slice()[0].name.as_str() == SYSTEM_UNIT_NAME
        && sym.full_path.as_slice()[1].name.as_str() == STRING_TYPE_NAME
}

pub fn system_option_type() -> Symbol {
    let path = IdentPath::from_parts([
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(OPTION_TYPE_NAME),
    ]);

    Symbol::from(path).with_ty_params(Some(TypeParamList::new(
        [TypeParam::new(builtin_ident(OPTION_TYPE_PARAM_NAME))],
        builtin_span(),
    )))
}

pub fn system_option_type_of(item_type: Type) -> Symbol {
    system_option_type().with_ty_args(Some(typ::TypeArgList::new(
        [TypeName::inferred(item_type)], 
        builtin_span(),
    )))
}

pub fn is_system_option_name(sym: &Symbol) -> bool {
    let Some(type_params) = sym.type_params.as_ref() else {
        return false;
    };
    
    type_params.len() == 1
        && sym.full_path.len() == 2
        && sym.full_path.as_slice()[0].name.as_str() == SYSTEM_UNIT_NAME
        && sym.full_path.as_slice()[1].name.as_str() == OPTION_TYPE_NAME
}

pub fn builtin_methodinfo_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(METHOD_TYPE_NAME),
    ]))
}

pub fn builtin_typeinfo_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(TYPEINFO_TYPE_NAME),
    ]))
}

pub fn builtin_funcinfo_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(FUNCINFO_TYPE_NAME),
    ]))
}

pub fn string_to_char_lit(string: &str) -> Option<ast::Literal> {
    if string.len() == 1 && string[..1].is_ascii() {
        let char_byte = string.as_bytes()[0];
        return Some(ast::Literal::Integer(IntConstant::from(char_byte)));
    }

    None
}

pub fn builtin_comparable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(COMPARABLE_IFACE_NAME),
    ]))
}

pub fn builtin_comparable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();
    
    let iface_ty = Type::interface(builtin_comparable_name().full_path);

    ast::InterfaceDecl {
        name: builtin_comparable_name(),
        where_clause: None,
        tags: Vec::new(),
        supers: None,
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: Arc::new(builtin_comparable_compare_method(iface_ty, Type::MethodSelf)),
            }
        ],
        forward: false,

        kw_span: builtin_span.clone().into(),
        span: builtin_span.clone().into(),
        end_kw_span: None,
    }
}

pub fn builtin_comparable_compare_method(declaring_ty: Type, self_param_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
        kw_span: builtin_span.clone(),
        name: ast::FunctionName::new_method_decl(
            builtin_ident(COMPARABLE_COMPARE_NAME),
            None,
            declaring_ty,
            builtin_span.clone(),
        ),
        tags: Vec::new(),
        where_clause: None,
        kind: FunctionDeclKind::Function,
        params: vec![
            ast::FunctionParam {
                name: Arc::new(SELF_PARAM_NAME.to_string()),
                ty: self_param_ty.clone(),
                ty_span: None,
                modifier: None,
                span: None,
            },
            ast::FunctionParam {
                name: Arc::new("other".to_string()),
                ty: self_param_ty.clone(),
                ty_span: None,
                modifier: None,
                span: None,
            }
        ],
        result_ty: Type::from(Primitive::Int32),
        result_ty_span: None,
        mods: Vec::new(),
        span: builtin_span.clone(),
    }
}

pub fn builtin_displayable_name() -> Symbol {
    Symbol::from(IdentPath::from_vec(vec![
        builtin_ident(SYSTEM_UNIT_NAME),
        builtin_ident(DISPLAYABLE_IFACE_NAME),
    ]))
}

pub fn builtin_displayable_iface() -> ast::InterfaceDecl {
    let builtin_span = builtin_span();
    
    let iface_ty = Type::interface(builtin_displayable_name().full_path); 

    ast::InterfaceDecl {
        name: builtin_displayable_name(),
        where_clause: None,
        tags: Vec::new(),
        supers: None,
        methods: vec![
            ast::InterfaceMethodDecl {
                decl: Arc::new(builtin_displayable_display_method(iface_ty, Type::MethodSelf)),
            }
        ],
        forward: false,
        
        kw_span: builtin_span.clone().into(), 
        span: builtin_span.clone().into(),
        end_kw_span: None,
    }
}

pub fn builtin_displayable_display_method(declaring_ty: Type, self_param_ty: Type) -> ast::FunctionDecl {
    let builtin_span = builtin_span();

    ast::FunctionDecl {
        kw_span: builtin_span.clone(),
        name: ast::FunctionName::new_method_decl(
            builtin_ident(DISPLAYABLE_TOSTRING_METHOD),
            None,
            declaring_ty,
            builtin_span.clone(),
        ),
        tags: Vec::new(),
        where_clause: None,
        kind: FunctionDeclKind::Function,
        params: vec![
            ast::FunctionParam {
                name: Arc::new(SELF_PARAM_NAME.to_string()),
                ty: self_param_ty,
                ty_span: None,
                modifier: None,
                span: None,
            }
        ],
        result_ty: Type::class(IdentPath::from_vec(vec![
            Ident::new(SYSTEM_UNIT_NAME, builtin_span.clone()),
            Ident::new(STRING_TYPE_NAME, builtin_span.clone()),
        ])),
        result_ty_span: None,
        mods: Vec::new(),
        span: builtin_span.clone(),
    }
}

pub fn declare_builtin_ty(
    ctx: &mut Context,
    name: &str,
    ty: Type,
    comparable: bool,
    displayable: bool
) -> TypeResult<LinkedHashMap<Ident, ast::MethodDecl>> {
    let builtin_span = builtin_span();
    
    let ident = Ident::new(name, builtin_span.clone());
    ctx.declare_type(ident, ty.clone(), Visibility::Interface, false)?;

    let type_env = Environment::TypeDecl { ty: ty.clone() };
    ctx.scope(type_env, |ctx| {
        let mut methods = LinkedHashMap::new();
        
        if displayable {
            let display_method = Arc::new(builtin_displayable_display_method(ty.clone(), ty.clone()));
            ctx.declare_function(display_method.name.ident.clone(), display_method.clone(), Visibility::Interface)?;

            methods.insert(display_method.name.ident.clone(), ast::MethodDecl {
                func_decl: display_method,
                access: Access::Published,
            });
        }

        if comparable {
            let compare_method = Arc::new(builtin_comparable_compare_method(ty.clone(), ty.clone()));
            ctx.declare_function(compare_method.name.ident.clone(), compare_method.clone(), Visibility::Interface)?;
            
            methods.insert(compare_method.name.ident.clone(), ast::MethodDecl {
                func_decl: compare_method,
                access: Access::Published,
            });
        }

        Ok(methods)
    })
}
