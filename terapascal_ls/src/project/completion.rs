use terapascal_frontend::Operator;
use terapascal_frontend::ast::Access;
use terapascal_frontend::ast::FunctionDeclKind;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::IncompleteExpr;
use terapascal_frontend::ast::Visibility;
use terapascal_frontend::typ::Binding;
use terapascal_frontend::typ::Context;
use terapascal_frontend::typ::Decl;
use terapascal_frontend::typ::InstanceMember;
use terapascal_frontend::typ::ScopeMember;
use terapascal_frontend::typ::ScopeMemberRef;
use terapascal_frontend::typ::Type;
use terapascal_frontend::typ::Value;
use terapascal_frontend::typ::ast::FunctionDecl;
use terapascal_frontend::typ::ast::Literal;
use terapascal_frontend::typ::ast::OverloadCandidate;
use tower_lsp::lsp_types as lsp;
use tower_lsp::lsp_types::CompletionItemLabelDetails;

const FIELD_ORDER: usize = 0;
const METHOD_ORDER: usize = 1;
const UFCS_ORDER: usize = 2;
const FUNCTION_ORDER: usize = 3;
const TYPE_ORDER: usize = 3;
const NAMESPACE_ORDER: usize = 3;
const VARIABLE_ORDER: usize = 0;

pub fn resolve_completions(incomplete: &IncompleteExpr<Value>) -> Vec<lsp::CompletionItem> {
    let mut entries = Vec::new();

    let ctx = &incomplete.context.context;

    match (incomplete.completion_op, incomplete.target.annotation()) {
        (Operator::Period, Value::Typed(typed_val)) => {
            let access = typed_val.ty.get_current_access(ctx);

            match ctx.instance_members(&typed_val.ty) {
                Ok(members) => {
                    for member in members {
                        instance_member_to_completions(member, access, &mut entries);
                    }
                },

                Err(err) => {
                    eprintln!("[resolve_completions] {}", err);
                },
            }
        },

        (Operator::Period, Value::Type(ty, ..)) => {
            type_static_members_to_completions(ty, ctx, &mut entries);
        },

        (Operator::Period, Value::Namespace(namespace, ..)) => {
            namespace_members_to_completions(namespace, ctx, &mut entries);
        },

        _ => {},
    }

    entries
}

fn namespace_members_to_completions(
    namespace: &IdentPath,
    ctx: &Context,
    entries: &mut Vec<lsp::CompletionItem>,
) {
    let is_local_ns = *namespace == ctx.namespace();

    let Some(ScopeMemberRef::Scope { path, .. }) = ctx.find_path(namespace) else {
        eprintln!("[resolve_completions] not a namespace path: {namespace}");
        return;
    };

    for (ident, member) in path.top().members() {
        match member {
            ScopeMember::Scope(..) => {
                let name = ident.name.to_string();
                let full_name = path.to_namespace().child(ident.clone());

                entries.push(lsp::CompletionItem {
                    sort_text: Some(format!("{NAMESPACE_ORDER}{name}")),
                    label: name,
                    detail: Some(full_name.to_string()),
                    kind: Some(lsp::CompletionItemKind::MODULE),
                    ..lsp::CompletionItem::default()
                });
            },

            ScopeMember::Decl(decl) => {
                let visible = if is_local_ns {
                    true
                } else if let Some(vis) = decl.visibility() {
                    vis >= Visibility::Interface
                } else {
                    true
                };

                if !visible {
                    continue;
                }

                let label = ident.name.to_string();

                match decl {
                    Decl::Alias(aliased) => {
                        if let Some(type_def) = ctx.find_type_def(aliased)
                            && let Some(ty) = type_def.defined_type(ctx)
                        {
                            entries.push(type_completion(label, &ty))
                        } else {
                            entries.push(lsp::CompletionItem {
                                sort_text: Some(format!("{TYPE_ORDER}{label}")),
                                label,
                                label_details: Some(CompletionItemLabelDetails {
                                    detail: Some(format!(" = {aliased}")),
                                    description: Some("alias".to_string()),
                                }),
                                kind: Some(lsp::CompletionItemKind::STRUCT),
                                ..lsp::CompletionItem::default()
                            })
                        }
                    },

                    Decl::Type { ty, .. } => {
                        entries.push(type_completion(label, ty));
                    },

                    Decl::GlobalVariable { binding, .. } => {
                        entries.push(binding_completion(label, binding))
                    },

                    Decl::GlobalConst { ty, val, .. } => {
                        entries.push(const_completion(label, ty, val))
                    },

                    Decl::Function { overloads, .. } => {
                        for overload in overloads {
                            entries.push(function_completion(overload.decl(), false));
                        }
                    },

                    _ => {},
                }
            },
        }
    }
}

fn type_static_members_to_completions(
    ty: &Type,
    ctx: &Context,
    entries: &mut Vec<lsp::CompletionItem>,
) {
    let access = ty.get_current_access(ctx);

    if let Ok(methods) = ty.methods(ctx) {
        for method in methods {
            if !method.func_decl.kind.is_static_method() || access < method.access {
                continue;
            }

            entries.push(function_completion(&method.func_decl, false));
        }
    }

    if let Type::Variant(variant_name) = ty {
        let Ok(variant_def) = ctx.instantiate_variant_def(variant_name) else {
            return;
        };

        for case in &variant_def.cases {
            let name = case.ident.name.to_string();

            entries.push(lsp::CompletionItem {
                sort_text: Some(format!("{FIELD_ORDER}{}", name)),
                detail: Some(format!("{}.{}", variant_name, name)),
                label: name,
                kind: Some(lsp::CompletionItemKind::ENUM_MEMBER),
                ..lsp::CompletionItem::default()
            });
        }
    }
}

fn function_completion(decl: &FunctionDecl, ufcs: bool) -> lsp::CompletionItem {
    let order = if ufcs {
        UFCS_ORDER
    } else if decl.kind == FunctionDeclKind::Function {
        FUNCTION_ORDER
    } else {
        METHOD_ORDER
    };

    let name = decl.ident().name.clone();
    let mut detail = String::new();
    if let Some(params) = &decl.name.type_params {
        detail.push_str(&params.to_string());
    }

    if decl.params_len() > 0 {
        detail.push_str("(");
        for group in &decl.param_groups {
            detail.push_str(&group.to_string());
        }
        detail.push_str(")");
    }

    if decl.result_ty != Type::Nothing {
        detail.push_str(": ");
        detail.push_str(&decl.result_ty.to_string());
    }

    let kind = match decl.kind {
        FunctionDeclKind::Function => lsp::CompletionItemKind::FUNCTION,
        FunctionDeclKind::ClassMethod => lsp::CompletionItemKind::METHOD,
        FunctionDeclKind::Constructor => lsp::CompletionItemKind::CONSTRUCTOR,
        FunctionDeclKind::Destructor => lsp::CompletionItemKind::METHOD,
    };

    lsp::CompletionItem {
        sort_text: Some(format!("{order}{name}")),
        label: name.to_string(),
        label_details: Some(lsp::CompletionItemLabelDetails {
            description: Some(decl.kind.to_string()),
            detail: Some(detail),
        }),
        detail: Some(decl.to_string()),
        kind: Some(kind),
        ..lsp::CompletionItem::default()
    }
}

fn const_completion(label: String, ty: &Type, val: &Literal) -> lsp::CompletionItem {
    lsp::CompletionItem {
        label,
        label_details: Some(lsp::CompletionItemLabelDetails {
            detail: Some(format!(": {} = {val}", ty)),
            description: Some("constant".to_string()),
        }),
        kind: Some(lsp::CompletionItemKind::CONSTANT),
        ..lsp::CompletionItem::default()
    }
}

fn binding_completion(label: String, binding: &Binding) -> lsp::CompletionItem {
    lsp::CompletionItem {
        sort_text: Some(format!("{VARIABLE_ORDER}{label}")),
        label,
        label_details: Some(lsp::CompletionItemLabelDetails {
            detail: Some(format!(": {}", binding.ty)),
            description: None,
        }),
        kind: Some(lsp::CompletionItemKind::VARIABLE),
        ..lsp::CompletionItem::default()
    }
}

fn type_completion(label: String, ty: &Type) -> lsp::CompletionItem {
    let kind = match ty {
        Type::Nothing | Type::Nil => lsp::CompletionItemKind::VALUE,

        Type::GenericParam(_) => lsp::CompletionItemKind::TYPE_PARAMETER,

        Type::Class(_) | Type::Weak(_) | Type::DynArray { .. } => lsp::CompletionItemKind::CLASS,

        Type::Interface(_) | Type::Any => lsp::CompletionItemKind::INTERFACE,

        Type::Enum(_) => lsp::CompletionItemKind::ENUM,

        _ => lsp::CompletionItemKind::STRUCT,
    };

    lsp::CompletionItem {
        sort_text: Some(format!("{TYPE_ORDER}{label}")),
        label,
        label_details: Some(lsp::CompletionItemLabelDetails {
            detail: ty.full_name().map(|sym| format!(" ({})", sym)),
            description: Some(ty.kind_description()),
        }),
        kind: Some(kind),
        ..lsp::CompletionItem::default()
    }
}

fn instance_member_to_completions(
    member: InstanceMember,
    access: Access,
    entries: &mut Vec<lsp::CompletionItem>,
) {
    match member {
        InstanceMember::Field {
            decl, decl_index, ..
        } if access >= decl.access => {
            let name = decl.idents[decl_index].name.clone();
            let detail = format!("{} {}: {}", decl.access, name, decl.ty);

            entries.push(lsp::CompletionItem {
                label: name.to_string(),
                kind: Some(lsp::CompletionItemKind::FIELD),
                sort_text: Some(format!("{FIELD_ORDER}{name}")),
                detail: Some(detail),
                ..lsp::CompletionItem::default()
            });
        },

        InstanceMember::Method { method, .. }
            if access >= method.access && !method.func_decl.kind.is_static_method() =>
        {
            entries.push(function_completion(&method.func_decl, false));
        },

        InstanceMember::UFCSCall { decl, .. } => {
            entries.push(function_completion(&decl, true));
        },

        InstanceMember::Overloaded { candidates } => {
            for candidate in candidates {
                let completion = match candidate {
                    OverloadCandidate::Method { decl, .. }
                        if access >= decl.access && !decl.func_decl.kind.is_static_method() =>
                    {
                        Some(function_completion(&decl.func_decl, false))
                    },

                    OverloadCandidate::Function { decl, .. } => {
                        Some(function_completion(&decl, true))
                    },

                    _ => None,
                };

                if let Some(completion) = completion {
                    entries.push(completion);
                }
            }
        },

        _ => {},
    }
}
