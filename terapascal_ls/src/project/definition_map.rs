use crate::project::Project;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::Location;
use terapascal_common::span::MaybeSpanned;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_frontend::ast::MemberDeclSection;
use terapascal_frontend::ast::TypeMemberDecl;
use terapascal_frontend::typ::Context;
use terapascal_frontend::typ::ModuleUnit;
use terapascal_frontend::typ::Type;
use terapascal_frontend::typ::TypeName;
use terapascal_frontend::typ::ast::AliasDecl;
use terapascal_frontend::typ::ast::EnumDecl;
use terapascal_frontend::typ::ast::Expr;
use terapascal_frontend::typ::ast::FunctionDecl;
use terapascal_frontend::typ::ast::FunctionDeclContext;
use terapascal_frontend::typ::ast::InterfaceDecl;
use terapascal_frontend::typ::ast::ObjectCtorArgs;
use terapascal_frontend::typ::ast::SetDecl;
use terapascal_frontend::typ::ast::Stmt;
use terapascal_frontend::typ::ast::StructDecl;
use terapascal_frontend::typ::ast::Tag;
use terapascal_frontend::typ::ast::TypeDeclItem;
use terapascal_frontend::typ::ast::UnitDecl;
use terapascal_frontend::typ::ast::VariantDecl;

struct DefinitionEntry {
    span: Span,
    definition: Span,
}

pub struct DefinitionMap {
    entries: HashMap<Arc<PathBuf>, Vec<DefinitionEntry>>,
}

impl DefinitionMap {
    pub fn empty() -> Self {
        DefinitionMap {
            entries: HashMap::new(),
        }
    }

    pub fn from_project(project: &Project) -> Self {
        let mut map = DefinitionMap {
            entries: HashMap::new(),
        };

        let Some(module) = &project.module else {
            return map;
        };

        for module_unit in &module.units {
            map.add_unit(module_unit);
        }

        map
    }

    pub fn add(&mut self, span: Span, definition: Span) {
        let file_entries = self.entries.entry(span.file.clone()).or_insert_with(Vec::new);

        match file_entries.binary_search_by_key(&span.start, by_span_start) {
            Ok(existing) => {
                let existing_def = &file_entries[existing].definition;
                eprintln!(
                    "[definition_map] already have a definition starting at {} (old: {}-{}, new: {}-{})",
                    span, existing_def, existing_def.end, definition, definition.end
                );
            },

            Err(insert_index) => {
                file_entries.insert(insert_index, DefinitionEntry { span, definition });
            },
        }
    }

    pub fn find(&self, file_path: &PathBuf, location: Location) -> Option<&Span> {
        let file_entries = self.entries.get(file_path)?;

        match file_entries.binary_search_by_key(&location, by_span_start) {
            Ok(index) => {
                let entry = &file_entries[index];
                if entry.span.contains(&location) {
                    Some(&file_entries[index].definition)
                } else {
                    None
                }
            },

            Err(index) => {
                let existing = file_entries.get(index.saturating_sub(1))?;
                if existing.span.contains(&location) {
                    Some(&existing.definition)
                } else {
                    None
                }
            },
        }
    }

    pub fn count_entries(&self) -> usize {
        self.entries.iter().map(|(_, file_entries)| file_entries.len()).sum()
    }

    fn add_unit(&mut self, module_unit: &ModuleUnit) {
        let unit = &module_unit.unit;
        let ctx = &module_unit.context;

        for iface_decl in &unit.iface_decls {
            self.add_unit_decl(iface_decl, ctx);
        }

        for impl_decl in &unit.impl_decls {
            self.add_unit_decl(impl_decl, ctx);
        }

        if let Some(init_block) = &unit.init {
            for stmt in &init_block.body {
                self.add_stmt(stmt, ctx);
            }
        }
    }

    fn add_unit_decl(&mut self, unit_decl: &UnitDecl, ctx: &Context) {
        match unit_decl {
            UnitDecl::FunctionDecl { decl } => {
                self.add_func_decl(decl.as_ref(), ctx);
            },
            UnitDecl::FunctionDef { def } => {
                self.add_func_decl(def.decl.as_ref(), ctx);
            },

            UnitDecl::Type { decl } => {
                for item in &decl.items {
                    self.add_type_decl(item, ctx);
                }
            },

            UnitDecl::Uses { .. } => {},
            UnitDecl::Binding { .. } => {},
        }
    }

    fn add_type_decl(&mut self, type_decl: &TypeDeclItem, ctx: &Context) {
        match type_decl {
            TypeDeclItem::Struct(struct_decl) => {
                self.add_struct_decl(struct_decl, ctx);
            },
            TypeDeclItem::Interface(iface_decl) => {
                self.add_iface_decl(iface_decl, ctx);
            },
            TypeDeclItem::Variant(variant_decl) => {
                self.add_variant_decl(variant_decl, ctx);
            },
            TypeDeclItem::Alias(alias_decl) => {
                self.add_alias_decl(alias_decl, ctx);
            },
            TypeDeclItem::Enum(enum_decl) => {
                self.add_enum_decl(enum_decl, ctx);
            },
            TypeDeclItem::Set(set_decl) => {
                self.add_set_decl(set_decl, ctx);
            },
        }
    }

    fn add_struct_decl(&mut self, struct_decl: &StructDecl, ctx: &Context) {
        self.add_tags(&struct_decl.tags, ctx);
        
        for member in struct_decl.members() {
            match member {
                TypeMemberDecl::Field(field_decl) => {
                    self.add_typename(&field_decl.ty, ctx);
                },
                TypeMemberDecl::Method(method_decl) => {
                    self.add_func_decl(&method_decl.func_decl, ctx);
                },
            }
        }
    }

    fn add_iface_decl(&mut self, iface_decl: &InterfaceDecl, ctx: &Context) {
        self.add_tags(&iface_decl.tags, ctx);

        for method_decl in &iface_decl.methods {
            self.add_func_decl(&method_decl.decl, ctx);
        }
    }

    fn add_variant_decl(&mut self, variant_decl: &VariantDecl, ctx: &Context) {
        self.add_tags(&variant_decl.tags, ctx);

        for case in &variant_decl.cases {
            let Some(data) = &case.data else {
                continue;
            };

            self.add_typename(&data.ty, ctx);
        }

        for section in &variant_decl.sections {
            for method in &section.methods {
                self.add_func_decl(&method.func_decl, ctx);
            }
        }
    }

    fn add_alias_decl(&mut self, alias_decl: &AliasDecl, ctx: &Context) {
        self.add_typename(&alias_decl.target, ctx);
    }

    fn add_enum_decl(&mut self, enum_decl: &EnumDecl, ctx: &Context) {
    }

    fn add_set_decl(&mut self, set_decl: &SetDecl, ctx: &Context) {
    }

    fn add_tags(&mut self, tags: &[Tag], ctx: &Context) {
        for tag in tags {
            for item in &tag.items {
                self.add_typename(&item.tag_type, ctx);

                self.add_object_ctor_args(item.tag_type.ty(), &item.args, ctx);
            }
        }
    }

    fn add_object_ctor_args(&mut self, ty: &Type, args: &ObjectCtorArgs, ctx: &Context) {
        for arg in &args.members {
            if let Ok(Some((decl, index))) = ty.find_field_decl(arg.ident.as_str(), ctx) {
                self.add(arg.ident.span.clone(), decl.idents[index].span.clone());
            }
            
            self.add_expr(&arg.value, ctx);
        }
    }

    fn add_typename(&mut self, type_name: &TypeName, ctx: &Context) {
        let Some(span) = type_name.get_span() else {
            return;
        };

        let Some(type_path) = type_name.ty().full_path() else {
            return;
        };

        let Some(def) = ctx.find_type_def(type_path.as_ref()) else {
            return;
        };

        let definition = def.ident().span().clone();
        self.add(span.clone(), definition);
    }

    fn add_func_decl(&mut self, func_decl: &FunctionDecl, ctx: &Context) {
        self.add_tags(&func_decl.tags, ctx);
        
        if let FunctionDeclContext::MethodDef {
            declaring_type,
            ty_name_span,
            ..
        } = &func_decl.name.context
        {
            // typename with a span of only the path part
            let type_path_name = TypeName::named(declaring_type.clone(), ty_name_span.clone());
            self.add_typename(&type_path_name, ctx);
        };

        for param in &func_decl.params {
            let Some(span) = &param.ty_span else {
                continue;
            };

            let param_ty_name = TypeName::named(param.ty.clone(), span.clone());
            self.add_typename(&param_ty_name, ctx);
        }

        self.add_typename(&func_decl.result_ty, ctx);
    }

    fn add_stmt(&mut self, stmt: &Stmt, ctx: &Context) {
    }

    fn add_expr(&mut self, expr: &Expr, ctx: &Context) {
    }
}

fn by_span_start(entry: &DefinitionEntry) -> Location {
    entry.span.start
}
