use crate::project::Project;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::Location;
use terapascal_common::span::MaybeSpanned;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_frontend::Ident;
use terapascal_frontend::Operator;
use terapascal_frontend::ast::Exit;
use terapascal_frontend::ast::ForLoopCounterInit;
use terapascal_frontend::ast::ForLoopRange;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::Literal;
use terapascal_frontend::ast::SetDeclRange;
use terapascal_frontend::ast::TypeMemberDecl;
use terapascal_frontend::typ::Context;
use terapascal_frontend::typ::Invocation;
use terapascal_frontend::typ::ModuleUnit;
use terapascal_frontend::typ::ScopeMemberRef;
use terapascal_frontend::typ::Type;
use terapascal_frontend::typ::TypeArgList;
use terapascal_frontend::typ::TypeName;
use terapascal_frontend::typ::TypeParamList;
use terapascal_frontend::typ::TypePattern;
use terapascal_frontend::typ::Value;
use terapascal_frontend::typ::ast::AliasDecl;
use terapascal_frontend::typ::ast::Block;
use terapascal_frontend::typ::ast::Call;
use terapascal_frontend::typ::ast::CaseBlock;
use terapascal_frontend::typ::ast::EnumDecl;
use terapascal_frontend::typ::ast::Expr;
use terapascal_frontend::typ::ast::ForLoop;
use terapascal_frontend::typ::ast::FunctionDecl;
use terapascal_frontend::typ::ast::FunctionDeclContext;
use terapascal_frontend::typ::ast::IfCond;
use terapascal_frontend::typ::ast::InterfaceDecl;
use terapascal_frontend::typ::ast::MatchBlock;
use terapascal_frontend::typ::ast::ObjectCtorArgs;
use terapascal_frontend::typ::ast::ObjectCtorMember;
use terapascal_frontend::typ::ast::SetDecl;
use terapascal_frontend::typ::ast::Stmt;
use terapascal_frontend::typ::ast::StructDecl;
use terapascal_frontend::typ::ast::SupersClause;
use terapascal_frontend::typ::ast::Tag;
use terapascal_frontend::typ::ast::TypeDeclItem;
use terapascal_frontend::typ::ast::UnitDecl;
use terapascal_frontend::typ::ast::VariantDecl;
use terapascal_frontend::typ::ast::WhereClause;
use terapascal_frontend::typ::ast::WhileLoop;
use terapascal_frontend::typ::seq::TypeSequenceSupport;

pub struct LinksEntry {
    pub key: Span,
    pub links: Vec<Span>,
}

impl LinksEntry {
    pub fn binary_search(entries: &[LinksEntry], location: Location) -> Option<&LinksEntry> {
        match entries.binary_search_by_key(&location, by_span_start) {
            Ok(index) => {
                let entry = &entries[index];
                if !entry.key.contains(&location) {
                    return None;
                }

                Some(&entries[index])
            },

            Err(index) => {
                if let Some(existing) = entries.get(index.saturating_sub(1)) {
                    if existing.key.contains(&location) {
                        return Some(&existing);
                    }
                }

                None
            },
        }
    }

    pub fn insert(entries: &mut Vec<LinksEntry>, key: Span, link: Span) {
        match entries.binary_search_by_key(&key.start, by_span_start) {
            Ok(existing) => {
                let existing_entry = &mut entries[existing];
                if existing_entry.key == key {
                    existing_entry.links.push(link);
                } else {
                    let existing_def = &existing_entry.links[0];

                    eprintln!(
                        "[definition_map] already have a link starting at {} (old: {}-{}, new: {}-{})",
                        key, existing_def, existing_def.end, link, link.end
                    );
                }
            },

            Err(insert_index) => {
                entries.insert(insert_index, LinksEntry {
                    key,
                    links: vec![link],
                });
            },
        }
    }
}

struct FileEntry {
    definitions: Vec<LinksEntry>,
    usages: Vec<LinksEntry>,
}

impl FileEntry {
    pub fn new() -> Self {
        FileEntry {
            definitions: Vec::new(),
            usages: Vec::new(),
        }
    }
}

pub struct DefinitionMap {
    entries: HashMap<Arc<PathBuf>, FileEntry>,
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
        let file_entries = self.entries.entry(span.file.clone()).or_insert_with(FileEntry::new);

        LinksEntry::insert(
            &mut file_entries.definitions,
            span.clone(),
            definition.clone(),
        );
        LinksEntry::insert(&mut file_entries.usages, definition, span);
    }

    pub fn add_self_def(&mut self, span: &Span) {
        let file_entries = self.entries.entry(span.file.clone()).or_insert_with(FileEntry::new);

        LinksEntry::insert(&mut file_entries.definitions, span.clone(), span.clone());
    }

    pub fn find_definitions(&self, file_path: &PathBuf, location: Location) -> Option<&LinksEntry> {
        let Some(file_entry) = self.entries.get(file_path) else {
            return None;
        };

        LinksEntry::binary_search(&file_entry.definitions, location)
    }

    pub fn find_usages(&self, file_path: &PathBuf, location: Location) -> Option<&LinksEntry> {
        let Some(file_entry) = self.entries.get(file_path) else {
            return None;
        };

        LinksEntry::binary_search(&file_entry.usages, location)
    }

    pub fn count_entries(&self) -> usize {
        self.entries
            .iter()
            .map(|(_, file_entries)| {
                let defs_len = file_entries.definitions.len();
                let usages_len = file_entries.usages.len();

                defs_len + usages_len
            })
            .sum()
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
                self.add_block(&def.body, ctx);
            },

            UnitDecl::Type { decl } => {
                for item in &decl.items {
                    self.add_type_decl(item, ctx);
                }
            },

            UnitDecl::Uses { decl } => {
                for item in &decl.units {
                    self.add_unit_ident(&item.ident, ctx);
                }
            },

            UnitDecl::Binding { decl } => {
                for item in &decl.items {
                    if let Some(init) = &item.init {
                        self.add_expr(&init.expr, ctx);
                    }
                }
            },
        }
    }

    fn add_unit_ident(&mut self, unit_ident: &IdentPath, ctx: &Context) {
        let mut partial_path = IdentPath::from(unit_ident.first().clone());
        let mut next_index = 1;

        loop {
            if let Some(scope_member) = ctx.find_path(&partial_path) {
                if let ScopeMemberRef::Scope { path } = scope_member {
                    let def_span = path.to_namespace().path_span();

                    self.add(partial_path.last().span.clone(), def_span);
                }
            }

            let Some(next_part) = unit_ident.as_slice().get(next_index) else {
                break;
            };

            partial_path = partial_path.child(next_part.clone());
            next_index += 1;
        }
    }

    fn add_type_decl(&mut self, type_decl: &TypeDeclItem, ctx: &Context) {
        self.add_self_def(&type_decl.name().ident().span);

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

    fn add_supers_clause(&mut self, supers: &SupersClause, ctx: &Context) {
        for super_type in &supers.types {
            self.add_typename(super_type, ctx);
        }
    }

    fn add_where_clause(&mut self, where_clause: &WhereClause, ctx: &Context) {
        for constraint in &where_clause.constraints {
            self.add_typename(&constraint.is_ty, ctx);
        }
    }

    fn add_struct_decl(&mut self, struct_decl: &StructDecl, ctx: &Context) {
        self.add_tags(&struct_decl.tags, ctx);

        if let Some(params) = &struct_decl.name.type_params {
            self.add_type_params(params, ctx);
        }

        if let Some(implements) = &struct_decl.implements {
            self.add_supers_clause(implements, ctx);
        }
        if let Some(where_clause) = &struct_decl.where_clause {
            self.add_where_clause(where_clause, ctx);
        }

        for member in struct_decl.members() {
            match member {
                TypeMemberDecl::Field(field_decl) => {
                    for ident in &field_decl.idents {
                        self.add_self_def(&ident.span);
                    }

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

        if let Some(params) = &iface_decl.name.type_params {
            self.add_type_params(params, ctx);
        }

        if let Some(supers) = &iface_decl.supers {
            self.add_supers_clause(supers, ctx);
        }
        if let Some(where_clause) = &iface_decl.where_clause {
            self.add_where_clause(where_clause, ctx);
        }

        for method_decl in &iface_decl.methods {
            self.add_func_decl(&method_decl.decl, ctx);
        }
    }

    fn add_variant_decl(&mut self, variant_decl: &VariantDecl, ctx: &Context) {
        self.add_tags(&variant_decl.tags, ctx);

        if let Some(params) = &variant_decl.name.type_params {
            self.add_type_params(params, ctx);
        }

        if let Some(implements) = &variant_decl.implements {
            self.add_supers_clause(implements, ctx);
        }
        if let Some(where_clause) = &variant_decl.where_clause {
            self.add_where_clause(where_clause, ctx);
        }

        for case in &variant_decl.cases {
            self.add_self_def(&case.ident.span);
            
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
        for item in &enum_decl.items {
            if let Some(expr) = &item.value {
                self.add_expr(&expr.expr, ctx);
            }
        }
    }

    fn add_set_decl(&mut self, set_decl: &SetDecl, ctx: &Context) {
        match &set_decl.range.as_ref() {
            SetDeclRange::Type { ty, span } => {
                let type_name = TypeName::named(ty.clone(), span.clone());
                self.add_typename(&type_name, ctx);
            },
            SetDeclRange::Range { from, to, .. } => {
                self.add_expr(from, ctx);
                self.add_expr(to, ctx);
            },
        }
    }

    fn add_tags(&mut self, tags: &[Tag], ctx: &Context) {
        for tag in tags {
            for item in &tag.items {
                self.add_typename(&item.tag_type, ctx);

                self.add_object_ctor_args(Some(item.tag_type.ty()), &item.args, ctx);
            }
        }
    }

    fn add_object_ctor_args(
        &mut self,
        object_ty: Option<&Type>,
        args: &ObjectCtorArgs,
        ctx: &Context,
    ) {
        for member in &args.members {
            self.add_object_ctor_member(object_ty, member, ctx);
        }
    }

    fn add_object_ctor_member(
        &mut self,
        object_ty: Option<&Type>,
        member: &ObjectCtorMember,
        ctx: &Context,
    ) {
        if let Some(ty) = object_ty {
            if let Ok(Some((decl, index))) = ty.find_field_decl(member.ident.as_str(), ctx) {
                let decl_ident = &decl.idents[index];

                self.add(member.ident.span.clone(), decl_ident.span.clone());
            }
        }

        self.add_expr(&member.value, ctx);
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
        
        self.add_self_def(&func_decl.name.ident.span);

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

        if let Some(params) = &func_decl.name.type_params {
            self.add_type_params(params, ctx);
        }

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
        match stmt {
            Stmt::Ident(ident, value) => {
                self.add_value(value, ident.span(), ctx);
            },

            Stmt::Member(member) => {
                self.add_expr(&member.base, ctx);
                self.add_value(&member.annotation, member.name.span(), ctx);
            },

            Stmt::LocalBinding(binding) => {
                self.add_self_def(&binding.name.span);

                self.add_typename(&binding.ty, ctx);
                if let Some(expr) = &binding.val {
                    self.add_expr(expr, ctx);
                }
            },

            Stmt::Call(call) => {
                self.add_call(call, ctx);
            },

            Stmt::Exit(exit_stmt) => {
                if let Exit::WithValue { value_expr, .. } = exit_stmt.as_ref() {
                    self.add_expr(value_expr, ctx);
                }
            },

            Stmt::Block(block) => {
                self.add_block(block, ctx);
            },

            Stmt::ForLoop(for_loop) => {
                self.add_for_loop(for_loop.as_ref(), ctx);
            },
            Stmt::WhileLoop(while_loop) => {
                self.add_while_loop(while_loop.as_ref(), ctx);
            },
            Stmt::Assignment(assignment) => {
                self.add_expr(&assignment.lhs, ctx);
                self.add_expr(&assignment.rhs, ctx);
            },
            Stmt::CompoundAssignment(assignment) => {
                self.add_expr(&assignment.lhs, ctx);
                self.add_expr(&assignment.rhs, ctx);
            },
            Stmt::If(if_cond) => {
                self.add_if_cond(if_cond, ctx, &Self::add_stmt);
            },
            Stmt::Raise(raise) => {
                self.add_expr(&raise.value, ctx);
            },

            Stmt::Case(case_block) => {
                self.add_case_block(case_block, ctx, &Self::add_stmt);
            },
            Stmt::Match(match_block) => {
                self.add_match_block(match_block, ctx, &Self::add_stmt);
            },

            Stmt::Break(_) | Stmt::Continue(_) => {},
        }
    }

    fn add_for_loop(&mut self, for_loop: &ForLoop, ctx: &Context) {
        match &for_loop.range {
            ForLoopRange::UpTo(range) => {
                match &range.init {
                    ForLoopCounterInit::Binding { init, ty, name, .. } => {
                        self.add_self_def(&name.span);

                        self.add_typename(ty, ctx);
                        self.add_expr(init, ctx);
                    },
                    ForLoopCounterInit::Assignment { value, counter, .. } => {
                        self.add_expr(value, ctx);
                        self.add_expr(counter, ctx);
                    },
                }

                self.add_expr(&range.to_expr, ctx);
            },

            ForLoopRange::InSequence(sequence) => {
                self.add_self_def(&sequence.binding_name.span);

                self.add_typename(&sequence.binding_ty, ctx);
                self.add_expr(&sequence.src_expr, ctx);

                // link the "in" keyword to the sequence type
                let src_ty = sequence.src_expr.annotation().ty();
                if let Some(seq_ty_span) = Self::find_sequence_type_def(&src_ty, ctx) {
                    self.add(sequence.in_kw_span.clone(), seq_ty_span);
                }
            },
        }

        self.add_stmt(&for_loop.body, ctx);
    }

    fn find_sequence_type_def(src_ty: &Type, ctx: &Context) -> Option<Span> {
        let seq_support = TypeSequenceSupport::try_from_type(src_ty, ctx).ok()?;

        let path = seq_support.sequence_type.full_path()?;

        let ty_def = ctx.find_type_def(path.as_ref())?;

        Some(ty_def.ident().span.clone())
    }

    fn add_while_loop(&mut self, while_loop: &WhileLoop, ctx: &Context) {
        self.add_expr(&while_loop.condition, ctx);
        self.add_stmt(&while_loop.body, ctx);
    }

    fn add_expr(&mut self, expr: &Expr, ctx: &Context) {
        match expr {
            Expr::BinOp(bin_op) => {
                self.add_expr(&bin_op.lhs, ctx);
                self.add_expr(&bin_op.rhs, ctx);
            },
            Expr::UnaryOp(unary_op) => {
                self.add_expr(&unary_op.operand, ctx);
            },
            Expr::Ident(ident, value) => self.add_value(value, ident.span(), ctx),
            Expr::Literal(item) => match &item.literal {
                Literal::SizeOf(ty) | Literal::DefaultValue(ty) | Literal::TypeInfo(ty) => {
                    self.add_typename(ty.as_ref(), ctx);
                },
                _ => {},
            },
            Expr::Call(call) => {
                self.add_call(call, ctx);
            },
            Expr::ObjectCtor(ctor) => {
                if let Some(expr) = &ctor.type_expr {
                    self.add_expr(expr, ctx);

                    let object_ty = match expr.annotation() {
                        Value::Type(object_ty, _) => Some(object_ty),
                        _ => None,
                    };

                    self.add_object_ctor_args(object_ty, &ctor.args, ctx);
                } else {
                    self.add_object_ctor_args(None, &ctor.args, ctx);
                }
            },
            Expr::CollectionCtor(ctor) => {
                for element in &ctor.elements {
                    self.add_expr(&element.value, ctx);
                }
            },
            Expr::IfCond(if_cond) => {
                self.add_if_cond(if_cond, ctx, &Self::add_expr);
            },
            Expr::Block(block) => {
                self.add_block(block, ctx);
            },
            Expr::Raise(raise) => {
                self.add_expr(&raise.value, ctx);
            },
            Expr::Exit(exit_expr) => {
                if let Exit::WithValue { value_expr, .. } = exit_expr.as_ref() {
                    self.add_expr(value_expr, ctx);
                }
            },
            Expr::Case(case_block) => self.add_case_block(case_block, ctx, &Self::add_expr),
            Expr::Match(match_block) => self.add_match_block(match_block, ctx, &Self::add_expr),
            Expr::Cast(cast) => {
                self.add_typename(&cast.as_type, ctx);
                self.add_expr(&cast.expr, ctx);
            },
            Expr::AnonymousFunction(_) => {},
            Expr::ExplicitSpec(spec) => {
                self.add_expr(&spec.type_expr, ctx);
                self.add_type_args(&spec.type_args, ctx);
            },
        }
    }

    fn add_type_params(&mut self, params: &TypeParamList, ctx: &Context) {
        for param in &params.items {
            self.add_self_def(&param.name.span);
            
            if let Some(constraint) = &param.constraint {
                self.add_typename(&constraint.is_ty, ctx);
            }
        }
    }

    fn add_type_args(&mut self, args: &TypeArgList, ctx: &Context) {
        for arg in &args.items {
            self.add_typename(arg, ctx);
        }
    }

    fn add_value(&mut self, value: &Value, at_span: &Span, ctx: &Context) {
        match value {
            Value::Typed(typed_val) => {
                if let Some(decl) = &typed_val.decl {
                    self.add(typed_val.span.clone(), decl.path_span().clone())
                }
            },

            Value::Function(function) => {
                self.add(at_span.clone(), function.decl.name.span.clone());
            },

            Value::UfcsFunction(ufcs) => {
                self.add(at_span.clone(), ufcs.decl.name.span.clone());
            },

            Value::Invocation(invocation) => match invocation.as_ref() {
                Invocation::Function { function, .. } => {
                    self.add(at_span.clone(), function.decl.name.span.clone());
                },
                Invocation::Method { method, .. } => {
                    self.add(at_span.clone(), method.decl.func_decl.name.span.clone());
                },
                Invocation::ObjectCtor {
                    object_type,
                    members,
                    ..
                } => {
                    self.add_typename(object_type, ctx);

                    for member in members {
                        self.add_object_ctor_member(Some(object_type.ty()), member, ctx);
                    }
                },
                Invocation::VariantCtor {
                    variant_type, case, ..
                } => {
                    if let Some(variant_path) = variant_type.full_path() {
                        self.add_variant_case(
                            variant_path.as_ref(),
                            &variant_path.path_span(),
                            &case,
                            ctx,
                        );
                    }
                },
                Invocation::FunctionValue { value, .. } => {
                    self.add_expr(value, ctx);
                },
            },

            Value::Method(method) => {
                self.add(at_span.clone(), method.decl.func_decl.name.span.clone());
            },

            Value::Type(ty, ..) => {
                let typename = TypeName::named(ty.clone(), at_span.clone());
                self.add_typename(&typename, ctx);
            },

            Value::Namespace(path, ..) => {
                if let Some(ScopeMemberRef::Scope { path }) = ctx.find_path(&path) {
                    let namespace = path.to_namespace();
                    self.add(at_span.clone(), namespace.path_span());
                }
            },

            Value::VariantCase(case_val) => {
                self.add_variant_case(
                    &case_val.variant_name.full_path,
                    &case_val.variant_name_span,
                    &case_val.case,
                    ctx,
                );
            },

            Value::Overload(overload) => {
                for candidate in &overload.candidates {
                    self.add(at_span.clone(), candidate.decl().name.span.clone());
                }
            },

            Value::Const(const_val) => {
                if let Some(path) = &const_val.decl {
                    self.add(at_span.clone(), path.path_span());
                }
            },

            Value::Untyped(_) => {
                // nothing
            },
        }
    }

    fn add_variant_case(
        &mut self,
        variant_path: &IdentPath,
        variant_name_span: &Span,
        case_ident: &Ident,
        ctx: &Context,
    ) {
        if let Some((variant_span, case_def_span)) =
            find_variant_case_def_span(variant_path, case_ident.as_str(), ctx)
        {
            self.add(variant_name_span.clone(), variant_span);
            self.add(case_ident.span.clone(), case_def_span);
        }
    }

    fn add_if_cond<B, AddItemFn>(
        &mut self,
        if_cond: &IfCond<B>,
        ctx: &Context,
        add_item: &AddItemFn,
    ) where
        AddItemFn: Fn(&mut Self, &B, &Context),
    {
        self.add_expr(&if_cond.cond, ctx);

        if let Some(pattern) = &if_cond.is_pattern {
            self.add_type_pattern(pattern, ctx);
        }

        add_item(self, &if_cond.then_branch, ctx);

        if let Some(branch) = &if_cond.else_branch {
            add_item(self, &branch.item, ctx);
        }
    }

    fn add_match_block<B, AddItemFn>(
        &mut self,
        match_block: &MatchBlock<B>,
        ctx: &Context,
        add_item: &AddItemFn,
    ) where
        AddItemFn: Fn(&mut Self, &B, &Context),
    {
        self.add_expr(&match_block.cond_expr, ctx);
        for branch in &match_block.branches {
            self.add_type_pattern(&branch.pattern, ctx);

            add_item(self, &branch.item, ctx);
        }

        if let Some(branch) = &match_block.else_branch {
            add_item(self, &branch.item, ctx);
        }
    }

    fn add_case_block<B, AddItemFn>(
        &mut self,
        match_block: &CaseBlock<B>,
        ctx: &Context,
        add_item: &AddItemFn,
    ) where
        AddItemFn: Fn(&mut Self, &B, &Context),
    {
        self.add_expr(&match_block.cond_expr, ctx);
        for branch in &match_block.branches {
            for case_value in &branch.case_values {
                self.add_expr(case_value, ctx);
            }

            add_item(self, &branch.item, ctx);
        }

        if let Some(branch) = &match_block.else_branch {
            add_item(self, &branch.item, ctx);
        }
    }

    fn add_type_pattern(&mut self, pattern: &TypePattern, ctx: &Context) {
        match pattern {
            TypePattern::VariantCase {
                variant,
                case,
                variant_name_span,
                data_binding,
                ..
            } => {
                self.add_variant_case(&variant.full_path, variant_name_span, case, ctx);
                if let Some(binding) = data_binding {
                    self.add_self_def(&binding.span);
                }
            },

            TypePattern::NegatedVariantCase {
                variant,
                case,
                variant_name_span,
                ..
            } => {
                self.add_variant_case(&variant.full_path, variant_name_span, case, ctx);
            },

            TypePattern::Type { ty, binding, .. } => {
                self.add_typename(ty, ctx);
                if let Some(binding) = binding {
                    self.add_self_def(&binding.span);
                }
            },

            TypePattern::NegatedType { ty, .. } => {
                self.add_typename(ty, ctx);
            },
        }
    }

    fn add_block(&mut self, block: &Block, ctx: &Context) {
        for stmt in &block.stmts {
            self.add_stmt(stmt, ctx);
        }

        if let Some(expr) = &block.output {
            self.add_expr(expr, ctx);
        }
    }

    fn add_call(&mut self, call: &Call, ctx: &Context) {
        match &call.target {
            Expr::BinOp(bin_op)
                if bin_op.op == Operator::Period && bin_op.rhs.as_ident().is_some() =>
            {
                let rhs_ident = bin_op.rhs.as_ident().unwrap();

                self.add_expr(&bin_op.lhs, ctx);
                self.add_value(&call.annotation, rhs_ident.span(), ctx);
            },

            _ => {
                self.add_expr(&call.target, ctx);
            },
        }

        if let Some(args) = &call.type_args {
            self.add_type_args(args, ctx);
        }

        for arg in &call.args {
            self.add_expr(arg, ctx);
        }
    }
}

fn find_variant_case_def_span(
    variant: &IdentPath,
    case: &str,
    ctx: &Context,
) -> Option<(Span, Span)> {
    let variant_decl = ctx.find_variant_def(variant).ok()?;
    let case = variant_decl.find_case(case)?;

    Some((variant_decl.name.span().clone(), case.span.clone()))
}

fn by_span_start(entry: &LinksEntry) -> Location {
    entry.key.start
}
