use std::fmt;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::MaybeSpanned;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_frontend::ast;
pub use terapascal_frontend::ast::Annotation;
use terapascal_frontend::ast::ConstExprValue;
use terapascal_frontend::ast::DeclName;
use terapascal_frontend::ast::FunctionName;
use terapascal_frontend::ast::InterfaceDecl;
use terapascal_frontend::ast::MemberDeclSection;
use terapascal_frontend::ast::Pattern;
use terapascal_frontend::ast::PatternSemanticElement;
use terapascal_frontend::ast::SemanticHint;
use terapascal_frontend::ast::SetDeclRange;
use terapascal_frontend::ast::StructDecl;
use terapascal_frontend::ast::TypeAnnotation;
use terapascal_frontend::ast::TypeDeclItem;
use terapascal_frontend::ast::TypeMemberDeclRef;
use terapascal_frontend::ast::UnaryPosition;
use terapascal_frontend::ast::VariantDecl;
use terapascal_frontend::Operator;
use tower_lsp::lsp_types::SemanticToken;
use tower_lsp::lsp_types::SemanticTokenType;

pub fn semantic_legend() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NUMBER,
        SemanticTokenType::STRING,
        SemanticTokenType::OPERATOR,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::TYPE,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::ENUM,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::METHOD,
    ]
}

const SEMANTIC_KEYWORD: u32 = 0;
const SEMANTIC_NUMBER: u32 = 1;
const SEMANTIC_STRING: u32 = 2;
const SEMANTIC_OPERATOR: u32 = 3;
const SEMANTIC_NAMESPACE: u32 = 4;
const SEMANTIC_FUNCTION: u32 = 5;
const SEMANTIC_TYPE: u32 = 6;
const SEMANTIC_TYPE_PARAMETER: u32 = 7;
const SEMANTIC_PARAMETER: u32 = 8;
const SEMANTIC_VARIABLE: u32 = 9;
const SEMANTIC_ENUM: u32 = 10;
const SEMANTIC_ENUM_MEMBER: u32 = 11;
const SEMANTIC_PROPERTY: u32 = 12;
const SEMANTIC_METHOD: u32 = 13;

pub struct SemanticTokenBuilder<A>
where
    A: Annotation + fmt::Display + fmt::Debug,
{
    cur_line: u32,
    cur_col: u32,

    file: Arc<PathBuf>,

    result: Vec<SemanticToken>,

    _a: PhantomData<A>,
}

impl<A> SemanticTokenBuilder<A>
where
    A: Annotation + fmt::Display + fmt::Debug,
{
    pub fn new(file: Arc<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            cur_line: line,
            cur_col: col,
            result: Vec::new(),

            file,

            _a: PhantomData,
        }
    }

    pub fn finish(self) -> Vec<SemanticToken> {
        self.result
    }

    fn token_delta(&mut self, span: &Span, debug_desc: &str) -> Option<(u32, u32)> {
        let prev_line = self.cur_line;
        let line = span.start.line as u32;
        if line < prev_line {
            eprintln!(
                "[semantic_tokens] invalid line in {debug_desc} span {}-{} (expected >= {}:{})",
                span,
                span.end,
                prev_line + 1,
                self.cur_col + 1
            );
            return None;
        }

        let delta_line = line.saturating_sub(self.cur_line);
        if line != self.cur_line {
            self.cur_line = line;
            self.cur_col = 0;
        }

        let col = span.start.col as u32;
        if line == prev_line && col < self.cur_col {
            eprintln!(
                "[semantic_tokens] invalid col in {debug_desc} span {}-{} (expected >= {}:{})",
                span,
                span.end,
                line + 1,
                self.cur_col + 1
            );
            return None;
        }

        let delta_col = col.saturating_sub(self.cur_col);
        self.cur_col = col;

        Some((delta_line, delta_col))
    }

    fn add(&mut self, span: &Span, token_type: u32, debug_desc: &str) {
        if span.file != self.file {
            eprintln!(
                "[semantic_tokens] invalid {} span @ {}-{}: not from this file ({})",
                debug_desc,
                span,
                span.end,
                self.file.display()
            );
            return;
        }

        let Some((delta_line, delta_col)) = self.token_delta(span, debug_desc) else {
            return;
        };

        self.result.push(SemanticToken {
            delta_line,
            delta_start: delta_col,
            length: span.end.col.saturating_sub(span.start.col) as u32 + 1,
            token_type,
            token_modifiers_bitset: 0,
        });
    }

    fn add_keyword(&mut self, span: &Span) {
        self.add(span, SEMANTIC_KEYWORD, "keyword");
    }

    pub fn add_unit(&mut self, unit: &ast::Unit<A>) {
        if let Some(unit_kw) = &unit.unit_kw {
            self.add_keyword(unit_kw);
        }

        let unit_path_span = unit.ident.path_span();
        if unit_path_span.end.col != 0 {
            self.add(
                &unit.ident.path_span(),
                SEMANTIC_NAMESPACE,
                "unit identifier",
            );
        }

        if let Some(iface_kw) = &unit.iface_kw {
            self.add_keyword(iface_kw);
        }

        for decl in &unit.iface_decls {
            self.add_unit_decl(decl);
        }

        if let Some(impl_kw) = &unit.impl_kw {
            self.add_keyword(impl_kw);
        }

        for decl in &unit.impl_decls {
            self.add_unit_decl(decl);
        }

        if let Some(init_block) = &unit.init {
            self.add_keyword(&init_block.keyword_span);

            for stmt in &init_block.body {
                self.add_stmt(stmt);
            }

            if let Some(span) = &init_block.end_span {
                self.add_keyword(span);
            }
        }

        if let Some(span) = &unit.end_kw {
            self.add_keyword(span);
        }
    }

    fn add_unit_decl(&mut self, decl: &ast::UnitDecl<A>) {
        match decl {
            ast::UnitDecl::FunctionDecl { decl } => {
                self.add_func_decl(decl, SEMANTIC_FUNCTION)
            },
            ast::UnitDecl::FunctionDef { def } => {
                self.add_func_def(def, SEMANTIC_FUNCTION)
            },

            ast::UnitDecl::Type { decl } => self.add_type_decl(decl),
            ast::UnitDecl::Uses { decl } => {
                for unit_path in &decl.units {
                    for part in unit_path.ident.as_slice() {
                        self.add(&part.span, SEMANTIC_NAMESPACE, "used namespace");
                    }
                }
            }
            ast::UnitDecl::Binding { decl } => self.add_unit_binding(decl),
        }
    }

    fn add_type_decl(&mut self, type_decl: &ast::TypeDecl<A>) {
        self.add_keyword(&type_decl.kw_span);

        for item in &type_decl.items {
            self.add_tags(item.tags());

            let item_name = item.name();
            self.add(
                item_name.ident().span(),
                match item {
                    TypeDeclItem::Enum(..) | TypeDeclItem::Variant(..) => SEMANTIC_ENUM,
                    TypeDeclItem::Set(..) => SEMANTIC_NUMBER,
                    _ => SEMANTIC_TYPE,
                },
                "type name",
            );

            for i in 0..item_name.type_params_len() {
                if let Some(param_span) = item_name.type_param_name_span(i) {
                    self.add(param_span, SEMANTIC_TYPE_PARAMETER, "type parameter");
                }
            }

            match item {
                TypeDeclItem::Struct(struct_decl) => {
                    self.add_struct_decl(struct_decl);
                },

                TypeDeclItem::Interface(iface_decl) => {
                    self.add_iface_decl(iface_decl)
                },

                TypeDeclItem::Variant(variant_decl) => {
                    self.add_variant_decl(variant_decl);
                },

                TypeDeclItem::Alias(aliased) => {
                    self.add_typename(&aliased.target, "alias typename");
                },

                TypeDeclItem::Enum(enum_decl) => {
                    self.add_enum_decl(enum_decl)
                },
                
                TypeDeclItem::Set(set_decl) => {
                    self.add_set_decl(set_decl)
                },
            }
        }
    }

    fn add_struct_decl(&mut self, struct_decl: &StructDecl<A>) {
        self.add_keyword(&struct_decl.kw_span);

        if let Some(supers) = &struct_decl.implements {
            self.add_supers_clause(supers);
        }

        if let Some(clause) = &struct_decl.where_clause {
            self.add_where_clause(clause);
        }

        for section in &struct_decl.sections {
            self.add_type_decl_section(section);
        }

        if let Some(span) = &struct_decl.end_kw_span {
            self.add_keyword(span);
        }
    }

    fn add_variant_decl(&mut self, variant_decl: &VariantDecl<A>) {
        self.add_keyword(&variant_decl.kw_span);

        if let Some(supers) = &variant_decl.implements {
            self.add_supers_clause(supers);
        }

        if let Some(clause) = &variant_decl.where_clause {
            self.add_where_clause(clause);
        }

        for case in &variant_decl.cases {
            self.add(&case.ident.span, SEMANTIC_ENUM_MEMBER, "variant case name");

            if let Some(data) = &case.data {
                self.add_typename(&data.ty, "variant case data type");
            }
        }

        for section in &variant_decl.sections {
            self.add_type_decl_section(section);
        }

        if let Some(span) = &variant_decl.end_kw_span {
            self.add_keyword(span);
        }
    }

    fn add_iface_decl(&mut self, iface_decl: &InterfaceDecl<A>) {
        self.add_keyword(&iface_decl.kw_span);

        if let Some(supers) = &iface_decl.supers {
            self.add_supers_clause(supers);
        }

        if let Some(clause) = &iface_decl.where_clause {
            self.add_where_clause(clause);
        }

        for method in &iface_decl.methods {
            self.add_func_decl(&method.decl, SEMANTIC_METHOD);
        }

        if let Some(span) = &iface_decl.end_kw_span {
            self.add_keyword(span);
        }
    }

    fn add_enum_decl(&mut self, enum_decl: &ast::EnumDecl<A>) {
        for item in &enum_decl.items {
            self.add(
                &item.ident.span,
                SEMANTIC_ENUM_MEMBER,
                "enumeration item name",
            );
            if let Some(val) = &item.value {
                self.add(val.span(), SEMANTIC_NUMBER, "enumeration item value");
            }
        }
    }

    fn add_set_decl(&mut self, set_decl: &ast::SetDecl<A>) {
        match set_decl.range.as_ref() {
            SetDeclRange::Type { span, ty, .. } => {
                self.add_type(ty, span, "set type name");
            },

            SetDeclRange::Range {
                from,
                to,
                range_op_span,
                ..
            } => {
                self.add_expr(from);
                self.add(range_op_span, SEMANTIC_OPERATOR, "set range operator");
                self.add_expr(to);
            },
        }
    }

    fn add_type_decl_section(&mut self, section: &impl MemberDeclSection<A>) {
        if let Some(kw_span) = &section.access_kw_span() {
            self.add_keyword(kw_span);
        }

        for member in section.members() {
            match member {
                TypeMemberDeclRef::Field(field) => {
                    for ident in &field.idents {
                        self.add(ident.span(), SEMANTIC_PROPERTY, "field name");
                    }
                    self.add_typename(&field.ty, "field type");
                },

                TypeMemberDeclRef::Method(decl) => {
                    self.add_func_decl(&decl.func_decl, SEMANTIC_METHOD);
                },
            }
        }
    }

    fn add_where_clause(&mut self, clause: &ast::WhereClause<A>) {
        self.add_keyword(&clause.where_kw_span);

        for constraint in &clause.constraints {
            self.add(
                &constraint.name.span,
                SEMANTIC_TYPE_PARAMETER,
                "constraint name",
            );

            if let Some(span) = &constraint.is_kw_span {
                self.add_keyword(span);
            }

            self.add_typename(&constraint.is_ty, "constraint type name");
        }
    }

    fn add_supers_clause(&mut self, clause: &ast::SupersClause<A>) {
        self.add_keyword(&clause.kw_span);

        for ty in &clause.types {
            self.add_typename(ty, "super type name");
        }
    }

    fn add_unit_binding(&mut self, binding: &ast::UnitBinding<A>) {
        self.add_keyword(&binding.kw_span);

        for item in &binding.items {
            for ident in &item.idents {
                self.add(&ident.span, SEMANTIC_VARIABLE, "unit binding name");
            }

            if let Some(span) = &item.ty_span {
                self.add_type(&item.ty, span, "unit binding type");
            }

            if let Some(init) = &item.init {
                self.add(&init.eq_span, SEMANTIC_OPERATOR, "unit binding assignment");
                self.add_expr(&init.expr);
            }
        }
    }

    fn add_stmt(&mut self, stmt: &ast::Stmt<A>) {
        match stmt {
            ast::Stmt::Ident(ident, value) => {
                self.add_value(value, ident.span(), "ident statement")
            },
            ast::Stmt::Member(member) => self.add_member_stmt(member),
            ast::Stmt::LocalBinding(binding) => self.add_local_binding(binding),
            ast::Stmt::Call(call) => self.add_call(call),
            ast::Stmt::Exit(exit) => self.add_exit(exit),
            ast::Stmt::Block(block) => self.add_block(block),
            ast::Stmt::ForLoop(for_loop) => self.add_for(for_loop),
            ast::Stmt::WhileLoop(while_loop) => self.add_while(while_loop),
            ast::Stmt::Assignment(assignment) => self.add_assignment(assignment),
            ast::Stmt::CompoundAssignment(assignment) => self.add_compound_assignment(assignment),
            ast::Stmt::If(if_cond) => self.add_if_cond(if_cond, Self::add_stmt),
            ast::Stmt::Break(a) | ast::Stmt::Continue(a) => self.add_keyword(a.span()),
            ast::Stmt::Raise(raise) => self.add_raise(raise),
            ast::Stmt::Case(block) => self.add_case_block(block, Self::add_stmt),
            ast::Stmt::Match(block) => self.add_match_block(block, Self::add_stmt),
        }
    }

    fn add_member_stmt(&mut self, member_stmt: &ast::MemberStmt<A>) {
        self.add_expr(&member_stmt.base);

        self.add_value(
            &member_stmt.annotation,
            &member_stmt.name.span,
            "member statement name",
        );
    }

    fn add_assignment(&mut self, assignment: &ast::Assignment<A>) {
        self.add_expr(&assignment.lhs);
        self.add(
            &assignment.op_span,
            SEMANTIC_OPERATOR,
            "assignment operator",
        );
        self.add_expr(&assignment.rhs);
    }

    fn add_compound_assignment(&mut self, assignment: &ast::CompoundAssignment<A>) {
        self.add_expr(&assignment.lhs);
        self.add(
            &assignment.op_span,
            SEMANTIC_OPERATOR,
            "compound assignment operator",
        );
        self.add_expr(&assignment.rhs);
    }

    fn add_local_binding(&mut self, binding: &ast::LocalBinding<A>) {
        self.add_keyword(&binding.kw_span);

        self.add(
            &binding.name.span,
            SEMANTIC_VARIABLE,
            "variable binding name",
        );

        self.add_typename(&binding.ty, "variable binding type");

        if let Some(span) = &binding.assign_op_span {
            self.add(span, SEMANTIC_OPERATOR, "variable binding assignment");
        }

        if let Some(expr) = &binding.val {
            self.add_expr(expr);
        }
    }

    fn add_if_cond<B: Spanned, BranchFn>(
        &mut self,
        if_cond: &ast::IfCond<B, A>,
        add_branch: BranchFn,
    ) where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add_keyword(&if_cond.if_kw_span);
        self.add_expr(&if_cond.cond);

        if let Some(span) = &if_cond.is_kw {
            self.add_keyword(span);
        }

        if let Some(pattern) = &if_cond.is_pattern {
            self.add_pattern(pattern);
        }

        self.add_keyword(&if_cond.then_kw_span);
        add_branch(self, &if_cond.then_branch);

        if let Some(branch) = &if_cond.else_branch {
            self.add_keyword(&branch.else_kw_span);
            add_branch(self, &branch.item);
        }
    }

    fn add_case_block<B, BranchFn>(&mut self, block: &ast::CaseBlock<B, A>, add_branch: BranchFn)
    where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add_keyword(&block.kw_span);
        self.add_expr(&block.cond_expr);
        self.add_keyword(&block.of_span);

        for branch in &block.branches {
            for val in &branch.case_values {
                self.add_expr(val);
            }
            add_branch(self, &branch.item);
        }

        if let Some(branch) = &block.else_branch {
            self.add_keyword(&branch.else_kw_span);
            add_branch(self, &branch.item);
        }

        self.add_keyword(&block.end_span);
    }

    fn add_match_block<B, BranchFn>(&mut self, block: &ast::MatchBlock<B, A>, add_branch: BranchFn)
    where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add_keyword(&block.kw_span);
        self.add_expr(&block.cond_expr);
        self.add_keyword(&block.of_span);

        for branch in &block.branches {
            self.add_pattern(&branch.pattern);
            add_branch(self, &branch.item);
        }

        if let Some(branch) = &block.else_branch {
            self.add_keyword(&branch.else_kw_span);
            add_branch(self, &branch.item);
        }

        self.add_keyword(&block.end_span);
    }

    fn add_pattern(&mut self, pattern: &A::Pattern) {
        for element in pattern.semantic_elements() {
            match element {
                PatternSemanticElement::Keyword(span) => {
                    self.add_keyword(&span);
                },
                PatternSemanticElement::Binding(span) => {
                    self.add(&span, SEMANTIC_VARIABLE, "pattern binding");
                },
                PatternSemanticElement::Type(ty) => {
                    self.add_typename(&ty, "pattern type");
                },
                PatternSemanticElement::VariantCase(span) => {
                    self.add(&span, SEMANTIC_ENUM_MEMBER, "variant member pattern");
                },
                PatternSemanticElement::Path(span) => {
                    self.add(&span, SEMANTIC_TYPE, "type pattern");
                },
            }
        }
    }

    fn add_cast(&mut self, cast: &ast::Cast<A>) {
        self.add_expr(&cast.expr);

        if let Some(span) = &cast.as_kw {
            self.add_keyword(span);
        }

        self.add_typename(&cast.as_type, "cast typename");
    }

    fn add_for(&mut self, for_loop: &ast::ForLoop<A>) {
        self.add_keyword(&for_loop.for_kw_span);

        match &for_loop.range {
            ast::ForLoopRange::UpTo(range) => {
                match &range.init {
                    ast::ForLoopCounterInit::Binding {
                        binding_kw_span,
                        name,
                        ty,
                        assign_op_span,
                        init,
                    } => {
                        self.add_keyword(binding_kw_span);
                        self.add(name.span(), SEMANTIC_VARIABLE, "for loop counter binding");
                        self.add_typename(ty, "for loop counter type");

                        self.add(
                            assign_op_span,
                            SEMANTIC_OPERATOR,
                            "for loop counter assignment",
                        );
                        self.add_expr(init);
                    },

                    ast::ForLoopCounterInit::Assignment {
                        counter,
                        assign_op_span,
                        value,
                    } => {
                        self.add_expr(counter);
                        self.add(
                            assign_op_span,
                            SEMANTIC_OPERATOR,
                            "for loop counter assignment",
                        );
                        self.add_expr(value);
                    },
                }

                self.add_keyword(&range.to_kw_span);
                self.add_expr(&range.to_expr);
            },

            ast::ForLoopRange::InSequence(range) => {
                self.add_keyword(&range.binding_kw_span);
                self.add(
                    &range.binding_name.span,
                    SEMANTIC_VARIABLE,
                    "for loop range binding",
                );
                self.add_typename(&range.binding_ty, "for loop range type");

                self.add_keyword(&range.in_kw_span);
                self.add_expr(&range.src_expr);
            },
        }

        self.add_keyword(&for_loop.do_kw_span);
        self.add_stmt(&for_loop.body);
    }

    fn add_while(&mut self, while_loop: &ast::WhileLoop<A>) {
        self.add_keyword(&while_loop.while_kw_span);
        self.add_expr(&while_loop.condition);
        self.add_keyword(&while_loop.do_kw_span);
        self.add_stmt(&while_loop.body);
    }
    
    fn add_arg_list(&mut self, args: &[ast::Expr<A>], args_span: Option<&Span>) {
        let (args_open, args_close) = match (&args_span, Span::range(&args)) {
            (Some(args_span), Some(args_inner_span)) => {
                let (left, right) = args_span.split(&args_inner_span);

                (Some(left), Some(right))
            }

            _ => (None, None),
        };

        if let Some(span) = &args_open {
            // eprintln!("left span: {span}-{}", span.end);
            self.add(span, SEMANTIC_OPERATOR, "arg list open bracket");
        }

        for arg in args {
            // eprintln!("arg ({arg}) span: {}-{}", arg.span(), arg.span().end);
            self.add_expr(arg);
        }

        if let Some(span) = &args_close {
            // eprintln!("right span: {span}-{}", span.end);
            self.add(span, SEMANTIC_OPERATOR, "arg list close bracket");
        }
    }

    fn add_call(&mut self, call: &ast::Call<A>) {
        self.add_expr(&call.target);

        if let Some(args) = &call.type_args {
            self.add_type_arg_list(args);
        }

        self.add_arg_list(&call.args, call.args_span.as_ref());
    }

    fn add_type_arg_list(&mut self, args: &ast::TypeArgList<A>) {
        for arg in &args.items {
            self.add_typename(&arg, "type argument");
        }
    }

    fn add_expr(&mut self, expr: &ast::Expr<A>) {
        match expr {
            ast::Expr::BinOp(op) => self.add_bin_op(op),
            ast::Expr::UnaryOp(op) => self.add_unary_op(op),
            ast::Expr::Literal(item) => self.add_literal(item),
            ast::Expr::Ident(ident, value) => {
                self.add_value(value, ident.span(), "ident expr")
            },
            ast::Expr::Call(call) => self.add_call(call),
            ast::Expr::ObjectCtor(ctor) => self.add_object_ctor(ctor),
            ast::Expr::CollectionCtor(_) => {},
            ast::Expr::IfCond(if_cond) => self.add_if_cond(if_cond, Self::add_expr),
            ast::Expr::Block(block) => self.add_block(block),
            ast::Expr::Raise(raise) => self.add_raise(raise),
            ast::Expr::Exit(exit) => self.add_exit(exit),
            ast::Expr::Case(block) => self.add_case_block(block, Self::add_expr),
            ast::Expr::Match(block) => self.add_match_block(block, Self::add_expr),
            ast::Expr::Cast(cast) => self.add_cast(cast),
            ast::Expr::AnonymousFunction(_) => {},
            ast::Expr::ExplicitSpec(_) => {},
        }
    }

    fn add_value(&mut self, value: &A, display_span: &Span, debug_desc: &str) {
        if let Some(token_type) = semantic_hint_to_token_type(value.semantic_hint()) {
            self.add(display_span, token_type, debug_desc);
        }
    }

    fn add_bin_op(&mut self, bin_op: &ast::BinOp<A>) {
        self.add_expr(&bin_op.lhs);

        let op_semantic = if bin_op.op.is_keyword() {
            SEMANTIC_KEYWORD
        } else {
            SEMANTIC_OPERATOR
        };

        if bin_op.op == Operator::Index {
            let (left_span, right_span) = &bin_op.op_span.split(&bin_op.rhs);

            self.add(&left_span, op_semantic, "left index bracket");
            self.add_expr(&bin_op.rhs);
            self.add(&right_span, op_semantic, "right index bracket");
        } else {
            self.add(&bin_op.op_span, op_semantic, "operator");
            self.add_expr(&bin_op.rhs);
        }
    }

    fn add_unary_op(&mut self, unary_op: &ast::UnaryOp<A>) {
        let op_semantic = if unary_op.op.is_keyword() {
            SEMANTIC_KEYWORD
        } else {
            SEMANTIC_OPERATOR
        };

        match unary_op.pos {
            UnaryPosition::Prefix => {
                self.add(&unary_op.op_span, op_semantic, "unary operator");
                self.add_expr(&unary_op.operand);
            },
            UnaryPosition::Postfix => {
                self.add_expr(&unary_op.operand);
                self.add(&unary_op.op_span, op_semantic, "unary operator");
            },
        }
    }

    fn add_literal(&mut self, item: &ast::LiteralItem<A>) {
        let span = item.annotation.span();

        match &item.literal {
            ast::Literal::Nil => self.add(span, SEMANTIC_KEYWORD, "nil literal"),
            ast::Literal::Integer(..) | ast::Literal::Real(_) => {
                self.add(span, SEMANTIC_NUMBER, "integer literal")
            },
            ast::Literal::String(..) => self.add(span, SEMANTIC_STRING, "string literal"),
            ast::Literal::Boolean(..) => self.add(span, SEMANTIC_KEYWORD, "boolean literal"),
            ast::Literal::SizeOf(..) => self.add(span, SEMANTIC_KEYWORD, "sizeof literal"),
            ast::Literal::DefaultValue(typename) => {
                self.add_literal_with_type(
                    span,
                    typename.as_ref(),
                    "sizeof literal",
                    "sizeof typename",
                );
            },
            ast::Literal::TypeInfo(typename) => {
                self.add_literal_with_type(
                    span,
                    typename.as_ref(),
                    "typeinfo literal",
                    "typeinfo typename",
                );
            },
        }
    }

    fn add_typename(&mut self, type_name: &A::TypeName, debug_desc: &str) {
        let Some(span) = type_name.get_span() else {
            return;
        };

        if let Some(token_type) = semantic_hint_to_token_type(type_name.semantic_hint()) {
            self.add(span, token_type, debug_desc);
        }
    }

    // TODO: uses of generally indicate types that should be replaced with typenames in the nodes
    fn add_type(&mut self, ty: &A::Type, span: &Span, debug_desc: &str) {
        if let Some(token_type) = semantic_hint_to_token_type(ty.semantic_hint()) {
            self.add(span, token_type, debug_desc);
        }
    }

    fn add_literal_with_type(
        &mut self,
        lit_span: &Span,
        typename: &A::TypeName,
        lit_desc: &str,
        type_desc: &str,
    ) {
        if let Some(typename_span) = typename.get_span() {
            let (left, right) = lit_span.split(typename_span);
            self.add(&left, SEMANTIC_KEYWORD, lit_desc);
            self.add_typename(typename, type_desc);
            self.add(&right, SEMANTIC_KEYWORD, lit_desc);
        } else {
            self.add(lit_span, SEMANTIC_KEYWORD, lit_desc);
        }
    }

    fn add_raise(&mut self, raise: &ast::Raise<A>) {
        self.add_keyword(&raise.kw_span);
        self.add_expr(&raise.value);
    }

    fn add_exit(&mut self, exit: &ast::Exit<A>) {
        match exit {
            ast::Exit::WithoutValue(a) => self.add_keyword(a.span()),
            ast::Exit::WithValue {
                value_expr,
                exit_kw,
                ..
            } => {
                self.add_keyword(exit_kw);
                self.add_expr(value_expr);
            },
        }
    }

    fn add_block(&mut self, block: &ast::Block<A>) {
        if let Some(unsafe_kw) = &block.unsafe_kw {
            self.add_keyword(unsafe_kw);
        }
        self.add_keyword(&block.begin);

        for stmt in &block.stmts {
            self.add_stmt(stmt);
        }

        if let Some(expr) = &block.output {
            self.add_expr(expr);
        }

        self.add_keyword(&block.end);
    }

    fn add_tags(&mut self, tags: &[ast::tag::Tag<A>]) {
        for tag in tags {
            for item in &tag.items {
                self.add_typename(&item.tag_type, "tag typename");

                self.add_object_ctor_args(&item.args)
            }
        }
    }

    fn add_func_decl(&mut self, decl: &ast::FunctionDecl<A>, name_type: u32) {
        self.add_tags(&decl.tags);

        self.add_keyword(&decl.kw_span);

        if let Some(name_span) = decl.name.owning_type_name_span() {
            let hint = decl.name.owning_type_name_semantic_hint();

            if let Some(token_type) = semantic_hint_to_token_type(hint) {
                let desc = "function owning type qualification";
                self.add(name_span, token_type, desc);
            }
        }

        for i in 0..decl.name.owning_type_params_len() {
            let desc = "function owning type parameter";
            self.add(decl.name.owning_type_param_span(i), SEMANTIC_TYPE_PARAMETER, desc);
        }

        self.add(&decl.name.ident().span, name_type, "function name");

        let type_param_desc = "function type parameter";
        for i in 0..decl.name.type_params_len() {
            self.add(decl.name.type_param_span(i), SEMANTIC_TYPE_PARAMETER, type_param_desc);
        }

        for param in &decl.params {
            if let Some(modifier) = &param.modifier {
                self.add_keyword(&modifier.span);
            }
            
            if let Some(param_span) = param.get_span() {
                self.add(param_span, SEMANTIC_PARAMETER, "function parameter name");
            }

            if let Some(ty_span) = &param.ty_span {
                self.add_type(&param.ty, ty_span, "function parameter type");
            }
        }

        self.add_typename(&decl.result_ty, "function result type");

        if let Some(clause) = &decl.where_clause {
            self.add_where_clause(clause);
        }

        for decl_mod in &decl.mods {
            self.add_keyword(decl_mod.keyword_span());
            if let Some(arg) = decl_mod.arg() {
                self.add_expr(arg.as_expr());
            }
        }
    }

    fn add_func_def(&mut self, def: &ast::FunctionDef<A>, name_type: u32) {
        self.add_func_decl(&def.decl, name_type);

        self.add_block(&def.body);
    }

    fn add_object_ctor(&mut self, ctor: &ast::ObjectCtor<A>) {
        if let Some(expr) = &ctor.type_expr {
            self.add_expr(expr);
        }

        if let Some(args) = &ctor.type_args {
            self.add_type_arg_list(args);
        }

        self.add_object_ctor_args(&ctor.args);
    }

    fn add_object_ctor_args(&mut self, args: &ast::ObjectCtorArgs<A>) {
        for item in &args.members {
            self.add(
                &item.ident.span,
                SEMANTIC_PROPERTY,
                "object constructor member name",
            );
            self.add_expr(&item.value);
        }
    }
}

fn semantic_hint_to_token_type(semantic_hint: SemanticHint) -> Option<u32> {
    let token_type = match semantic_hint {
        SemanticHint::None => return None,
        SemanticHint::Variable => SEMANTIC_VARIABLE,
        SemanticHint::Function => SEMANTIC_FUNCTION,
        SemanticHint::Method => SEMANTIC_METHOD,
        SemanticHint::Const => SEMANTIC_NAMESPACE,
        SemanticHint::Type => SEMANTIC_TYPE,
        SemanticHint::VariantCase => SEMANTIC_ENUM_MEMBER,
        SemanticHint::Namespace => SEMANTIC_NAMESPACE,
        SemanticHint::Property => SEMANTIC_PROPERTY,
        SemanticHint::String => SEMANTIC_STRING,
        SemanticHint::Parameter => SEMANTIC_PARAMETER,
        SemanticHint::Enum | SemanticHint::Variant => SEMANTIC_ENUM,
        SemanticHint::TypeParameter => SEMANTIC_TYPE_PARAMETER,
        SemanticHint::Number => SEMANTIC_NUMBER,
    };

    Some(token_type)
}
