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
use terapascal_frontend::ast::SetDeclRange;
use terapascal_frontend::ast::StructDecl;
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

pub struct SemanticTokenBuilder {
    cur_line: u32,
    cur_col: u32,

    result: Vec<SemanticToken>,
}

impl SemanticTokenBuilder {
    pub fn new(line: u32, col: u32) -> Self {
        Self {
            cur_line: line,
            cur_col: col,
            result: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<SemanticToken> {
        self.result
    }

    fn token_delta(&mut self, span: &Span) -> (u32, u32) {
        let line = span.start.line as u32;
        let delta_line = line.saturating_sub(self.cur_line);
        if line != self.cur_line {
            self.cur_line = line;
            self.cur_col = 0;
        }

        let col = span.start.col as u32;
        let delta_col = col.saturating_sub(self.cur_col);
        self.cur_col = col;

        (delta_line, delta_col)
    }

    fn add(&mut self, span: &Span, token_type: u32) {
        let (delta_line, delta_col) = self.token_delta(span);

        self.result.push(SemanticToken {
            delta_line,
            delta_start: delta_col,
            length: span.end.col.saturating_sub(span.start.col) as u32 + 1,
            token_type,
            token_modifiers_bitset: 0,
        });
    }

    pub fn add_unit<A: Annotation>(&mut self, unit: &ast::Unit<A>) {
        if let Some(unit_kw) = &unit.unit_kw {
            self.add(unit_kw, SEMANTIC_KEYWORD);
        }

        self.add(&unit.ident.path_span(), SEMANTIC_NAMESPACE);

        if let Some(iface_kw) = &unit.iface_kw {
            self.add(iface_kw, SEMANTIC_KEYWORD);
        }

        for decl in &unit.iface_decls {
            self.add_unit_decl(decl);
        }

        if let Some(impl_kw) = &unit.impl_kw {
            self.add(impl_kw, SEMANTIC_KEYWORD);
        }

        for decl in &unit.impl_decls {
            self.add_unit_decl(decl);
        }

        if let Some(init_block) = &unit.init {
            self.add(&init_block.keyword_span, SEMANTIC_KEYWORD);

            for stmt in &init_block.body {
                self.add_stmt(stmt);
            }

            self.add(&init_block.end_span, SEMANTIC_KEYWORD);
        }
        
        if let Some(span) = &unit.end_kw {
            self.add(span, SEMANTIC_KEYWORD);
        }
    }

    fn add_unit_decl<A: Annotation>(&mut self, decl: &ast::UnitDecl<A>) {
        match decl {
            ast::UnitDecl::FunctionDecl { decl } => self.add_func_decl(decl, SEMANTIC_FUNCTION),
            ast::UnitDecl::FunctionDef { def } => self.add_func_def(def, SEMANTIC_FUNCTION),

            ast::UnitDecl::Type { decl } => self.add_type_decl(decl),
            ast::UnitDecl::Uses { .. } => {},
            ast::UnitDecl::Binding { decl } => self.add_unit_binding(decl),
        }
    }

    fn add_type_decl<A: Annotation>(&mut self, type_decl: &ast::TypeDecl<A>) {
        self.add(&type_decl.kw_span, SEMANTIC_KEYWORD);

        for item in &type_decl.items {
            self.add_tags(item.tags());

            let item_name = item.name();
            self.add(item_name.ident().span(), match item {
                TypeDeclItem::Enum(..) => SEMANTIC_ENUM,
                TypeDeclItem::Set(..) => SEMANTIC_NUMBER,
                _ => SEMANTIC_TYPE,
            });

            for i in 0..item_name.type_params_len() {
                if let Some(param_span) = item_name.type_param_name_span(i) {
                    self.add(param_span, SEMANTIC_TYPE_PARAMETER);
                }
            }

            match item {
                TypeDeclItem::Struct(struct_decl) => {
                    self.add_struct_decl(struct_decl);
                },
                TypeDeclItem::Interface(iface_decl) => self.add_iface_decl(iface_decl),

                TypeDeclItem::Variant(variant_decl) => {
                    self.add_variant_decl(variant_decl);
                },

                TypeDeclItem::Alias(aliased) => {
                    self.add(&aliased.ty_span, SEMANTIC_TYPE);
                },

                TypeDeclItem::Enum(enum_decl) => self.add_enum_decl(enum_decl),
                TypeDeclItem::Set(set_decl) => self.add_set_decl(set_decl),
            }
        }
    }

    fn add_struct_decl<A: Annotation>(&mut self, struct_decl: &StructDecl<A>) {
        self.add(&struct_decl.kw_span, SEMANTIC_KEYWORD);
        
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
            self.add(span, SEMANTIC_KEYWORD);
        }
    }

    fn add_variant_decl<A: Annotation>(&mut self, variant_decl: &VariantDecl<A>) {
        self.add(&variant_decl.kw_span, SEMANTIC_KEYWORD);

        if let Some(supers) = &variant_decl.implements {
            self.add_supers_clause(supers);
        }

        if let Some(clause) = &variant_decl.where_clause {
            self.add_where_clause(clause);
        }

        for case in &variant_decl.cases {
            self.add(&case.ident.span, SEMANTIC_ENUM_MEMBER);

            if let Some(data) = &case.data {
                self.add(&data.span, SEMANTIC_TYPE);
            }
        }

        for section in &variant_decl.sections {
            self.add_type_decl_section(section);
        }

        if let Some(span) = &variant_decl.end_kw_span {
            self.add(span, SEMANTIC_KEYWORD);
        }
    }

    fn add_iface_decl<A: Annotation>(&mut self, iface_decl: &InterfaceDecl<A>) {
        self.add(&iface_decl.kw_span, SEMANTIC_KEYWORD);
        
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
            self.add(span, SEMANTIC_KEYWORD);
        }
    }

    fn add_enum_decl<A: Annotation>(&mut self, enum_decl: &ast::EnumDecl<A>) {
        for item in &enum_decl.items {
            self.add(&item.ident.span, SEMANTIC_ENUM_MEMBER);
            if let Some(val) = &item.value {
                self.add(val.span(), SEMANTIC_NUMBER);
            }
        }
    }

    fn add_set_decl<A: Annotation>(&mut self, set_decl: &ast::SetDecl<A>) {
        match set_decl.range.as_ref() {
            SetDeclRange::Type { span, .. } => {
                self.add(span, SEMANTIC_TYPE);
            },
            SetDeclRange::Range {
                from,
                to,
                range_op_span,
                ..
            } => {
                self.add_expr(from);
                self.add(range_op_span, SEMANTIC_OPERATOR);
                self.add_expr(to);
            },
        }
    }

    fn add_type_decl_section<A: Annotation>(&mut self, section: &impl MemberDeclSection<A>) {
        if let Some(kw_span) = &section.access_kw_span() {
            self.add(kw_span, SEMANTIC_KEYWORD);
        }

        for member in section.members() {
            match member {
                TypeMemberDeclRef::Field(field) => {
                    for ident in &field.idents {
                        self.add(ident.span(), SEMANTIC_PROPERTY);
                    }
                    self.add(&field.ty_span, SEMANTIC_TYPE);
                },

                TypeMemberDeclRef::Method(decl) => {
                    self.add_func_decl(&decl.func_decl, SEMANTIC_METHOD);
                },
            }
        }
    }

    fn add_where_clause<A: Annotation>(&mut self, clause: &ast::WhereClause<A>) {
        self.add(&clause.where_kw_span, SEMANTIC_KEYWORD);

        for constraint in &clause.constraints {
            self.add(&constraint.name.span, SEMANTIC_TYPE_PARAMETER);

            if let Some(span) = &constraint.is_kw_span {
                self.add(span, SEMANTIC_KEYWORD);
            }

            if let Some(span) = &constraint.is_ty_span {
                self.add(span, SEMANTIC_TYPE);
            }
        }
    }

    fn add_supers_clause<A: Annotation>(&mut self, clause: &ast::SupersClause<A>) {
        self.add(&clause.kw_span, SEMANTIC_KEYWORD);
        
        for ty in &clause.types {
            if let Some(span) = ty.get_span() {
                self.add(span, SEMANTIC_TYPE);
            }
        }
    }

    fn add_unit_binding<A: Annotation>(&mut self, binding: &ast::UnitBinding<A>) {
        self.add(&binding.kw_span, SEMANTIC_KEYWORD);

        for item in &binding.items {
            for ident in &item.idents {
                self.add(&ident.span, SEMANTIC_VARIABLE);
            }

            if let Some(span) = &item.ty_span {
                self.add(span, SEMANTIC_TYPE);
            }

            if let Some(init) = &item.init {
                self.add(&init.eq_span, SEMANTIC_OPERATOR);
                self.add_expr(&init.expr);
            }
        }
    }

    fn add_stmt<A: Annotation>(&mut self, stmt: &ast::Stmt<A>) {
        match stmt {
            ast::Stmt::Ident(_, value) => self.add_value(value),
            ast::Stmt::LocalBinding(binding) => self.add_local_binding(binding),
            ast::Stmt::Call(call) => self.add_call(call),
            ast::Stmt::Exit(exit) => self.add_exit(exit),
            ast::Stmt::Block(block) => self.add_block(block),
            ast::Stmt::ForLoop(for_loop) => self.add_for(for_loop),
            ast::Stmt::WhileLoop(while_loop) => self.add_while(while_loop),
            ast::Stmt::Assignment(assignment) => self.add_assignment(assignment),
            ast::Stmt::CompoundAssignment(assignment) => self.add_compound_assignment(assignment),
            ast::Stmt::If(if_cond) => self.add_if_cond(if_cond, Self::add_stmt),
            ast::Stmt::Break(a) | ast::Stmt::Continue(a) => self.add(a.span(), SEMANTIC_KEYWORD),
            ast::Stmt::Raise(raise) => self.add_raise(raise),
            ast::Stmt::Case(block) => self.add_case_block(block, Self::add_stmt),
            ast::Stmt::Match(block) => self.add_match_block(block, Self::add_stmt),
        }
    }

    fn add_assignment<A: Annotation>(&mut self, assignment: &ast::Assignment<A>) {
        self.add_expr(&assignment.lhs);
        self.add(&assignment.op_span, SEMANTIC_OPERATOR);
        self.add_expr(&assignment.rhs);
    }

    fn add_compound_assignment<A: Annotation>(&mut self, assignment: &ast::CompoundAssignment<A>) {
        self.add_expr(&assignment.lhs);
        self.add(&assignment.op_span, SEMANTIC_OPERATOR);
        self.add_expr(&assignment.rhs);
    }

    fn add_local_binding<A: Annotation>(&mut self, binding: &ast::LocalBinding<A>) {
        self.add(&binding.kw_span, SEMANTIC_KEYWORD);
        
        self.add(&binding.name.span, SEMANTIC_VARIABLE);

        if let Some(span) = binding.ty.get_span() {
            self.add(span, SEMANTIC_TYPE);
        }
        
        if let Some(span) = &binding.assign_op_span {
            self.add(span, SEMANTIC_OPERATOR);
        }

        if let Some(expr) = &binding.val {
            self.add_expr(expr);
        }
    }

    fn add_if_cond<A: Annotation, B: Spanned, BranchFn>(
        &mut self,
        if_cond: &ast::IfCond<A, B>,
        add_branch: BranchFn,
    ) where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add(&if_cond.if_kw_span, SEMANTIC_KEYWORD);
        self.add_expr(&if_cond.cond);
        
        if let Some(span) = &if_cond.is_kw {
            self.add(span, SEMANTIC_KEYWORD);
        }
        
        if let Some(pattern) = &if_cond.is_pattern {
            self.add_pattern::<A>(pattern);
        }

        self.add(&if_cond.then_kw_span, SEMANTIC_KEYWORD);
        add_branch(self, &if_cond.then_branch);

        if let Some(branch) = &if_cond.else_branch {
            self.add(&branch.else_kw_span, SEMANTIC_KEYWORD);
            add_branch(self, &branch.item);
        }
    }

    fn add_case_block<A: Annotation, B, BranchFn>(
        &mut self,
        block: &ast::CaseBlock<A, B>,
        add_branch: BranchFn,
    ) where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add(&block.kw_span, SEMANTIC_KEYWORD);
        self.add_expr(&block.cond_expr);
        self.add(&block.of_span, SEMANTIC_KEYWORD);
        
        for branch in &block.branches {
            for val in &branch.case_values {
                self.add_expr(val);
            }
            add_branch(self, &branch.item);
        }
        
        if let Some(branch) = &block.else_branch {
            self.add(&branch.else_kw_span, SEMANTIC_KEYWORD);
            add_branch(self, &branch.item);
        }
        
        self.add(&block.end_span, SEMANTIC_KEYWORD);
    }

    fn add_match_block<A: Annotation, B, BranchFn>(
        &mut self,
        block: &ast::MatchBlock<A, B>,
        add_branch: BranchFn,
    ) where
        BranchFn: Fn(&mut Self, &B),
    {
        self.add(&block.kw_span, SEMANTIC_KEYWORD);
        self.add_expr(&block.cond_expr);
        self.add(&block.of_span, SEMANTIC_KEYWORD);

        for branch in &block.branches {
            self.add_pattern::<A>(&branch.pattern);
            add_branch(self, &branch.item);
        }

        if let Some(branch) = &block.else_branch {
            self.add(&branch.else_kw_span, SEMANTIC_KEYWORD);
            add_branch(self, &branch.item);
        }

        self.add(&block.end_span, SEMANTIC_KEYWORD);
    }

    fn add_pattern<A: Annotation>(&mut self, pattern: &A::Pattern) {
        for element in pattern.semantic_elements() {
            match element {
                PatternSemanticElement::Keyword(span) => self.add(&span, SEMANTIC_KEYWORD),
                PatternSemanticElement::Binding(span) => self.add(&span, SEMANTIC_VARIABLE),
                PatternSemanticElement::Type(ty) => {
                    if let Some(span) = ty.get_span() {
                        self.add(span, SEMANTIC_TYPE)
                    }
                },
                PatternSemanticElement::VariantCase(span) => self.add(&span, SEMANTIC_ENUM_MEMBER),
                PatternSemanticElement::Path(span) => self.add(&span, SEMANTIC_TYPE),
            }
        }
    }
    
    fn add_for<A: Annotation>(&mut self, for_loop: &ast::ForLoop<A>) {
        self.add(&for_loop.for_kw_span, SEMANTIC_KEYWORD);

        match &for_loop.range {
            ast::ForLoopRange::UpTo(range) => {
                match &range.init {
                    ast::ForLoopCounterInit::Binding {
                        binding_kw_span,
                        name: _,
                        ty,
                        assign_op_span,
                        init,
                    } => {
                        self.add(binding_kw_span, SEMANTIC_KEYWORD);
                        if let Some(span) = ty.get_span() {
                            self.add(span, SEMANTIC_TYPE);
                        }
                        self.add(assign_op_span, SEMANTIC_OPERATOR);
                        self.add_expr(init);
                    },

                    ast::ForLoopCounterInit::Assignment {
                        counter,
                        assign_op_span,
                        value,
                    } => {
                        self.add_expr(counter);
                        self.add(assign_op_span, SEMANTIC_OPERATOR);
                        self.add_expr(value);
                    },
                }
                
                self.add(&range.to_kw_span, SEMANTIC_KEYWORD);
                self.add_expr(&range.to_expr);
            },

            ast::ForLoopRange::InSequence(range) => {
                self.add(&range.binding_kw_span, SEMANTIC_KEYWORD);
                self.add(&range.binding_name.span, SEMANTIC_VARIABLE);
                if let Some(span) = range.binding_ty.get_span() {
                    self.add(span, SEMANTIC_TYPE);
                }
                self.add(&range.in_kw_span, SEMANTIC_KEYWORD);
                self.add_expr(&range.src_expr);
            },
        }

        self.add(&for_loop.do_kw_span, SEMANTIC_KEYWORD);
        self.add_stmt(&for_loop.body);
    }

    fn add_while<A: Annotation>(&mut self, while_loop: &ast::WhileLoop<A>) {
        self.add(&while_loop.while_kw_span, SEMANTIC_KEYWORD);
        self.add_expr(&while_loop.condition);
        self.add(&while_loop.do_kw_span, SEMANTIC_KEYWORD);
        self.add_stmt(&while_loop.body);
    }
    
    fn add_call<A: Annotation>(&mut self, call: &ast::Call<A>) {
        if let Some(target_expr) = call.target_expr() {
            self.add_expr(target_expr);
        } else {
            if let Some(span) = call.type_qualification_span() {
                self.add(span, SEMANTIC_TYPE);
            }

            if let Some(span) = call.method_name_span() {
                self.add(span, SEMANTIC_METHOD);
            }
        }
        
        if let Some(args) = call.type_args() {
            self.add_type_arg_list::<A>(args);
        }
        
        for arg in call.args() {
            self.add_expr(arg);
        }
    }
    
    fn add_type_arg_list<A: Annotation>(&mut self, args: &ast::TypeArgList<A>) {
        for arg in &args.items {
            if let Some(span) = arg.get_span() {
                self.add(span, SEMANTIC_TYPE);
            }
        }
    }

    fn add_expr<A: Annotation>(&mut self, expr: &ast::Expr<A>) {
        match expr {
            ast::Expr::BinOp(op) => self.add_bin_op(op),
            ast::Expr::UnaryOp(op) => self.add_unary_op(op),
            ast::Expr::Literal(item) => self.add_literal(item),
            ast::Expr::Ident(_, value) => self.add_value(value),
            ast::Expr::Call(call) => self.add_call(call),
            ast::Expr::ObjectCtor(ctor) => self.add_object_ctor(ctor),
            ast::Expr::CollectionCtor(_) => {},
            ast::Expr::IfCond(if_cond) => self.add_if_cond(if_cond, Self::add_expr),
            ast::Expr::Block(block) => self.add_block(block),
            ast::Expr::Raise(raise) => self.add_raise(raise),
            ast::Expr::Exit(exit) => self.add_exit(exit),
            ast::Expr::Case(block) => self.add_case_block(block, Self::add_expr),
            ast::Expr::Match(block) => self.add_match_block(block, Self::add_expr),
            ast::Expr::Cast(_) => {},
            ast::Expr::AnonymousFunction(_) => {},
            ast::Expr::ExplicitSpec(_) => {},
        }
    }
    
    fn add_value<A: Annotation>(&mut self, _value: &A) {
        
    }

    fn add_bin_op<A: Annotation>(&mut self, bin_op: &ast::BinOp<A>) {
        self.add_expr(&bin_op.lhs);
        
        let op_semantic = if bin_op.op.is_keyword() {
            SEMANTIC_KEYWORD  
        } else {
            SEMANTIC_OPERATOR
        };

        if bin_op.op == Operator::Index {
            let (left_span, right_span) = &bin_op.op_span.split(&bin_op.rhs);

            self.add(&left_span, op_semantic);
            self.add_expr(&bin_op.rhs);
            self.add(&right_span, op_semantic);
        } else {
            self.add(&bin_op.op_span, op_semantic);
            self.add_expr(&bin_op.rhs);
        }
    }

    fn add_unary_op<A: Annotation>(&mut self, unary_op: &ast::UnaryOp<A>) {
        let op_semantic = if unary_op.op.is_keyword() {
            SEMANTIC_KEYWORD
        } else {
            SEMANTIC_OPERATOR
        };

        match unary_op.pos {
            UnaryPosition::Prefix => {
                self.add(&unary_op.op_span, op_semantic);
                self.add_expr(&unary_op.operand);
            },
            UnaryPosition::Postfix => {
                self.add_expr(&unary_op.operand);
                self.add(&unary_op.op_span, op_semantic);
            },
        }
    }

    fn add_literal<A: Annotation>(&mut self, item: &ast::LiteralItem<A>) {
        let span = item.annotation.span();

        match item.literal {
            ast::Literal::Nil => self.add(span, SEMANTIC_KEYWORD),
            ast::Literal::Integer(..) | ast::Literal::Real(_) => self.add(span, SEMANTIC_NUMBER),
            ast::Literal::String(..) => self.add(span, SEMANTIC_STRING),
            ast::Literal::Boolean(..) => self.add(span, SEMANTIC_KEYWORD),
            ast::Literal::SizeOf(..) => self.add(span, SEMANTIC_KEYWORD),
            ast::Literal::DefaultValue(..) => self.add(span, SEMANTIC_KEYWORD), // TODO type is separate
            ast::Literal::TypeInfo(..) => self.add(span, SEMANTIC_TYPE),
        }
    }

    fn add_raise<A: Annotation>(&mut self, raise: &ast::Raise<A>) {
        self.add(&raise.kw_span, SEMANTIC_KEYWORD);
        self.add_expr(&raise.value);
    }

    fn add_exit<A: Annotation>(&mut self, exit: &ast::Exit<A>) {
        match exit {
            ast::Exit::WithoutValue(a) => self.add(a.span(), SEMANTIC_KEYWORD),
            ast::Exit::WithValue {
                value_expr,
                exit_kw,
                ..
            } => {
                self.add(exit_kw, SEMANTIC_KEYWORD);
                self.add_expr(value_expr);
            },
        }
    }

    fn add_block<A: Annotation>(&mut self, block: &ast::Block<A>) {
        if let Some(unsafe_kw) = &block.unsafe_kw {
            self.add(unsafe_kw, SEMANTIC_KEYWORD);
        }
        self.add(&block.begin, SEMANTIC_KEYWORD);

        for stmt in &block.stmts {
            self.add_stmt(stmt);
        }

        if let Some(expr) = &block.output {
            self.add_expr(expr);
        }

        self.add(&block.end, SEMANTIC_KEYWORD);
    }

    fn add_tags<A: Annotation>(&mut self, tags: &[ast::tag::Tag<A>]) {
        for tag in tags {
            for item in &tag.items {
                self.add_object_ctor_args(&item.args)
            }
        }
    }

    fn add_func_decl<A: Annotation>(&mut self, decl: &ast::FunctionDecl<A>, name_type: u32) {
        self.add_tags(&decl.tags);

        self.add(&decl.kw_span, SEMANTIC_KEYWORD);

        if let Some(name_span) = decl.name.owning_type_name_span() {
            self.add(name_span, SEMANTIC_TYPE);
        }

        for i in 0..decl.name.owning_type_params_len() {
            self.add(decl.name.owning_type_param_span(i), SEMANTIC_TYPE_PARAMETER);
        }

        self.add(&decl.name.ident().span, name_type);

        for i in 0..decl.name.type_params_len() {
            self.add(decl.name.type_param_span(i), SEMANTIC_TYPE_PARAMETER);
        }

        for param in &decl.params {
            self.add(&param.ident.span, SEMANTIC_PARAMETER);
            if let Some(ty_span) = &param.ty_span {
                self.add(ty_span, SEMANTIC_TYPE);
            }
        }

        if let Some(result_ty_span) = &decl.result_ty_span {
            self.add(&result_ty_span, SEMANTIC_TYPE);
        }

        if let Some(clause) = &decl.where_clause {
            self.add_where_clause(clause);
        }

        for decl_mod in &decl.mods {
            self.add(decl_mod.keyword_span(), SEMANTIC_KEYWORD);
            if let Some(arg) = decl_mod.arg() {
                self.add_expr(arg.as_expr());
            }
        }
    }

    fn add_func_def<A: Annotation>(&mut self, def: &ast::FunctionDef<A>, name_type: u32) {
        self.add_func_decl(&def.decl, name_type);

        self.add_block(&def.body);
    }

    fn add_object_ctor<A: Annotation>(&mut self, ctor: &ast::ObjectCtor<A>) {
        if let Some(expr) = &ctor.type_expr {
            self.add(expr.span(), SEMANTIC_TYPE);
        }

        if let Some(args) = &ctor.type_args {
            self.add_type_arg_list::<A>(args);
        }

        self.add_object_ctor_args(&ctor.args);
    }

    fn add_object_ctor_args<A: Annotation>(&mut self, args: &ast::ObjectCtorArgs<A>) {
        for item in &args.members {
            self.add(&item.ident.span, SEMANTIC_PROPERTY);
            self.add_expr(&item.value);
        }
    }
}
