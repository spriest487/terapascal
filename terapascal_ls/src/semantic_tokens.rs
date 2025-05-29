use terapascal_common::span::Span;
use terapascal_frontend::DelimiterPair;
use terapascal_frontend::TokenTree;
use terapascal_frontend::ast;
use terapascal_frontend::ast::{Annotation, ConstExprValue};
use terapascal_frontend::ast::FunctionName;
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

    pub fn add_token_tree(&mut self, tt: &TokenTree) {
        match tt {
            TokenTree::IntNumber { span, .. } | TokenTree::RealNumber { span, .. } => {
                self.add(span, SEMANTIC_NUMBER);
            },
            TokenTree::String { span, .. } => {
                self.add(span, SEMANTIC_STRING);
            },

            TokenTree::Operator { op, span } if op.is_keyword() => {
                self.add(span, SEMANTIC_OPERATOR);
            },

            TokenTree::Separator { span, .. } | TokenTree::Operator { span, .. } => {
                self.add(span, SEMANTIC_OPERATOR);
            },

            TokenTree::Keyword { span, .. } => {
                self.add(span, SEMANTIC_KEYWORD);
            },

            TokenTree::Delimited(group) => {
                let delim_type = match group.delim {
                    DelimiterPair::BeginEnd | DelimiterPair::CaseEnd | DelimiterPair::MatchEnd => {
                        SEMANTIC_KEYWORD
                    },

                    DelimiterPair::Bracket | DelimiterPair::SquareBracket => SEMANTIC_OPERATOR,
                };

                self.add(&group.open, delim_type);

                for inner in &group.inner {
                    self.add_token_tree(inner);
                }

                self.add(&group.close, delim_type);
            },

            _ => {},
        }
    }

    pub fn add_unit(&mut self, unit: &ast::Unit) {
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
    }

    fn add_unit_decl(&mut self, decl: &ast::UnitDecl) {
        match decl {
            ast::UnitDecl::FunctionDecl { decl } => self.add_func_decl(decl),
            ast::UnitDecl::FunctionDef { def } => self.add_func_def(def),

            ast::UnitDecl::Type { .. } => {},
            ast::UnitDecl::Uses { .. } => {},
            ast::UnitDecl::Binding { decl } => {
                self.add(&decl.kw_span, SEMANTIC_KEYWORD);
                for item in &decl.items {
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
            },
        }
    }

    fn add_stmt<A: Annotation>(&mut self, stmt: &ast::Stmt<A>) {
        match stmt {
            ast::Stmt::Ident(_, _) => {},
            ast::Stmt::LocalBinding(_) => {},
            ast::Stmt::Call(_) => {},
            ast::Stmt::Exit(_) => {},
            ast::Stmt::Block(block) => self.add_block(block),
            ast::Stmt::ForLoop(_) => {},
            ast::Stmt::WhileLoop(_) => {},
            ast::Stmt::Assignment(_) => {},
            ast::Stmt::CompoundAssignment(_) => {},
            ast::Stmt::If(_) => {},
            ast::Stmt::Break(_) => {},
            ast::Stmt::Continue(_) => {},
            ast::Stmt::Raise(_) => {},
            ast::Stmt::Case(_) => {},
            ast::Stmt::Match(_match_stmt) => {},
        }
    }

    fn add_expr<A: Annotation>(&mut self, expr: &ast::Expr<A>) {
        match expr {
            ast::Expr::BinOp(_) => {},
            ast::Expr::UnaryOp(_) => {},
            ast::Expr::Literal(_) => {},
            ast::Expr::Ident(_, _) => {},
            ast::Expr::Call(_) => {},
            ast::Expr::ObjectCtor(_) => {},
            ast::Expr::CollectionCtor(_) => {},
            ast::Expr::IfCond(_) => {},
            ast::Expr::Block(block) => self.add_block(block),
            ast::Expr::Raise(_) => {},
            ast::Expr::Exit(_) => {},
            ast::Expr::Case(_) => {},
            ast::Expr::Match(_) => {},
            ast::Expr::Cast(_) => {},
            ast::Expr::AnonymousFunction(_) => {},
            ast::Expr::ExplicitSpec(_) => {},
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

    fn add_func_decl<A: Annotation>(&mut self, decl: &ast::FunctionDecl<A>) {
        self.add(&decl.kw_span, SEMANTIC_KEYWORD);

        if let Some(name_span) = decl.name.owning_type_name_span() {
            self.add(name_span, SEMANTIC_TYPE);
        }

        for i in 0..decl.name.owning_type_params_len() {
            self.add(decl.name.owning_type_param_span(i), SEMANTIC_TYPE_PARAMETER);
        }

        self.add(&decl.name.ident().span, SEMANTIC_FUNCTION);

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

        for decl_mod in &decl.mods {
            self.add(decl_mod.keyword_span(), SEMANTIC_KEYWORD);
            if let Some(arg) = decl_mod.arg() {
                self.add_expr(arg.as_expr());
            }
        }
    }

    fn add_func_def(&mut self, def: &ast::FunctionDef) {
        self.add_func_decl(&def.decl);

        self.add_block(&def.body);
    }
}
