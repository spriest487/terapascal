#[cfg(test)]
mod test;

use crate::ast::tag::Tag;
use crate::ast::type_name::TypeName;
use crate::ast::{Annotation, SemanticHint};
use crate::ast::BindingDeclKind;
use crate::ast::Block;
use crate::ast::DeclMod;
use crate::ast::Expr;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::TypeAnnotation;
use crate::ast::TypeList;
use crate::ast::TypePath;
use crate::ast::WhereClause;
use crate::parse::{ContinueParse, Matcher, Parser};
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::{MatchOneOf, TryParse};
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use derivative::*;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::{MaybeSpanned, Span};
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionDeclKind {
    // function or procedure
    Function,
    
    // declared with a preceding `class` keyword, must be within a type decl
    ClassMethod,
    
    // declared with the `constructor` keyword, must not specify a return type
    Constructor,
    
    // declared with the `destructor` keyword, must not specify a return type or
    // any parameters. one per concrete type
    Destructor,
}

impl FunctionDeclKind {
    pub fn is_static_method(self) -> bool {
        matches!(self, FunctionDeclKind::ClassMethod | FunctionDeclKind::Constructor)
    }
    
    pub fn must_be_method(self) -> bool {
        matches!(self, FunctionDeclKind::Destructor 
            | FunctionDeclKind::Constructor 
            | FunctionDeclKind::ClassMethod)
    }
}

impl fmt::Display for FunctionDeclKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionDeclKind::Function => write!(f, "function"),
            FunctionDeclKind::ClassMethod => write!(f, "class method"),
            FunctionDeclKind::Constructor => write!(f, "constructor"),
            FunctionDeclKind::Destructor => write!(f, "destructor"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum FunctionParamMod {
    Var,
    Out,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct FunctionParamModDecl {
    pub param_mod: FunctionParamMod, 
    
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl fmt::Display for FunctionParamMod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FunctionParamMod::Var => "var",
                FunctionParamMod::Out => "out",
            }
        )
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct FunctionParam<A: Annotation = Span> {
    pub name: Arc<String>,
    
    pub ty: A::TypeName,
    
    pub modifier: Option<FunctionParamModDecl>,
    
    // true if this is the implicit `self` parameter of a method
    pub is_implicit_self: bool,

    // may be None for implicit params
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub name_span: Option<Span>,
}

impl<A: Annotation> FunctionParam<A> {
    pub fn get_modifier(&self) -> Option<FunctionParamMod> {
        self.modifier.as_ref().map(|m| m.param_mod)
    }
}


impl<A: Annotation> fmt::Display for FunctionParam<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            write!(f, "{} ", modifier.param_mod)?;
        }
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl<A: Annotation> MaybeSpanned for FunctionParam<A> {
    fn get_span(&self) -> Option<&Span> {
        self.name_span.as_ref()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct FunctionDecl<A: Annotation = Span> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw_span: Span,
    
    pub name: A::FunctionName,
    pub where_clause: Option<WhereClause<A>>,

    pub tags: Vec<Tag<A>>,

    pub kind: FunctionDeclKind,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    pub params: Vec<FunctionParam<A>>,

    pub result_ty: A::TypeName,

    pub mods: Vec<DeclMod<A>>,
}

impl FunctionDecl<Span> {
    pub fn parse(parser: &mut Parser, allow_methods: bool, tags: Vec<Tag>) -> ParseResult<Self> {
        let mut kw_matcher = Keyword::Function | Keyword::Procedure;

        if allow_methods {
            kw_matcher = kw_matcher
                | Keyword::Class
                | Keyword::Constructor
                | Keyword::Destructor;
        }

        let func_kw = parser.match_one(kw_matcher)?;
        
        let (kw_span, kind, expect_result_ty) = match func_kw.as_keyword().unwrap() {
            Keyword::Class => {
                let class_func_kw = parser.match_one(Keyword::Function | Keyword::Procedure)?;

                let kind = FunctionDeclKind::ClassMethod;
                let expect_return = match class_func_kw.as_keyword() {
                    Some(Keyword::Function) => true,
                    Some(Keyword::Procedure) => false,
                    _ => unreachable!(),
                };

                let mut kw_span = func_kw.into_span();
                kw_span.extend(class_func_kw.span());

                (kw_span, kind, expect_return)
            }

            Keyword::Function => (func_kw.into_span(), FunctionDeclKind::Function, true),
            Keyword::Procedure => (func_kw.into_span(), FunctionDeclKind::Function, false),
            Keyword::Constructor => (func_kw.into_span(), FunctionDeclKind::Constructor, false),
            Keyword::Destructor => (func_kw.into_span(), FunctionDeclKind::Destructor, false),
            
            _ => unreachable!("doesn't match matcher"),
        };

        let name = Self::parse_name(parser)?;
        let span = kw_span.to(&name.span());
        
        // from this point on we have enough info to return at least a partial version of this
        // func decl and should return this instead of failing on any subsequent error
        let mut func_decl = FunctionDecl {
            tags,
            kind,
            name,
            kw_span,
            params: Vec::new(),
            mods: Vec::new(),
            where_clause: None,
            result_ty: TypeName::Unspecified,
            span,
        };

        if let Some(params_tt) = parser.match_one_maybe(DelimiterPair::Bracket) {
            let params_group = match params_tt {
                TokenTree::Delimited(group) => group,
                _ => unreachable!(),
            };

            func_decl.span.extend(&params_group.span);

            let mut params_tokens = params_group.into_inner_tokens();
            
            func_decl.params = Self::parse_params(&mut params_tokens)
                .or_continue_with(parser.errors(), Vec::new);

            params_tokens.finish().or_continue(parser.errors(), ());
        }

        if expect_result_ty && parser.match_one_maybe(Separator::Colon).is_some() {
            // look for a result type
            if let Some(ty) = TypeName::parse(parser).ok_or_continue(parser.errors()) {
                if let Some(ty_span) = ty.get_span() {
                    func_decl.span.extend(ty_span);
                }
                func_decl.result_ty = ty;
            }
        }

        func_decl.mods = DeclMod::parse_seq(parser).or_continue_with(parser.errors(), Vec::new);
        if let Some(last_mod) = func_decl.mods.last() {
            func_decl.span.extend(last_mod.span());
        }
        
        func_decl.where_clause = WhereClause::try_parse(parser.tokens())
            .or_continue(parser.errors(), None);

        Ok(func_decl)
    }
    
    fn parse_name(tokens: &mut TokenStream) -> ParseResult<QualifiedFunctionName> {
        let mut instance_ty_path = Vec::new();
        
        // the name always starts with at least one ident
        instance_ty_path.push(Ident::parse(tokens)?);
        
        let mut instance_ty_params = None;
        
        // note this returns a list of type *idents*, because constraints (needed for TypeParams)
        // are parsed later
        let mut type_params = None;
        
        loop {
            // nested types aren't supported, so if we get a type arg list, the next
            // tokens must be the method name and that's the end of the path
            let match_next = if type_params.is_none() || instance_ty_params.is_none() {
                Operator::Period | DelimiterPair::SquareBracket
            } else {
                Matcher::from(Operator::Period)
            };
            
            match tokens.match_one_maybe(match_next) {
                // followed by subsequent path parts for a potentially namespace-qualified type name
                Some(TokenTree::Operator { op: Operator::Period, .. }) => {
                    // if there's a period following the type list, it's actually the type params
                    // for the instance type of this method decl
                    if type_params.is_some() {
                        assert!(instance_ty_params.is_none(), "matcher shouldn't allow this");
                        instance_ty_params = type_params.take();
                    }
                    
                    instance_ty_path.push(Ident::parse(tokens)?);
                },

                // or a type list in a [] group, which can either be a type param list for
                // the instance type of the method, or the type param list of the function itself
                Some(TokenTree::Delimited(DelimitedGroup {
                    delim: DelimiterPair::SquareBracket,
                    inner: type_list_inner,
                    span: type_list_span,
                    ..
                })) => {
                    let mut type_list_tokens = TokenStream::new(type_list_inner, type_list_span.clone());
                    
                    let type_list_items: Vec<Ident> = TypeList::parse_items(&mut type_list_tokens)?;
                    type_list_tokens.finish()?;

                    let type_list = TypeList::new(type_list_items, type_list_span);
                    type_params = Some(type_list);
                },
                
                None => break,
                
                _ => unreachable!("patterns above must match the matchable tokens"),
            }
        }
        
        let name_ident = instance_ty_path.remove(instance_ty_path.len() - 1);
        
        let instance_ty = if instance_ty_path.is_empty() {
            None
        } else {
            let type_name = IdentPath::from_vec(instance_ty_path);
            let name_span = type_name.path_span();
            let type_span = match &instance_ty_params {
                Some(param_list) => name_span.to(param_list),
                None => name_span.clone(),
            };

            Some(Box::new(TypePath {
                span: type_span,
                name: type_name,
                name_span,
                type_params: instance_ty_params,
            }))
        };
        
        let qualified_name = QualifiedFunctionName {
            ident: name_ident,
            owning_ty_qual: instance_ty,
            type_params,
        }; 

        Ok(qualified_name)
    }

    pub fn parse_params(tokens: &mut TokenStream) -> ParseResult<Vec<FunctionParam<Span>>> {
        let mut params = Vec::new();

        let match_mod = Keyword::Var.or(Keyword::Out);
        let match_more = match_mod.clone().or(Matcher::AnyIdent);

        loop {
            if !params.is_empty() && tokens.match_one_maybe(Separator::Semicolon).is_none() {
                break;
            }

            // check if there's another param ahead
            if tokens.look_ahead().match_one(match_more.clone()).is_none() {
                break;
            }

            // might start with a modifier keyword which applies to all params declared in this group
            let modifier = match tokens.match_one_maybe(match_mod.clone()) {
                Some(tt) if tt.is_keyword(Keyword::Var) => Some(FunctionParamModDecl {
                    param_mod: FunctionParamMod::Var,
                    span: tt.into_span(),
                }),
                Some(tt) if tt.is_keyword(Keyword::Out) => Some(FunctionParamModDecl {
                    param_mod: FunctionParamMod::Out,
                    span: tt.into_span(),
                }),
                _ => None,
            };

            // match comma-separated idents for this param type
            let mut idents = Vec::new();
            loop {
                let ident = Ident::parse(tokens)?;
                idents.push(ident);

                if tokens.match_one_maybe(Separator::Comma).is_none() {
                    break;
                }
            }

            tokens.match_one(Separator::Colon)?;
            let ty = TypeName::parse(tokens)?;

            // TODO: the AST should preserve the groups as written
            // right now type spans need to be optional because we only want to include the same
            // span once per group of identifiers when producing semantic tokens, but this makes
            // this code more complicated than it needs to be
            for i in 0..idents.len() {
                let ident = &idents[i];

                params.push(FunctionParam {
                    name_span: Some(ident.span.clone()),
                    ty: ty.clone(),
                    name: ident.name.clone(),
                    is_implicit_self: false,
                    modifier: modifier.clone(),
                })
            }
        }

        Ok(params)
    }
}

impl<A: Annotation> FunctionDecl<A> {
    pub fn ident(&self) -> &Ident {
        self.name.ident()
    }
    
    pub fn external_src(&self) -> Option<&A::ConstStringExpr> {
        self.mods
            .iter()
            .filter_map(|decl_mod| match decl_mod {
                DeclMod::External { src, .. } => Some(src),
                _ => None,
            })
            .next()
    }


    pub fn get_mod(&self, keyword: &str) -> Option<&DeclMod<A>> {
        self.mods
            .iter()
            .find(|decl_mod| decl_mod.keyword() == keyword)
    }
    
    pub fn is_overload(&self) -> bool {
        self.get_mod(DeclMod::<A>::OVERLOAD_WORD).is_some() && self.external_src().is_none()
    }
    
    pub fn type_params_len(&self) -> usize {
        self.name.type_params_len()
    } 
}

impl<A: Annotation> Spanned for FunctionDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FunctionDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ", match self.kind {
            FunctionDeclKind::Function => "function",
            FunctionDeclKind::ClassMethod => "class function",
            FunctionDeclKind::Constructor => "constructor",
            FunctionDeclKind::Destructor => "destructor",
        })?;

        write!(f, "{}", self.name)?;
        
        if !self.params.is_empty() {
            write!(f, "(")?;

            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, "; ")?;
                }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
        }

        if self.result_ty.is_known() 
            && matches!(self.kind, FunctionDeclKind::Function | FunctionDeclKind::ClassMethod) 
        {
            write!(f, ": {}", self.result_ty)?;
        }
        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionLocalBinding<A = Span>
    where A: Annotation
{
    pub kind: BindingDeclKind,

    pub ident: Ident,
    pub ty: A::Type,

    pub initial_val: Option<A::ConstValue>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for FunctionLocalBinding<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl FunctionLocalBinding<Span> {
    // matches the next token of any optional element that wasn't parsed as part of this 
    // otherwise valid decl, e.g. the explicit init of a variable. used for better error messages
    pub fn match_trailing(&self) -> Option<Matcher> {
        match self.initial_val.as_ref() {
            None => Some(Matcher::from(LOCAL_INIT_DECL_OPERATOR)),
            Some(..) => None,
        }
    }
}

// note that function local init is NOT an assignment (:=), we're declaring a value
// that it's already *equal to* at the start of the body
const LOCAL_INIT_DECL_OPERATOR: Operator = Operator::Equals;

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct FunctionDef<A: Annotation = Span> {
    pub decl: Arc<FunctionDecl<A>>,

    pub locals: Vec<FunctionLocalBinding<A>>,

    pub body: Block<A>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionDef<Span> {
    pub fn parse_body_of_decl(
        parser: &mut Parser,
        decl: Arc<FunctionDecl<Span>>,
    ) -> ParseResult<Self> {
        let body_start_matcher = Self::match_body_start();
        
        let mut trailing_semicolon = false;

        let mut locals = Vec::new();
        loop {
            match parser.look_ahead().match_one(body_start_matcher.clone()) {
                Some(tt) if tt.is_keyword(Keyword::Var) => {
                    parser.advance(1);

                    let vars = Self::parse_locals_block(parser, BindingDeclKind::Var)
                        .or_continue_with(parser.errors(), Vec::new);

                    locals.extend(vars);

                    trailing_semicolon = parser.match_one_maybe(Separator::Semicolon).is_some();
                },

                Some(tt) if tt.is_keyword(Keyword::Const) => {
                    parser.advance(1);

                    let consts = Self::parse_locals_block(parser, BindingDeclKind::Const)
                        .or_continue_with(parser.errors(), Vec::new);

                    locals.extend(consts);

                    trailing_semicolon = parser.match_one_maybe(Separator::Semicolon).is_some();
                }

                _ => break,
            }
        }

        let body = Block::parse(parser)
            .map_err(|err| {
                Self::map_unexpected_err_after_locals(err, trailing_semicolon, &locals)
            })?;

        let span = decl.span.to(body.span());

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span,
        })
    }
    
    fn map_unexpected_err_after_locals(
        err: TracedError<ParseError>,
        trailing_semicolon: bool,
        locals: &[FunctionLocalBinding<Span>]) -> TracedError<ParseError> {       
        err.map(|err| match err {
            ParseError::UnexpectedToken(tt, expected) if !trailing_semicolon => {
                let trailing_match = Self::match_trailing_for_locals(&locals);

                ParseError::UnexpectedToken(tt, match (expected, trailing_match) {
                    (Some(expected), Some(trailing)) => Some(expected.or(trailing)),
                    (Some(expected), None) => Some(expected),
                    (None, Some(trailing)) => Some(trailing),
                    (None, trailing) => trailing,
                })
            }

            _ => err,
        })
    }
    
    fn match_trailing_for_locals(locals: &[FunctionLocalBinding<Span>]) -> Option<Matcher> {
        let last_local = locals.last()?;
        last_local.match_trailing()
    }

    fn parse_locals_block(tokens: &mut TokenStream, kind: BindingDeclKind) -> ParseResult<Vec<FunctionLocalBinding<Span>>> {
        let mut decls = Vec::new();
        loop {
            // separator from previous item
            if !decls.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            // ident list, at least one ident, separated by commas
            let first_ident = Ident::parse(tokens)?;
            let mut idents = vec![first_ident];
            while tokens.match_one_maybe(Separator::Comma).is_some() {
                let ident = Ident::parse(tokens)?;
                idents.push(ident);
            }

            // if there's a colon following the names, expect an explicit type name to follow
            let ty = match tokens.match_one_maybe(Separator::Colon) {
                None => TypeName::Unspecified,
                Some(..) => TypeName::parse(tokens)?,
            };

            // equals operator indicates there's a default value
            let initial_val = match tokens.match_one_maybe(LOCAL_INIT_DECL_OPERATOR) {
                Some(..) => {
                    let expr = Expr::parse(tokens)?;
                    Some(Box::new(expr))
                },
                None => None,
            };

            decls.extend(idents.into_iter()
                .map(|ident| FunctionLocalBinding {
                    span: ident.span.clone(),
                    kind,
                    ident,
                    ty: ty.clone(),
                    initial_val: initial_val.clone(),
                }));

            let mut look_ahead = tokens.look_ahead();
            if !decls.is_empty() && look_ahead.match_one(Separator::Semicolon).is_none() {
                break;
            }
            if look_ahead.match_one(Matcher::AnyIdent).is_none() {
                break;
            }
        }

        Ok(decls)
    }

    pub fn match_body_start() -> Matcher {
        Keyword::Unsafe
            .or(DelimiterPair::BeginEnd)
            .or(Keyword::Var)
            .or(Keyword::Const)
    }
}

impl<A: Annotation> Spanned for FunctionDef<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FunctionDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.decl)?;

        let mut last_local_kind = None;
        for local in &self.locals {
            if Some(local.kind) != last_local_kind {
                write!(f, "{}", local.kind)?;
                last_local_kind = Some(local.kind);
            }

            write!(f, "  {}: {}", local.ident, local.ty)?;
            if let Some(initial_val) = &local.initial_val {
                write!(f, " = {}", initial_val)?;
            }
            writeln!(f, ";")?;
        }

        write!(f, "{}", self.body)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AnonymousFunctionDef<A: Annotation> {
    pub annotation: A,

    pub params: Vec<FunctionParam<A>>,
    pub return_ty: A::Type,

    pub body: Block<A>,
    pub captures: LinkedHashMap<Ident, A::Type>,
}

impl<A: Annotation> fmt::Display for AnonymousFunctionDef<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")?;

        if !self.params.is_empty() {
            write!(f, "(")?;

            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ";")?;
                }
                if let Some(modifier) = &param.modifier {
                    write!(f, "{} ", modifier.param_mod)?;
                }
                write!(f, "{}: {}", param.name, param.ty)?;
            }

            write!(f, ")")?;
        }

        if self.return_ty.is_known() {
            write!(f, ": {}", self.return_ty)?;
        }
        write!(f, ";")?;
        
        match (self.body.stmts.len(), &self.body.output) {
            (0, Some(output)) => {
                write!(f, " {}", output)?;
            }

            (1, None) => {
                write!(f, " {}", self.body.stmts[0])?;
            }
            
            _ => {
                writeln!(f)?;
                write!(f, "{}", self.body)?;
            }
        }
        
        Ok(())
    }
}

impl<A: Annotation> Spanned for AnonymousFunctionDef<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl Parse for AnonymousFunctionDef<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let func_kw = tokens.match_one(Keyword::Function | Keyword::Procedure | Keyword::Lambda)?;

        // the two forms are parsed differently:
        //  * using the short form (`lambda`), expect an argument list with optional types 
        //    and a single expression as the body. the type of the result can't be explicitly
        //    specified and depends on the type of the body expression.
        //    examples:
        //    `lambda: 123`: function that takes no params
        //    `lambda(x): x + 1`: lambda with a single argument type, which must be inferrable
        //    `lambda(x: Integer): x + 1`: lambda with explicit argument type
        //  * using the full form (`function`), expect a standard function with mandatory
        //    type arguments and explicit return type. the body must be enclosed in a begin/end
        //    block as usual.
        let function_def = if func_kw.is_keyword(Keyword::Lambda) {
            let mut params = Vec::new();

            if let Some(params_group) = tokens.match_one_maybe(DelimiterPair::Bracket) {
                let mut params_tokens = params_group
                    .into_delimited_group()
                    .unwrap()
                    .into_inner_tokens();
                
                while let Some(TokenTree::Ident(ident)) = params_tokens.match_one_maybe(Matcher::AnyIdent) {                    
                    let ty= if params_tokens.match_one_maybe(Separator::Colon).is_some() {
                        TypeName::parse(&mut params_tokens)?
                    } else {
                        TypeName::Unspecified
                    };

                    params.push(FunctionParam {
                        name: ident.name,
                        name_span: Some(ident.span),
                        ty,
                        modifier: None,
                        is_implicit_self: false,
                    });

                    if params_tokens.match_one_maybe(Separator::Semicolon).is_none() {
                        break;
                    }
                }

                params_tokens.finish()?;
            }

            tokens.match_one(Separator::Colon)?;

            let body_expr = Expr::parse(tokens)?;

            let body = Block {
                annotation: body_expr.span().clone(),
                begin: body_expr.span().clone(),
                end: body_expr.span().clone(),
                stmts: Vec::new(),
                output: Some(body_expr),
                unsafe_kw: None,
            };

            let span = func_kw.span().to(body.span());

            AnonymousFunctionDef {
                annotation: span,
                body,
                params,
                return_ty: TypeName::Unspecified,
                captures: Default::default(),
            }
        } else {
            let params = match tokens.match_one_maybe(DelimiterPair::Bracket) {
                Some(tt) => {
                    let TokenTree::Delimited(params_group) = tt else {
                        unreachable!()
                    };
                    
                    let mut params_tokens = params_group.into_inner_tokens();
                    let params = FunctionDecl::parse_params(&mut params_tokens)?;
                    params_tokens.finish()?;
                    
                    params
                }

                None => Vec::new(),
            };

            let can_have_result = !func_kw.is_keyword(Keyword::Procedure);
            let expect_result = can_have_result
                && tokens.match_one_maybe(Separator::Colon).is_some();
            
            let return_ty = if expect_result {
                TypeName::parse(tokens)?
            } else {
                TypeName::Unspecified
            };

            tokens.match_one(Separator::Semicolon)?;

            let body = Block::parse(tokens)?;

            let span = func_kw.span().to(body.span());

            AnonymousFunctionDef {
                annotation: span,
                body,
                params,
                return_ty,
                captures: Default::default(),
            }
        };

        Ok(function_def)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct QualifiedFunctionName {
    /// if the declaration is qualified with the owning type of this method, the name of that type.
    /// e.g. in `function A.B`, this is `Some(A)`, and in `function B` this is `None` 
    pub owning_ty_qual: Option<Box<TypePath>>,

    pub ident: Ident,

    pub type_params: Option<TypeList<Ident>>,
}

impl QualifiedFunctionName {
    pub fn span(&self) -> Span {
        match (&self.owning_ty_qual, &self.type_params) {
            (None, None) => self.ident.span.clone(),
            (Some(instance_ty), None) => instance_ty.span().to(self.ident.span()),
            (Some(instance_ty), Some(ty_params)) => instance_ty.span().to(ty_params.span()),
            (None, Some(ty_params)) => self.ident.span.to(&ty_params.span), 
        }
    }
}

impl FunctionName for QualifiedFunctionName {
    fn ident(&self) -> &Ident {
        &self.ident
    }

    fn owning_type_name_semantic_hint(&self) -> SemanticHint {
        SemanticHint::Type
    }

    fn owning_type_name_span(&self) -> Option<&Span> {
        self.owning_ty_qual
            .as_ref()
            .map(|qual| &qual.name_span)
    }

    fn owning_type_params_len(&self) -> usize {
        self.owning_ty_qual
            .as_ref()
            .and_then(|qual| qual.type_params.as_ref())
            .map(|params| params.len())
            .unwrap_or(0)
    }

    fn owning_type_param_span(&self, index: usize) -> &Span {
        let owning_ty_params = &self.owning_ty_qual.as_ref().unwrap().type_params;
        &owning_ty_params.as_ref().unwrap().items[index].span
    }

    fn type_params_len(&self) -> usize {
        self.type_params
            .as_ref()
            .map(|list| list.len())
            .unwrap_or(0)
    }

    fn type_param_span(&self, index: usize) -> &Span {
        &self.type_params
            .as_ref()
            .unwrap()
            .items[index].span
    }
}

impl fmt::Display for QualifiedFunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(instance_ty) = &self.owning_ty_qual {
            write!(f, "{}.", instance_ty)?;
        }
        
        write!(f, "{}", self.ident)?;


        if let Some(ty_list) = &self.type_params {
            write!(f, "{}", ty_list)?;
        }
        
        Ok(())
    }
}
