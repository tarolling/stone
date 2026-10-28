//! The code the interpreter runs: each body turned, once, when it is defined, into a tree in which
//! every variable is a slot and every call says what it calls, so running it indexes where the
//! AST would have the interpreter hash names and spans.
//!
//! For example, in `fn f(n: int) -> int: ret f(n - 1)`, `n` becomes `Var::Local(0)` and the call
//! becomes `Call::User` of `f`'s id, with no lookup left for the run.

use super::{Interpreter, LastUses, Layout, Types, Value, field_path, is_definition};
use crate::ast::{self, BoolOp, CompOp, Operator, PatternKind, UnaryOp, is_lambda, target_names};
use crate::checker::{
    Calls, Conversion, FunctionValue, Method, Step, Type, may_name_enum, may_name_struct, place,
    place_spans, range_args,
};
use crate::span::Span;
use crate::stdlib;
use std::rc::Rc;
use std::sync::Arc;

/// Where a variable lives: a slot of the running call's frame, or a global, by its id.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Var {
    Local(usize),
    Global(usize),
}

/// A statement, with the variables it uses for the last time, which are dropped after it so a
/// list nothing else holds can change in place.
pub(super) struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub last_uses: Box<[Var]>,
}

pub(super) enum StmtKind {
    Return(Option<Expr>),
    Assign {
        targets: Vec<Target>,
        value: Expr,
    },
    AugAssign {
        target: AugTarget,
        op: Operator,
        value: Expr,
    },
    /// A `for` loop over `range`, whose bounds are evaluated once.
    ForRange {
        target: Unpack,
        bounds: Vec<Expr>,
        body: Vec<Stmt>,
    },
    For {
        target: Unpack,
        iter: Expr,
        body: Vec<Stmt>,
    },
    While {
        test: Expr,
        body: Vec<Stmt>,
    },
    If {
        test: Expr,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    Expr(Expr),
    Match {
        subject: Expr,
        cases: Vec<(Pattern, Vec<Stmt>)>,
    },
    Break,
    Continue,
    /// A statement that cannot run, which fails with its message.
    Fail(String),
}

/// Names a value is assigned to, or unpacked into if it is a tuple, as `a, (b, c)`.
pub(super) enum Unpack {
    Var(Var),
    Tuple(Vec<Unpack>),
    Invalid,
}

/// What an assignment assigns to.
pub(super) enum Target {
    Unpack(Unpack),
    /// An element of a list or an entry of a map, as `xs[i]`: the place of the list, then the
    /// index, which fails at `span` if it is out of range.
    Element {
        list: PlaceOr,
        index: Expr,
        span: Span,
    },
    Field(PlaceOr),
    Invalid,
}

/// What an augmented assignment changes.
pub(super) enum AugTarget {
    Var {
        var: Var,
        span: Span,
    },
    /// An element, as `xs[i] += 1`: `spans` are the target's place spans, through the element.
    Element {
        list: PlaceOr,
        index: Expr,
        spans: Vec<Span>,
        span: Span,
    },
    Field(PlaceOr),
    Invalid,
}

/// A variable or a value inside one that a statement changes, as `ps[i].x`: the variable, then
/// each step down, with the span each step fails at ([`place_spans`]).
pub(super) struct Place {
    pub root: Var,
    pub steps: Vec<PlaceStep>,
    pub spans: Vec<Span>,
}

/// A place, or the error that writing one where no variable is fails with.
pub(super) type PlaceOr = Result<Place, String>;

pub(super) enum PlaceStep {
    Index(Expr),
    Field(String),
}

pub(super) enum Pattern {
    Wildcard,
    Bind(Var),
    /// A variant of the enum with the id `id`, by its name, and patterns for its values.
    Variant {
        id: usize,
        name: String,
        fields: Vec<Pattern>,
    },
    /// A pattern that matches nothing, such as a variant of an enum that does not exist.
    Never,
}

pub(super) struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub(super) enum ExprKind {
    Constant(Value),
    Var(Var),
    /// A function named as a value, or a closure, which holds a copy of the value of each of
    /// `captures`, taken where it is made.
    Function {
        value: Arc<FunctionValue>,
        id: usize,
        lambda: bool,
        types: Option<Rc<InstanceTypes>>,
        captures: Vec<Expr>,
    },
    Try(Box<Expr>),
    JoinedStr(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    BoolOp(BoolOp, Vec<Expr>),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    /// `needle in container` or `needle not in container`.
    Membership(CompOp, Box<Expr>, Box<Expr>),
    Compare(Box<Expr>, Vec<(CompOp, Expr)>),
    Call(Box<Call>),
    /// A variant of the enum with the id `id`, as `Shape.Circle(1.0)` or `Shape.Empty`.
    Variant {
        id: usize,
        name: String,
        args: Vec<Expr>,
    },
    Field(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Slice {
        value: Box<Expr>,
        lower: Option<Box<Expr>>,
        upper: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
    },
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    /// An expression that cannot run, which fails with its message.
    Fail(String),
}

/// What a call calls, as the checker and the definitions resolved it.
pub(super) enum Call {
    Conversion {
        conversion: Rc<Conversion>,
        arg: Expr,
        types: Option<Rc<InstanceTypes>>,
    },
    /// A call of a function value, as `f(x)`, `fs[0](x)`, or `p.run(x)`.
    Value {
        func: Expr,
        args: Vec<Expr>,
        moves: Box<[Move]>,
    },
    /// A struct's construction, with each value by the field it names, in written order.
    Struct {
        id: usize,
        name: String,
        fields: Vec<(String, Expr)>,
    },
    /// A function of an `impl` block, or a method through a bound. `function` is the id of
    /// `method.function`, `receiver` is `None` for an associated function, and `place` is the
    /// receiver's place, for a method that changes it.
    Method {
        method: Rc<Method>,
        function: usize,
        receiver: Option<Expr>,
        place: Option<PlaceOr>,
        args: Vec<Expr>,
        moves: Box<[Move]>,
        types: Option<Rc<InstanceTypes>>,
    },
    /// A builtin method that changes its receiver, as `xs.append(1)`.
    Change {
        place: PlaceOr,
        attr: String,
        args: Vec<Expr>,
    },
    BuiltinMethod {
        receiver: Expr,
        attr: String,
        args: Vec<Expr>,
    },
    Builtin {
        name: String,
        args: Vec<Expr>,
    },
    /// A stone function, by its id and its name.
    User {
        function: usize,
        name: String,
        args: Vec<Expr>,
        moves: Box<[Move]>,
        types: Option<Rc<InstanceTypes>>,
    },
}

/// What a statement's call may take from its caller, since nothing reads it after the call: a
/// variable, or a field of one reached through fields alone, such as `b.items`.
pub(super) enum Move {
    Variable(Var),
    Field(Place),
}

/// The type arguments of a call of a generic function: its type parameters' names and a type
/// for each, which may name the running copy's own parameters, and the types already filled in
/// if they name none.
pub(super) struct InstanceTypes {
    pub params: Vec<String>,
    pub args: Vec<Type>,
    pub constant: Option<Types>,
}

/// Returns whether `ty` names a type parameter anywhere, so the running copy's types fill it in.
fn holds_param(ty: &Type) -> bool {
    match ty {
        Type::Param(_) => true,
        Type::List(elem) => holds_param(elem),
        Type::Map(key, value) => holds_param(key) || holds_param(value),
        Type::Tuple(types) | Type::Enum { args: types, .. } | Type::Struct { args: types, .. } => {
            types.iter().any(holds_param)
        }
        Type::Function { params, ret } => params.iter().any(holds_param) || holds_param(ret),
        _ => false,
    }
}

/// Turns the AST of one body into [`Stmt`]s: a function's, given its [`Layout`], or the
/// program's or an interactive entry's, whose variables are all globals.
pub(super) struct Resolver<'r, 'out> {
    interpreter: &'r mut Interpreter<'out>,
    calls: &'r Calls,
    layout: Option<&'r Layout>,
    last_uses: &'r LastUses,
}

/// The call a statement makes as its value, by span, and the variables it may take its
/// arguments from, since nothing reads them after it.
struct TopCall {
    span: Span,
    names: Vec<String>,
}

impl<'r, 'out> Resolver<'r, 'out> {
    pub fn new(
        interpreter: &'r mut Interpreter<'out>,
        calls: &'r Calls,
        layout: Option<&'r Layout>,
        last_uses: &'r LastUses,
    ) -> Self {
        Resolver {
            interpreter,
            calls,
            layout,
            last_uses,
        }
    }

    /// Resolves the statements of `body`, leaving out definitions, which run before any of them.
    pub fn body(&mut self, body: &[ast::Stmt]) -> Vec<Stmt> {
        body.iter()
            .filter(|stmt| !is_definition(stmt))
            .map(|stmt| self.stmt(stmt))
            .collect()
    }

    /// Resolves a function's body, or a block nested in a statement, in which a definition
    /// cannot run.
    pub fn block(&mut self, body: &[ast::Stmt]) -> Vec<Stmt> {
        body.iter().map(|stmt| self.stmt(stmt)).collect()
    }

    fn var(&mut self, name: &str) -> Var {
        match self.layout.and_then(|layout| layout.slot(name)) {
            Some(slot) => Var::Local(slot),
            None => Var::Global(self.interpreter.global_id(name)),
        }
    }

    fn stmt(&mut self, stmt: &ast::Stmt) -> Stmt {
        let names = self.last_uses.get(stmt.span).cloned().unwrap_or_default();
        // a function drops only its own locals
        let layout = self.layout;
        let last_uses = names
            .iter()
            .filter(|name| layout.is_none_or(|layout| layout.slot(name).is_some()))
            .map(|name| self.var(name))
            .collect();
        let kind = self.stmt_kind(stmt, &names);
        Stmt {
            kind,
            span: stmt.span,
            last_uses,
        }
    }

    /// Returns the call `value` makes if it calls a stone function, with the variables it may
    /// take its arguments from: `last_uses`, what the statement uses for the last time, and the
    /// names and fields `targets` assigns, such as `b.items`.
    ///
    /// For example, `xs = add(xs, 1)` may take `xs`, since the assignment replaces it.
    fn top_call(
        &self,
        value: &ast::Expr,
        last_uses: &[String],
        targets: &[ast::Expr],
    ) -> Option<TopCall> {
        let ast::ExprKind::Call { func, .. } = &value.without_try().kind else {
            return None;
        };
        let span = value.without_try().span;
        let stone = match &func.kind {
            _ if self.calls.indirect_calls.contains(&span) => true,
            _ if self.calls.conversions.contains_key(&span) => false,
            ast::ExprKind::Name { id, .. } => {
                !stdlib::is_builtin(id)
                    && !(may_name_struct(func) && self.interpreter.struct_ids.contains_key(id))
            }
            _ => self.calls.methods.contains_key(&span),
        };
        if !stone {
            return None;
        }
        let mut names = last_uses.to_vec();
        for target in targets {
            if let Some(path) = field_path(target) {
                names.push(path);
            }
            let mut assigned = Vec::new();
            target_names(target, &mut assigned);
            for name in assigned {
                if let ast::ExprKind::Name { id, .. } = &name.kind {
                    names.push(id.clone());
                }
            }
        }
        Some(TopCall { span, names })
    }

    fn stmt_kind(&mut self, stmt: &ast::Stmt, last_uses: &[String]) -> StmtKind {
        match &stmt.kind {
            ast::StmtKind::FunctionDef { .. }
            | ast::StmtKind::ImplDef { .. }
            | ast::StmtKind::TraitDef { .. }
            | ast::StmtKind::EnumDef { .. }
            | ast::StmtKind::StructDef { .. } => {
                StmtKind::Fail("a definition must be at the top level".into())
            }
            ast::StmtKind::Return { value } => StmtKind::Return(value.as_ref().map(|value| {
                let top = self.top_call(value, last_uses, &[]);
                self.expr_with(value, top.as_ref())
            })),
            ast::StmtKind::Assign { targets, value } => {
                // a target's indexes are read after the call
                let top = match targets.iter().all(|target| {
                    matches!(
                        target.kind,
                        ast::ExprKind::Name { .. } | ast::ExprKind::Tuple { .. }
                    ) || field_path(target).is_some()
                }) {
                    true => self.top_call(value, last_uses, targets),
                    false => None,
                };
                let value = self.expr_with(value, top.as_ref());
                let targets = targets.iter().map(|target| self.target(target)).collect();
                StmtKind::Assign { targets, value }
            }
            ast::StmtKind::AugAssign { target, op, value } => {
                let target = match &target.kind {
                    ast::ExprKind::Name { id, .. } => AugTarget::Var {
                        var: self.var(id),
                        span: target.span,
                    },
                    ast::ExprKind::Subscript {
                        value: list, slice, ..
                    } => AugTarget::Element {
                        list: self.place(list),
                        index: self.expr(slice),
                        spans: place_spans(target),
                        span: target.span,
                    },
                    ast::ExprKind::Attribute { .. } => AugTarget::Field(self.place(target)),
                    _ => AugTarget::Invalid,
                };
                StmtKind::AugAssign {
                    target,
                    op: *op,
                    value: self.expr(value),
                }
            }
            ast::StmtKind::For { target, iter, body } => {
                let target = self.unpack(target);
                match range_args(iter) {
                    Some(args) => StmtKind::ForRange {
                        target,
                        bounds: args.iter().map(|arg| self.expr(arg)).collect(),
                        body: self.block(body),
                    },
                    None => StmtKind::For {
                        target,
                        iter: self.expr(iter),
                        body: self.block(body),
                    },
                }
            }
            ast::StmtKind::While { test, body } => StmtKind::While {
                test: self.expr(test),
                body: self.block(body),
            },
            ast::StmtKind::If { test, body, orelse } => StmtKind::If {
                test: self.expr(test),
                body: self.block(body),
                orelse: self.block(orelse),
            },
            ast::StmtKind::Expr { value } => {
                let top = self.top_call(value, last_uses, &[]);
                StmtKind::Expr(self.expr_with(value, top.as_ref()))
            }
            ast::StmtKind::Match { subject, cases } => StmtKind::Match {
                subject: self.expr(subject),
                cases: cases
                    .iter()
                    .map(|case| (self.pattern(&case.pattern), self.block(&case.body)))
                    .collect(),
            },
            ast::StmtKind::Break => StmtKind::Break,
            ast::StmtKind::Continue => StmtKind::Continue,
            ast::StmtKind::Use { .. } => {
                StmtKind::Fail("use is only allowed at the top level of a file".into())
            }
        }
    }

    fn unpack(&mut self, target: &ast::Expr) -> Unpack {
        match &target.kind {
            ast::ExprKind::Name { id, .. } => Unpack::Var(self.var(id)),
            ast::ExprKind::Tuple { elts, .. } => {
                Unpack::Tuple(elts.iter().map(|elt| self.unpack(elt)).collect())
            }
            _ => Unpack::Invalid,
        }
    }

    fn target(&mut self, target: &ast::Expr) -> Target {
        match &target.kind {
            ast::ExprKind::Name { .. } | ast::ExprKind::Tuple { .. } => {
                Target::Unpack(self.unpack(target))
            }
            ast::ExprKind::Subscript { value, slice, .. } => Target::Element {
                list: self.place(value),
                index: self.expr(slice),
                span: target.span,
            },
            ast::ExprKind::Attribute { .. } => Target::Field(self.place(target)),
            _ => Target::Invalid,
        }
    }

    fn place(&mut self, expr: &ast::Expr) -> PlaceOr {
        let Some((root, steps)) = place(expr) else {
            return Err("only a value in a variable can be changed".into());
        };
        let steps = steps
            .into_iter()
            .map(|step| match step {
                Step::Index(index) => PlaceStep::Index(self.expr(index)),
                Step::Field(name) => PlaceStep::Field(name.to_string()),
            })
            .collect();
        Ok(Place {
            root: self.var(root),
            steps,
            spans: place_spans(expr),
        })
    }

    fn pattern(&mut self, pattern: &ast::Pattern) -> Pattern {
        match &pattern.kind {
            PatternKind::Wildcard => Pattern::Wildcard,
            PatternKind::Name(name) => Pattern::Bind(self.var(name)),
            PatternKind::Variant { path, fields } => match path.as_slice() {
                [(enum_name, _), (variant, _)] => match self.interpreter.enum_ids.get(enum_name) {
                    Some(&id) => Pattern::Variant {
                        id,
                        name: variant.clone(),
                        fields: fields.iter().map(|field| self.pattern(field)).collect(),
                    },
                    None => Pattern::Never,
                },
                _ => Pattern::Never,
            },
        }
    }

    /// Returns the type arguments of the call or function value at `span`, if it names a generic
    /// function.
    fn instance(&self, span: Span) -> Option<Rc<InstanceTypes>> {
        let instance = self.calls.instances.get(&span)?;
        let params = self.calls.type_params.get(&instance.function)?;
        let constant = match instance.args.iter().any(holds_param) {
            true => None,
            false => Some(Rc::new(
                params
                    .iter()
                    .cloned()
                    .zip(instance.args.iter().cloned())
                    .collect(),
            )),
        };
        Some(Rc::new(InstanceTypes {
            params: params.clone(),
            args: instance.args.clone(),
            constant,
        }))
    }

    fn exprs(&mut self, exprs: &[ast::Expr]) -> Vec<Expr> {
        exprs.iter().map(|expr| self.expr(expr)).collect()
    }

    fn boxed(&mut self, expr: &ast::Expr) -> Box<Expr> {
        Box::new(self.expr(expr))
    }

    fn expr(&mut self, expr: &ast::Expr) -> Expr {
        self.expr_with(expr, None)
    }

    /// Resolves `expr`, whose call at `top`'s span, if any, is its statement's top call.
    fn expr_with(&mut self, expr: &ast::Expr, top: Option<&TopCall>) -> Expr {
        let kind = match &expr.kind {
            ast::ExprKind::Lambda { .. } => {
                ExprKind::Fail("a lambda must be linked before it runs".into())
            }
            ast::ExprKind::Try { value } => ExprKind::Try(Box::new(self.expr_with(value, top))),
            ast::ExprKind::JoinedStr { values } => ExprKind::JoinedStr(self.exprs(values)),
            ast::ExprKind::Dict { keys, values } => ExprKind::Dict(
                keys.iter()
                    .zip(values)
                    .map(|(key, value)| (self.expr(key), self.expr(value)))
                    .collect(),
            ),
            ast::ExprKind::BoolOp { op, values } => {
                ExprKind::BoolOp(op.clone(), self.exprs(values))
            }
            ast::ExprKind::BinOp { op, left, right } => {
                ExprKind::Binary(*op, self.boxed(left), self.boxed(right))
            }
            ast::ExprKind::UnaryOp { op, operand } => ExprKind::Unary(*op, self.boxed(operand)),
            ast::ExprKind::Compare {
                left,
                ops,
                comparators,
            } if ops.iter().any(CompOp::is_membership) => match &ops[..] {
                [op] => {
                    ExprKind::Membership(op.clone(), self.boxed(left), self.boxed(&comparators[0]))
                }
                _ => ExprKind::Fail("'in' cannot be chained with other comparisons".into()),
            },
            ast::ExprKind::Compare {
                left,
                ops,
                comparators,
            } => ExprKind::Compare(
                self.boxed(left),
                ops.iter()
                    .zip(comparators)
                    .map(|(op, comparator)| (op.clone(), self.expr(comparator)))
                    .collect(),
            ),
            ast::ExprKind::Call {
                func,
                args,
                keywords,
            } => {
                let top = top.filter(|top| top.span == expr.span);
                return self.call(expr.span, func, args, keywords, top);
            }
            ast::ExprKind::Attribute { value, attr, .. } => match &value.kind {
                ast::ExprKind::Name { id, .. }
                    if may_name_enum(value) && self.interpreter.enum_ids.contains_key(id) =>
                {
                    ExprKind::Variant {
                        id: self.interpreter.enum_ids[id],
                        name: attr.clone(),
                        args: vec![],
                    }
                }
                _ => ExprKind::Field(self.boxed(value), attr.clone()),
            },
            ast::ExprKind::Constant { value, .. } => ExprKind::Constant(Value::from(&**value)),
            ast::ExprKind::Subscript { value, slice, .. } => match &slice.kind {
                ast::ExprKind::Slice { lower, upper, step } => ExprKind::Slice {
                    value: self.boxed(value),
                    lower: lower.as_ref().map(|bound| self.boxed(bound)),
                    upper: upper.as_ref().map(|bound| self.boxed(bound)),
                    step: step.as_ref().map(|bound| self.boxed(bound)),
                },
                _ => ExprKind::Index(self.boxed(value), self.boxed(slice)),
            },
            ast::ExprKind::Name { id, .. } => match self.calls.function_values.get(&expr.span) {
                Some(function) => ExprKind::Function {
                    value: function.clone(),
                    id: self.interpreter.function_id(&function.function),
                    lambda: is_lambda(&function.function),
                    types: self.instance(expr.span),
                    captures: vec![],
                },
                None => ExprKind::Var(self.var(id)),
            },
            ast::ExprKind::Closure { captures, .. } => {
                match self.calls.function_values.get(&expr.span) {
                    Some(function) => ExprKind::Function {
                        value: function.clone(),
                        id: self.interpreter.function_id(&function.function),
                        lambda: true,
                        types: self.instance(expr.span),
                        captures: self.exprs(captures),
                    },
                    None => ExprKind::Fail("a closure must be checked before it runs".into()),
                }
            }
            ast::ExprKind::List { elts, .. } => ExprKind::List(self.exprs(elts)),
            ast::ExprKind::Tuple { elts, .. } => ExprKind::Tuple(self.exprs(elts)),
            ast::ExprKind::Slice { .. } => {
                ExprKind::Fail("a slice can only appear in brackets".into())
            }
        };
        Expr {
            kind,
            span: expr.span,
        }
    }

    /// Returns what a call may take from its caller, for the call of `callee` with `args` when it
    /// is its statement's top call: each argument that `top` names, unless it starts from `keep`,
    /// the variable a method changes.
    fn moves(
        &mut self,
        args: &[ast::Expr],
        top: Option<&TopCall>,
        keep: Option<&str>,
    ) -> Box<[Move]> {
        let Some(top) = top else {
            return Box::default();
        };
        let mut moves = Vec::new();
        for arg in args {
            if let ast::ExprKind::Name { id: name, .. } = &arg.kind
                && top.names.contains(name)
                && keep != Some(name.as_str())
            {
                moves.push(Move::Variable(self.var(name)));
            }
            if let Some(path) = field_path(arg)
                && let Some((root, _)) = path.split_once('.')
                && keep != Some(root)
                && (top.names.contains(&path) || top.names.iter().any(|name| name == root))
                && let Ok(place) = self.place(arg)
            {
                moves.push(Move::Field(place));
            }
        }
        moves.into_boxed_slice()
    }

    fn call(
        &mut self,
        span: Span,
        func: &ast::Expr,
        args: &[ast::Expr],
        keywords: &[ast::Keyword],
        top: Option<&TopCall>,
    ) -> Expr {
        let call = |call: Call| Expr {
            kind: ExprKind::Call(Box::new(call)),
            span,
        };
        let calls = self.calls;
        if let Some(conversion) = calls.conversions.get(&span) {
            return call(Call::Conversion {
                conversion: Rc::new(conversion.clone()),
                arg: self.expr(&args[0]),
                types: self.instance(span),
            });
        }
        // a function value, as `f(x)`, `fs[0](x)`, or `p.run(x)` with a field `run` call
        if calls.indirect_calls.contains(&span) {
            return call(Call::Value {
                func: self.expr(func),
                args: self.exprs(args),
                moves: self.moves(args, top, None),
            });
        }
        if let ast::ExprKind::Name { id, .. } = &func.kind
            && may_name_struct(func)
            && let Some(&struct_id) = self.interpreter.struct_ids.get(id)
        {
            return call(Call::Struct {
                id: struct_id,
                name: id.clone(),
                fields: keywords
                    .iter()
                    .map(|keyword| (keyword.arg.clone(), self.expr(&keyword.value)))
                    .collect(),
            });
        }
        if let ast::ExprKind::Attribute { value, attr, .. } = &func.kind
            && let ast::ExprKind::Name { id, .. } = &value.kind
            && may_name_enum(value)
            && let Some(&enum_id) = self.interpreter.enum_ids.get(id)
        {
            return Expr {
                kind: ExprKind::Variant {
                    id: enum_id,
                    name: attr.clone(),
                    args: self.exprs(args),
                },
                span,
            };
        }
        if let ast::ExprKind::Attribute { value, .. } = &func.kind
            && let Some(method) = calls.methods.get(&span)
        {
            let receiver_place = method.changes.then(|| self.place(value));
            let receiver = (method.receiver && !method.changes).then(|| self.expr(value));
            // a method that changes its receiver never takes it as an argument too
            let keep = match (&method.implementations, method.changes) {
                (Some(_), _) if matches!(method.function.as_str(), "eq.eq" | "ord.cmp") => None,
                (_, true) => place(value).map(|(root, _)| root),
                _ => None,
            };
            let moves = match method.receiver {
                true => self.moves(args, top, keep),
                false => self.moves(args, top, None),
            };
            return call(Call::Method {
                method: Rc::new(method.clone()),
                function: self.interpreter.function_id(&method.function),
                receiver,
                place: receiver_place,
                args: self.exprs(args),
                moves,
                types: self.instance(span),
            });
        }
        if let ast::ExprKind::Attribute { value, attr, .. } = &func.kind {
            if stdlib::changes_receiver(attr) {
                return call(Call::Change {
                    place: self.place(value),
                    attr: attr.clone(),
                    args: self.exprs(args),
                });
            }
            return call(Call::BuiltinMethod {
                receiver: self.expr(value),
                attr: attr.clone(),
                args: self.exprs(args),
            });
        }
        let ast::ExprKind::Name { id, .. } = &func.kind else {
            return Expr {
                kind: ExprKind::Fail("only a function or a function value can be called".into()),
                span,
            };
        };
        if stdlib::is_builtin(id) {
            return call(Call::Builtin {
                name: id.clone(),
                args: self.exprs(args),
            });
        }
        call(Call::User {
            function: self.interpreter.function_id(id),
            name: id.clone(),
            args: self.exprs(args),
            moves: self.moves(args, top, None),
            types: self.instance(span),
        })
    }
}
