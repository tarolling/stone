//! Lowers a checked AST to [`ir`](super) functions.
//!
//! Names resolve the way the checker resolves them: a function's parameters and assigned names
//! are its locals, each one vreg, and every name assigned at the top level is a global that lives
//! in memory. Evaluation is left to right, like the interpreter, so runtime errors happen in the
//! same order. For example, `x = x + 1` in a function lowers to `v0 = add v0, 1`.
//!
//! Strings, lists, and enum values are counted (see [`RcKind`]), and lowering keeps every count
//! exact:
//!
//! - every variable and list slot owns one reference to the value it holds
//! - an expression's value is either borrowed, like a variable's or a literal's, or a temporary
//!   that owns a reference, like a call's result or a list element (which is retained when read,
//!   since a later call could replace it in the list)
//! - whatever uses a temporary releases it right after, and storing a value moves a temporary's
//!   reference or retains a borrowed value
//! - a parameter is borrowed from the caller, unless the function assigns or changes it: then
//!   the function owns it, and the caller gives it a reference, a temporary's own, a retained
//!   one, or a variable's own when nothing reads the variable after the call (`xs = f(xs)` or
//!   `ret f(xs)`), which moves the value in so the function can change it in place
//! - lists are values, so before a statement changes one (`xs[i] = v` or `xs.append(v)`), each
//!   list from the variable to it is made unique (`list_unique`): copied if anything else refers
//!   to it, so the change shows through no other variable
//! - returning releases every local, every list a `for` loop is walking, and every value a
//!   `match` holds
//! - the end of `main` releases every global, so nothing is left when the program ends
//!
//! Globals can be borrowed because only top-level statements assign them, and a statement's
//! assignment happens after everything it evaluates. For example, `s = s + "x"` in a function
//! lowers to `v2 = call stone.str_concat(v0, v1)`, then `release_str v0`, then `v0 = copy v2`.

#[cfg(test)]
mod tests;

use super::{
    BinOp, Block, BlockId, Callee, Cond, Function, Inst, Kernel, Operand, Program, RcKind,
    Terminator, VReg, leaf_name, variable_of,
};
use crate::ast::{
    Arg, BoolOp, Constant, Expr, ExprKind, Keyword, MatchCase, Mod, Operator, Pattern, PatternKind,
    Stmt, StmtKind, UnaryOp, constant_int,
};
use crate::checker::{
    Analysis, Conversion, Converter, DISPLAY, EQ, Enums, Instance, Method, Methods, OPTION, ORD,
    RESULT, Step, Structs, SymbolKind, Type, collect_assigned, collect_changed, may_name_enum,
    may_name_struct, place, place_root, place_spans, range_args, struct_display, type_text,
    variant_display, variants,
};
use crate::codegen::context::{function_label, module_routine};
use crate::last_use::{GlobalReads, LiveAfter, global_reads, kept_at_exit, live_after};
use crate::span::Span;
use crate::stdlib::{
    BYTE_RANGE, BYTES_INDEX_RANGE, CLAMP_RANGE, COMPARE_DEPTH, END_OF_INPUT, INDEX_RANGE,
    LENGTHS_DIFFER, MAX_PRINT_DEPTH, POP_EMPTY, POP_RANGE, PRELUDE_CLAMP, PRELUDE_SORT,
    PRINT_DEPTH, SLICE_STEP_ZERO, STRING_INDEX_RANGE, UNWRAP_ERR, UNWRAP_NONE, UNWRAP_OK, math,
    random,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// The type of each variable, keyed by the function it belongs to (`None` for globals) and name.
type Variables = HashMap<(Option<String>, String), Type>;

/// Lowers every function in `module`, then its top-level code as `main`, using what the checker
/// learned about it in `analysis`: the types it inferred for every expression, for every
/// variable, for the values of every variant, and for the fields of every struct, and the
/// function each method call calls. Last come the functions that print enum values, which
/// lowering makes for each enum type the program prints.
///
/// Fails only on input the checker rejects, such as a constant type no backend supports.
pub fn lower(module: &Mod, analysis: &Analysis) -> Result<Program, String> {
    let Mod::Module { body } = module;
    let Analysis {
        types,
        symbols,
        enums,
        structs,
        methods,
        instances,
        type_params,
        implemented,
        function_values,
        indirect_calls,
        conversions,
        ..
    } = analysis;

    let mut assigned = Vec::new();
    collect_assigned(body, &mut assigned);

    let mut variables: Variables = symbols
        .iter()
        .filter(|symbol| {
            !matches!(
                symbol.kind,
                SymbolKind::Function
                    | SymbolKind::Enum
                    | SymbolKind::Variant
                    | SymbolKind::Struct
                    | SymbolKind::Field
                    | SymbolKind::Method
                    | SymbolKind::Trait
                    | SymbolKind::TypeParam
            )
        })
        .map(|symbol| {
            (
                (symbol.scope.clone(), symbol.name.clone()),
                symbol.ty.clone(),
            )
        })
        .collect();
    // each value of a tuple or struct is a variable of its own, so neither is ever stored as one
    let tuples: Vec<_> = variables
        .iter()
        .filter(|(_, ty)| ty.is_flat())
        .map(|(key, ty)| (key.clone(), ty.clone()))
        .collect();
    for ((scope, name), ty) in tuples {
        for (i, leaf) in ty.leaves(structs).into_iter().enumerate() {
            variables.insert((scope.clone(), leaf_name(&name, i)), leaf);
        }
    }
    let stone_globals: HashSet<String> = assigned.iter().map(|(name, _)| name.clone()).collect();
    let globals: Vec<String> = assigned
        .into_iter()
        .flat_map(|(name, _)| match variables.get(&(None, name.clone())) {
            Some(ty) if ty.is_flat() => (0..ty.leaves(structs).len())
                .map(|i| leaf_name(&name, i))
                .collect(),
            _ => vec![name],
        })
        .collect();
    let global_set: HashSet<String> = globals.iter().cloned().collect();
    let global_reads = global_reads(body, &stone_globals);
    let context = Context {
        types,
        variables: &variables,
        globals: &globals,
        owned_params: owned_params(body, methods),
        constant_globals: constant_globals(body, &global_reads),
        shrinks_maps: pops_maps(body, types),
        global_reads,
        global_set,
        handles: types
            .values()
            .any(|ty| holds_handle(ty, structs, enums, &mut HashSet::new())),
        enums,
        structs,
        methods,
        instances,
        implemented,
        function_values,
        indirect_calls,
        conversions,
        helpers: RefCell::new(Vec::new()),
        requested: RefCell::new(Vec::new()),
    };

    let mut functions = Vec::new();
    let mut top_level = Vec::new();
    // a generic function is lowered once for each set of types it is called with, and any
    // function again for each function value a call binds a parameter of it to
    let mut generic = HashMap::new();
    let mut bodies = HashMap::new();
    for stmt in body {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name,
                type_params: params,
                args,
                body,
                ..
            } if !params.is_empty() => {
                generic.insert(name.clone(), (&args.args, body));
            }
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                bodies.insert(name.clone(), (&args.args, body));
                let lowerer = Lowerer::new(&context, Some(name.clone()));
                functions.push(lowerer.function(&args.args, body)?);
            }
            StmtKind::ImplDef { name, methods, .. } => {
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name: method,
                        args,
                        body,
                        ..
                    } = &method.kind
                    {
                        let function = format!("{name}.{method}");
                        // a generic struct's methods are generic over its type parameters
                        if type_params.contains_key(name) {
                            generic.insert(function, (&args.args, body));
                            continue;
                        }
                        bodies.insert(function.clone(), (&args.args, body));
                        let lowerer = Lowerer::new(&context, Some(function));
                        functions.push(lowerer.function(&args.args, body)?);
                    }
                }
            }
            // a trait's default methods are generic over `Self`, and the others have no body
            StmtKind::TraitDef { name, methods, .. } => {
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name: method,
                        args,
                        body,
                        ..
                    } = &method.kind
                        && !body.is_empty()
                    {
                        generic.insert(format!("{name}.{method}"), (&args.args, body));
                    }
                }
            }
            _ => top_level.push(stmt.clone()),
        }
    }
    functions.push(Lowerer::new(&context, None).function(&[], &top_level)?);
    // an instance can call other instances, and both instances and helpers can ask for the
    // helpers of the enums and structs their values hold, so this runs until none is left
    let (mut made, mut instanced) = (0, 0);
    loop {
        let next = context.requested.borrow().get(instanced).cloned();
        if let Some(copy) = next {
            instanced += 1;
            let function = &copy.instance.function;
            let Some(&(args, body)) = generic.get(function).or_else(|| bodies.get(function)) else {
                return Err(format!("'{function}' has no body to copy"));
            };
            let names = match generic.contains_key(function) {
                true => type_params
                    .get(function)
                    .ok_or_else(|| format!("'{function}' has no type parameters"))?
                    .as_slice(),
                false => &[],
            };
            let lowerer = Lowerer::new_instance(&context, &copy, names, args);
            functions.push(lowerer.function(args, body)?);
            continue;
        }
        let next = context.helpers.borrow().get(made).cloned();
        let Some(helper) = next else {
            break;
        };
        made += 1;
        let lowerer = Lowerer::new(&context, Some(helper.label()));
        functions.push(match helper {
            Helper::Print(ty @ Type::Struct { .. }) => lowerer.struct_printer(&ty)?,
            Helper::Print(ty) => lowerer.enum_printer(&ty)?,
            Helper::Equal(ty) => lowerer.enum_comparer(&ty)?,
            Helper::FunctionValue {
                function,
                owned,
                params,
                ret,
            } => lowerer.function_value_thunk(&function, &owned, &params, &ret)?,
        });
    }

    Ok(Program { functions, globals })
}

/// Returns the globals of the top-level code `body` that read as the same int, float, or bool
/// literal everywhere, with the immediate each reads as, so that reading one needs no load and no
/// check that it was assigned, and a divisor held in one divides like a literal.
///
/// A global qualifies when one statement of the top level itself, not inside a block, assigns it
/// a literal, nothing else assigns it anywhere (a function never assigns a global), and no
/// statement before that one can call a function that reads it, which would fail because the
/// global is not assigned yet. For example, `m = 7` at the start of a program makes every `m`
/// read as `7`.
fn constant_globals(body: &[Stmt], reads: &GlobalReads) -> HashMap<String, i64> {
    // a method that changes `self`, such as a trait's on `int`, changes its receiver without
    // assigning it
    let mut receivers = HashSet::new();
    method_receivers(body, &mut receivers);
    let mut constants = HashMap::new();
    for (i, stmt) in body.iter().enumerate() {
        let StmtKind::Assign { targets, value } = &stmt.kind else {
            continue;
        };
        let [target] = targets.as_slice() else {
            continue;
        };
        let ExprKind::Name { id, .. } = &target.kind else {
            continue;
        };
        let literal = match &value.kind {
            ExprKind::Constant {
                value: constant, ..
            } => match **constant {
                Constant::Float(x) => Some(x.to_bits() as i64),
                Constant::Bool(b) => Some(b as i64),
                _ => constant_int(value),
            },
            ExprKind::UnaryOp {
                op: UnaryOp::UnarySub,
                operand,
            } => match &operand.kind {
                ExprKind::Constant {
                    value: constant, ..
                } => match **constant {
                    Constant::Float(x) => Some((-x).to_bits() as i64),
                    _ => constant_int(value),
                },
                _ => None,
            },
            _ => None,
        };
        let Some(literal) = literal else {
            continue;
        };
        let assigned_elsewhere = body.iter().enumerate().any(|(j, other)| {
            let mut assigned = Vec::new();
            collect_assigned(std::slice::from_ref(other), &mut assigned);
            j != i && assigned.iter().any(|(name, _)| name == id)
        });
        if !assigned_elsewhere
            && !receivers.contains(id.as_str())
            && !reads.of_statements(&body[..i]).contains(id)
        {
            constants.insert(id.clone(), literal);
        }
    }
    constants
}

/// Returns whether any code in `body`, functions included, calls `pop` with a key on a map, whose
/// type `types` gives by span.
fn pops_maps(body: &[Stmt], types: &HashMap<Span, Type>) -> bool {
    let mut found = false;
    visit_statements(body, &mut |stmt| {
        for expr in stmt.exprs() {
            expr.visit(&mut |expr| {
                if let ExprKind::Call { func, args, .. } = &expr.kind
                    && let ExprKind::Attribute { value, attr, .. } = &func.kind
                    && attr == "pop"
                    && args.len() == 1
                    && matches!(types.get(&value.span), Some(Type::Map(..)))
                {
                    found = true;
                }
            });
        }
    });
    found
}

/// Calls `f` on every statement of `body`, in blocks and function bodies too.
fn visit_statements<'a>(body: &'a [Stmt], f: &mut impl FnMut(&'a Stmt)) {
    for stmt in body {
        f(stmt);
        match &stmt.kind {
            StmtKind::If { body, orelse, .. } => {
                visit_statements(body, f);
                visit_statements(orelse, f);
            }
            StmtKind::While { body, .. }
            | StmtKind::For { body, .. }
            | StmtKind::FunctionDef { body, .. } => visit_statements(body, f),
            StmtKind::ImplDef { methods, .. } | StmtKind::TraitDef { methods, .. } => {
                visit_statements(methods, f)
            }
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    visit_statements(&case.body, f);
                }
            }
            _ => {}
        }
    }
}

/// Adds to `out` every variable that top-level code `body`, in blocks too, calls a method on,
/// as `n` in `n.bump()`.
fn method_receivers<'a>(body: &'a [Stmt], out: &mut HashSet<&'a str>) {
    for stmt in body {
        for expr in stmt.exprs() {
            expr.visit(&mut |expr| {
                if let ExprKind::Call { func, .. } = &expr.kind
                    && let ExprKind::Attribute { value, .. } = &func.kind
                    && let ExprKind::Name { id, .. } = &value.kind
                {
                    out.insert(id);
                }
            });
        }
        match &stmt.kind {
            StmtKind::If { body, orelse, .. } => {
                method_receivers(body, out);
                method_receivers(orelse, out);
            }
            StmtKind::While { body, .. } | StmtKind::For { body, .. } => {
                method_receivers(body, out)
            }
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    method_receivers(&case.body, out);
                }
            }
            _ => {}
        }
    }
}

/// A function lowering makes for a type some code needs it for, called through
/// [`Callee::Helper`].
#[derive(Clone, PartialEq)]
enum Helper {
    /// Prints a value of an enum or struct type.
    Print(Type),
    /// Compares two values of an enum type with `==`.
    Equal(Type),
    /// Calls the stone function `function` (the label of its copy, for a generic one), whose
    /// parameters of types `params` it owns where `owned` says so, for a function value of it:
    /// a call through a value borrows every argument, so this gives each owned one a reference
    /// first.
    FunctionValue {
        function: String,
        owned: Vec<bool>,
        params: Vec<Type>,
        ret: Type,
    },
}

impl Helper {
    /// Returns the helper's label.
    ///
    /// For example, `option[list[int]]` is printed by `stone.print_enum.option.list_int`, `Point`
    /// by `stone.print_struct.Point`, and two `option[int]`s are compared by
    /// `stone.eq_enum.option.int`.
    fn label(&self) -> String {
        match self {
            Helper::Print(ty @ Type::Struct { .. }) => {
                format!("stone.print_struct.{}", mangle(ty))
            }
            Helper::Print(ty) => format!("stone.print_enum.{}", mangle(ty)),
            Helper::Equal(ty) => format!("stone.eq_enum.{}", mangle(ty)),
            Helper::FunctionValue { function, .. } => format!("stone.fn_value.{function}"),
        }
    }
}

/// Returns the label of the copy of the generic function `function` whose type parameters are
/// `args`: its name, then each type as [`mangle`] names it, each after two dots, which no linked
/// name or mangled type holds.
///
/// For example, `first` called with `list[int]` is `first..list_int`, and `zoo.pair` with `str`
/// and `int` is `zoo.pair..str..int`.
fn instance_label(function: &str, args: &[Type]) -> String {
    let args: Vec<String> = args.iter().map(mangle).collect();
    format!("{function}..{}", args.join(".."))
}

/// Names the type `ty` in a label.
///
/// For example, `option[tuple[int, str]]` is `option.tuple2_int_str`, and
/// `option[map[str, float]]` is `option.map_str_float`.
fn mangle(ty: &Type) -> String {
    match ty {
        Type::List(elem) => format!("list_{}", mangle(elem)),
        Type::Map(key, value) => format!("map_{}_{}", mangle(key), mangle(value)),
        Type::Tuple(fields) => {
            let fields: Vec<String> = fields.iter().map(mangle).collect();
            format!("tuple{}_{}", fields.len(), fields.join("_"))
        }
        Type::Enum { name, args } | Type::Struct { name, args } if args.is_empty() => name.clone(),
        Type::Enum { name, args } | Type::Struct { name, args } => {
            let args: Vec<String> = args.iter().map(mangle).collect();
            format!("{name}.{}", args.join("_"))
        }
        // the parameters, then what it returns
        Type::Function { params, ret } => {
            let params: Vec<String> = params.iter().map(mangle).collect();
            let ret = match **ret {
                Type::Unit => "unit".to_string(),
                _ => mangle(ret),
            };
            format!("fn{}_{}_{ret}", params.len(), params.join("_"))
        }
        other => other.to_string(),
    }
}

/// What lowering each function shares.
struct Context<'a> {
    types: &'a HashMap<Span, Type>,
    /// The type of each variable, including one for each value of a tuple variable, such as
    /// `t.0` for `t`.
    variables: &'a Variables,
    /// Every global, in the order they are first assigned, each value of a tuple as its own.
    globals: &'a [String],
    global_set: HashSet<String>,
    /// The globals that hold one literal for the whole program, with the immediate each reads as
    /// (see [`constant_globals`]).
    constant_globals: HashMap<String, i64>,
    /// Whether the program removes entries from maps, which can leave holes in a map's entries
    /// that must close up before anything reads them as a list (see `builtins::map_runtime`).
    shrinks_maps: bool,
    /// For each function, whether it owns each parameter, which it does if it assigns or changes
    /// it. A caller gives an owned parameter a reference of its own.
    owned_params: HashMap<String, Vec<bool>>,
    /// For each function, the globals it or any function it calls may read, which a caller must
    /// not move into it, by their stone names.
    global_reads: GlobalReads,
    /// The variants of every enum the program defines.
    enums: &'a Enums,
    /// Whether any variable holds a socket or listener, which is released at its last use.
    handles: bool,
    /// The fields of every struct the program defines.
    structs: &'a Structs,
    /// How every call of a function of an `impl` block was resolved, by the call's span.
    methods: &'a Methods,
    /// The type arguments of every call of a generic function, by the call's span.
    instances: &'a HashMap<Span, Instance>,
    /// The traits each type implements, by the type's linked name, which say whether a struct
    /// compares and orders by methods of its own.
    implemented: &'a HashMap<String, Vec<String>>,
    /// Every function named as a value, by the name's span, with its linked name.
    function_values: &'a HashMap<Span, String>,
    /// The span of every call of a function value.
    indirect_calls: &'a HashSet<Span>,
    /// Every conversion `Type(value)`, by the call's span.
    conversions: &'a HashMap<Span, Conversion>,
    /// Each copy of a function some code calls, in the order it was first called: one for each
    /// set of types a generic function is called with, and for each function value a function
    /// is passed for a parameter it never assigns.
    requested: RefCell<Vec<Request>>,
    /// Each helper some code calls, in the order it was first called: one that prints values of
    /// each enum and struct type the program prints, and one that compares values of each enum
    /// type it compares.
    helpers: RefCell<Vec<Helper>>,
}

/// A copy of a function lowering makes: of a generic function for the types `instance` gives
/// its type parameters, and with each parameter `constants` names bound to the function value
/// a call passed it, which the copy then calls directly, without taking it as an argument.
///
/// For example, `xs.map(double)` with `xs` a `list[int]` calls the copy of `prelude.map` with
/// `int` for both `T` and `U`, and `f`, its parameter 1, bound to `double`.
#[derive(Clone, Debug, PartialEq)]
struct Request {
    instance: Instance,
    constants: Vec<(usize, BoundFunction)>,
}

impl Request {
    /// Returns the copy's label: its instance's, as [`instance_label`] names it, or the
    /// function's own name if it is not generic, then each bound parameter's position and
    /// function's label after two dots.
    ///
    /// For example, `prelude.map` for `int` and `str` with `f` bound to `show` is
    /// `prelude.map..int..str..fn1.show`.
    fn label(&self) -> String {
        let mut label = match self.instance.args.is_empty() {
            true => self.instance.function.clone(),
            false => instance_label(&self.instance.function, &self.instance.args),
        };
        for (position, constant) in &self.constants {
            label.push_str(&format!("..fn{position}.{}", constant.label));
        }
        label
    }
}

/// The function a parameter of a copy holds, which every call of the parameter calls directly:
/// its stone name, for what it owns and reads, and the label of its copy.
///
/// For example, `first` used for `list[int]` is `first` with the label `first..list_int`.
#[derive(Clone, Debug, PartialEq)]
struct BoundFunction {
    base: String,
    label: String,
}

/// A name a `match` case binds, the values it takes, one for each value of a tuple, and how each
/// is counted.
type Binding = (String, Vec<Operand>, Vec<Option<RcKind>>);

/// What [`Lowerer::unwrapped`] finds for `unwrap`, `unwrap_or`, or `unwrap_err`: the tag of the
/// variant whose value it gives, that value's position among the enum's type arguments, the
/// error it stops the program with otherwise, and, for a result, the position of the other
/// variant's value, which the error goes on with.
type Unwrapped = (usize, usize, &'static str, Option<usize>);

/// What a `for` loop walks: a range up to an end, or a list.
#[derive(Clone, Copy)]
enum Iterable {
    Range(Operand),
    List(Operand),
    /// A string or, if `bytes`, bytes, and its length, which cannot change while the loop holds
    /// it.
    Str(Operand, Operand, bool),
}

/// A loop that `break` and `cont` can leave.
struct Loop {
    exit: BlockId,
    header: BlockId,
    /// The list or string a `for` loop walks, which it holds a reference to until it ends.
    held: Option<(Operand, RcKind)>,
}

/// A block being built, which may not have a terminator yet.
struct PendingBlock {
    insts: Vec<Inst>,
    spans: Vec<Span>,
    term: Option<Terminator>,
    term_span: Span,
    loop_depth: u32,
}

/// Lowers one function body.
struct Lowerer<'a> {
    context: &'a Context<'a>,
    /// The function's label, or `None` for `main`.
    name: Option<String>,
    /// The function's stone name, which is its label except in a copy of a generic function,
    /// whose label also names its types.
    base: Option<String>,
    /// The type each type parameter stands for in a copy of a generic function, by name.
    substitution: HashMap<String, Type>,
    /// The type of each variable of a copy of a generic function, with its type parameters
    /// filled in, including one for each value of a tuple or struct.
    instance_variables: HashMap<String, Type>,
    /// The label a call through a bound resolved to in this copy, by the call's span, which
    /// [`Lowerer::callee_label`] gives instead of the trait's method.
    dispatched: Option<(Span, String)>,
    /// The function each parameter bound in this copy holds, by the parameter's name, which
    /// the copy takes no argument for.
    constants: HashMap<String, BoundFunction>,
    /// Each local's vreg. Empty in `main`, where assigned names are globals.
    locals: HashMap<String, VReg>,
    /// The locals that own a reference, which are released when the function returns.
    counted_locals: Vec<(VReg, RcKind)>,
    /// Temporaries that own a reference, which whatever uses them must give away or release.
    owned: HashMap<VReg, RcKind>,
    /// References an assignment took for its target but has not stored yet, while it evaluates
    /// the target's indexes, which a `?` there releases before returning.
    pending: Vec<(Operand, RcKind)>,
    vreg_count: u32,
    blocks: Vec<PendingBlock>,
    /// Block ids in the order they were started, which becomes the layout order.
    order: Vec<BlockId>,
    /// The block instructions go to, or `None` after a terminator, until the next block starts.
    current: Option<BlockId>,
    /// Each enclosing loop, innermost last.
    loops: Vec<Loop>,
    loop_depth: u32,
    /// The variables whose value the next call to a stone function, or elementwise arithmetic,
    /// may move into it, since nothing reads them after the call: the target of `x = f(...)`, or
    /// every counted local for `ret f(...)`.
    movable: Vec<String>,
    /// The locals and globals a call moved, whose old value the assignment or return that follows
    /// must not release.
    moved_locals: HashSet<VReg>,
    moved_globals: HashSet<String>,
    /// The variables still read after each statement of the body being lowered.
    live_after: LiveAfter,
    /// The variables the expression statement being lowered uses for the last time, which an
    /// `append` it makes may move into the list.
    statement_last_uses: Vec<String>,
    /// The innermost statement or expression being lowered, which each instruction records as
    /// where a runtime error at it points.
    span: Span,
    /// The values the `match` statements being lowered hold, which they release when they end,
    /// each with how many loops enclose it, so `ret` releases them all and `break` and `cont`
    /// those inside the loop they leave.
    held: Vec<(Operand, usize)>,
    /// How many enum values the value being printed is inside, which a printer of an enum
    /// passes on to the printers of the enum values it holds.
    print_depth: Operand,
    /// How many enum values the values being compared are inside, which a comparer of an enum
    /// passes on to the comparers of the enum values it holds.
    compare_depth: Operand,
    /// Whether this is a method that changes `self`, which returns the values of `self` after
    /// its own, for the caller to store back into the receiver.
    returns_self: bool,
    /// The keys that [`Lowerer::place_indexes`] evaluated to owned strings for the statement
    /// being lowered, which it releases once the change is made.
    place_keys: Vec<Operand>,
}

impl<'a> Lowerer<'a> {
    fn new(context: &'a Context<'a>, name: Option<String>) -> Self {
        Lowerer {
            context,
            base: name.clone(),
            name,
            substitution: HashMap::new(),
            instance_variables: HashMap::new(),
            dispatched: None,
            constants: HashMap::new(),
            locals: HashMap::new(),
            counted_locals: Vec::new(),
            owned: HashMap::new(),
            pending: Vec::new(),
            vreg_count: 0,
            blocks: Vec::new(),
            order: Vec::new(),
            current: None,
            loops: Vec::new(),
            loop_depth: 0,
            movable: Vec::new(),
            moved_locals: HashSet::new(),
            moved_globals: HashSet::new(),
            live_after: LiveAfter::new(),
            statement_last_uses: Vec::new(),
            place_keys: Vec::new(),
            span: Span::default(),
            held: Vec::new(),
            print_depth: Operand::Imm(0),
            compare_depth: Operand::Imm(0),
            returns_self: false,
        }
    }

    /// Returns a lowerer for `copy` of a function, whose type parameters, named `names`, stand
    /// for the copy's types, and whose parameters it binds hold their functions.
    fn new_instance(
        context: &'a Context<'a>,
        copy: &Request,
        names: &[String],
        args: &[Arg],
    ) -> Self {
        let instance = &copy.instance;
        let function = &instance.function;
        let substitution: HashMap<String, Type> =
            names.iter().cloned().zip(instance.args.clone()).collect();
        let mut lowerer = Lowerer::new(context, Some(copy.label()));
        lowerer.base = Some(function.clone());
        for (position, constant) in &copy.constants {
            if let Some(arg) = args.get(*position) {
                lowerer.constants.insert(arg.arg.clone(), constant.clone());
            }
        }
        let scope = Some(function.clone());
        for ((owner, name), ty) in context.variables {
            if *owner != scope {
                continue;
            }
            let ty = ty.substitute(&substitution);
            if ty.is_flat() {
                for (i, leaf) in ty.leaves(context.structs).into_iter().enumerate() {
                    lowerer.instance_variables.insert(leaf_name(name, i), leaf);
                }
            }
            lowerer.instance_variables.insert(name.clone(), ty);
        }
        lowerer.substitution = substitution;
        lowerer
    }

    /// Returns `ty` with the type parameters of this copy of a generic function filled in.
    fn fill(&self, ty: &Type) -> Type {
        match self.substitution.is_empty() {
            true => ty.clone(),
            false => ty.substitute(&self.substitution),
        }
    }

    /// Returns the type the checker gave the expression at `span`, with the type parameters of
    /// this copy of a generic function filled in.
    fn span_type(&self, span: Span) -> Option<Type> {
        self.context.types.get(&span).map(|ty| self.fill(ty))
    }

    /// Returns the label to call for the stone function `name` at the call `call`: its own, or
    /// for a generic function the label of its copy for the call's types, which this asks
    /// lowering to make.
    fn callee_label(&self, name: &str, call: Span) -> String {
        if let Some((span, label)) = &self.dispatched
            && *span == call
        {
            return label.clone();
        }
        let Some(instance) = self.context.instances.get(&call) else {
            return name.to_string();
        };
        let instance = Instance {
            function: instance.function.clone(),
            args: instance.args.iter().map(|a| self.fill(a)).collect(),
        };
        self.request(instance)
    }

    /// Returns the label of the copy `instance` of a generic function, asking lowering to make
    /// it.
    fn request(&self, instance: Instance) -> String {
        self.request_copy(Request {
            instance,
            constants: vec![],
        })
    }

    /// Returns the label of `copy`, asking lowering to make it.
    fn request_copy(&self, copy: Request) -> String {
        let label = copy.label();
        let mut requested = self.context.requested.borrow_mut();
        if !requested.contains(&copy) {
            requested.push(copy);
        }
        label
    }

    /// Returns the label to call for the stone function `name` at `call`, where the arguments
    /// `args` go to its parameters from `first` on, and which of those it binds: none, and the
    /// label [`Lowerer::callee_label`] gives, unless an argument is a function value known here,
    /// a function named as a value or a parameter this copy binds, for a parameter `name` never
    /// assigns. Then the label is of the copy that calls that function directly.
    ///
    /// For example, `apply(double, 1)` calls `apply..fn0.double`, binding parameter 0.
    fn specialized_label(
        &self,
        name: &str,
        call: Span,
        args: &[Expr],
        first: usize,
    ) -> (String, HashSet<usize>) {
        let through_bound = self
            .dispatched
            .as_ref()
            .is_some_and(|(span, _)| *span == call);
        let owned = self.context.owned_params.get(name);
        let mut constants = vec![];
        for (i, arg) in args.iter().enumerate() {
            let ExprKind::Name { id, .. } = &arg.kind else {
                continue;
            };
            if through_bound || owned.and_then(|owned| owned.get(first + i)) != Some(&false) {
                continue;
            }
            let constant = match self.context.function_values.get(&arg.span) {
                Some(base) => BoundFunction {
                    base: base.clone(),
                    label: self.callee_label(base, arg.span),
                },
                None => match self.constants.get(id) {
                    Some(constant) => constant.clone(),
                    None => continue,
                },
            };
            constants.push((first + i, constant));
        }
        if constants.is_empty() {
            return (self.callee_label(name, call), HashSet::new());
        }
        let instance = match self.context.instances.get(&call) {
            Some(instance) => Instance {
                function: instance.function.clone(),
                args: instance.args.iter().map(|a| self.fill(a)).collect(),
            },
            None => Instance {
                function: name.to_string(),
                args: vec![],
            },
        };
        let bound = constants.iter().map(|(i, _)| i - first).collect();
        (
            self.request_copy(Request {
                instance,
                constants,
            }),
            bound,
        )
    }

    fn function(mut self, args: &[Arg], body: &[Stmt]) -> Result<Function, String> {
        let mut params = Vec::new();
        let mut assigned = Vec::new();
        let mut changed = HashSet::new();
        if self.name.is_some() {
            // a tuple is passed as each of its values
            for arg in args {
                for leaf in self.leaf_names(&arg.arg) {
                    let reg = self.local(&leaf);
                    // a parameter this copy binds takes no argument
                    if !self.constants.contains_key(&arg.arg) {
                        params.push(reg);
                    }
                }
            }
            collect_assigned(body, &mut assigned);
            collect_changed(body, &mut changed, self.context.methods);
            for (name, _) in &assigned {
                for leaf in self.leaf_names(name) {
                    self.local(&leaf);
                }
            }
        }

        self.live_after = live_after(body, &self.context.global_reads, &kept_at_exit(args));
        // a method owns `self` exactly when it changes it
        self.returns_self = args.first().is_some_and(|arg| arg.arg == "self")
            && self.base.as_ref().is_some_and(|name| {
                self.context
                    .owned_params
                    .get(name)
                    .is_some_and(|owned| owned.first() == Some(&true))
            });
        let entry = self.new_block();
        self.start(entry);
        // the caller gives a parameter the function assigns or changes a reference of its own to
        // release, and other counted locals start null, since they may not be assigned on every
        // path
        let names: Vec<String> = args
            .iter()
            .map(|arg| &arg.arg)
            .chain(assigned.iter().map(|(name, _)| name))
            .flat_map(|name| self.leaf_names(name))
            .collect();
        let mut seen = HashSet::new();
        for name in &names {
            let reg = self.locals[name];
            let Some(kind) = self.variable_kind(name) else {
                continue;
            };
            let name = variable_of(name);
            if !seen.insert(reg) {
                continue;
            }
            if !params.contains(&reg) {
                self.push(Inst::Copy {
                    dst: reg,
                    src: Operand::Imm(0),
                });
            } else if !assigned.iter().any(|(assigned, _)| assigned == name)
                && !changed.contains(name)
            {
                continue;
            }
            self.counted_locals.push((reg, kind));
        }

        for stmt in body {
            self.stmt(stmt)?;
        }
        // falling off the end returns none, and main exits with 0
        if self.current.is_some() {
            self.leave(&[Operand::Imm(0)]);
        }

        Ok(Function {
            name: self.name.clone(),
            params,
            blocks: self.layout(),
            vreg_count: self.vreg_count,
            helper: false,
        })
    }

    /// Makes the function that prints a value of the enum type `ty` as the interpreter does, such
    /// as `Shape.Rect(2.0, 3.0)` or `some('a')`, given the value and how many enum values it is
    /// inside. Printing one nested too deeply stops the program, pointing at the `print` that
    /// called it, which recorded its site.
    fn enum_printer(mut self, ty: &Type) -> Result<Function, String> {
        let (value, depth) = (self.fresh(), self.fresh());
        let entry = self.new_block();
        self.start(entry);
        self.print_depth = self.enter_level(Operand::Reg(depth), PRINT_DEPTH);
        let all = variants(self.context.enums, ty);
        let tag = self.fresh();
        self.push(Inst::EnumTag {
            dst: tag,
            value: Operand::Reg(value),
        });
        for (k, (name, fields)) in all.iter().enumerate() {
            if k + 1 < all.len() {
                let (this, next) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Eq,
                    float: false,
                    lhs: Operand::Reg(tag),
                    rhs: Operand::Imm(k as i64),
                    then: this,
                    otherwise: next,
                });
                self.start(this);
                self.print_variant(ty, name, fields, Operand::Reg(value));
                self.terminate(Terminator::Return(vec![Operand::Imm(0)]));
                self.start(next);
            } else {
                self.print_variant(ty, name, fields, Operand::Reg(value));
                self.terminate(Terminator::Return(vec![Operand::Imm(0)]));
            }
        }
        Ok(Function {
            name: self.name.clone(),
            params: vec![value, depth],
            blocks: self.layout(),
            vreg_count: self.vreg_count,
            helper: true,
        })
    }

    /// Makes the function that prints a value of the struct type `ty` as the interpreter does,
    /// such as `Point(x=1.0, y=2.0)`, given the struct's values and how many enum and struct
    /// values it is inside. Printing one nested too deeply stops the program, as for enums.
    fn struct_printer(mut self, ty: &Type) -> Result<Function, String> {
        let Type::Struct { name, .. } = ty else {
            return Err("only a struct has fields to print".to_string());
        };
        let values: Vec<VReg> = self.leaves(ty).iter().map(|_| self.fresh()).collect();
        let depth = self.fresh();
        let entry = self.new_block();
        self.start(entry);
        self.print_depth = self.enter_level(Operand::Reg(depth), PRINT_DEPTH);
        self.print_text(&format!("{}(", struct_display(name)));
        let mut start = 0;
        for (k, (field, field_ty)) in ty.struct_fields(self.context.structs).iter().enumerate() {
            if k > 0 {
                self.print_char(',');
                self.print_char(' ');
            }
            self.print_text(&format!("{field}="));
            let end = start + self.leaves(field_ty).len();
            let field_values: Vec<Operand> = values[start..end]
                .iter()
                .copied()
                .map(Operand::Reg)
                .collect();
            self.print_flat(field_ty, &field_values, true);
            start = end;
        }
        self.print_char(')');
        self.terminate(Terminator::Return(vec![Operand::Imm(0)]));
        let mut params = values;
        params.push(depth);
        Ok(Function {
            name: self.name.clone(),
            params,
            blocks: self.layout(),
            vreg_count: self.vreg_count,
            helper: true,
        })
    }

    /// Stops the program with `message` if a printer or comparer was called inside `depth` enum
    /// and struct values, which is more than it may go into, and otherwise returns the depth one
    /// level deeper, for the helpers it calls.
    fn enter_level(&mut self, depth: Operand, message: &str) -> Operand {
        let (deep, shallow) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Ge,
            float: false,
            lhs: depth,
            rhs: Operand::Imm(MAX_PRINT_DEPTH as i64),
            then: deep,
            otherwise: shallow,
        });
        self.start(deep);
        self.terminate(Terminator::Fail(message.to_string(), vec![]));
        self.start(shallow);
        let inner = self.fresh();
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: inner,
            lhs: depth,
            rhs: Operand::Imm(1),
        });
        Operand::Reg(inner)
    }

    /// Makes the function that compares two values of the enum type `ty` with `==`, given the two
    /// and how many enum values they are inside, returning 1 if they are equal and 0 if not: the
    /// same variant, and then the same values, compared in turn until two differ. Comparing ones
    /// nested too deeply stops the program, pointing at the comparison that called it, which
    /// recorded its site.
    fn enum_comparer(mut self, ty: &Type) -> Result<Function, String> {
        let (a, b, depth) = (self.fresh(), self.fresh(), self.fresh());
        let entry = self.new_block();
        self.start(entry);
        self.compare_depth = self.enter_level(Operand::Reg(depth), COMPARE_DEPTH);
        let (tag_a, tag_b) = (self.fresh(), self.fresh());
        self.push(Inst::EnumTag {
            dst: tag_a,
            value: Operand::Reg(a),
        });
        self.push(Inst::EnumTag {
            dst: tag_b,
            value: Operand::Reg(b),
        });
        let (same, equal, differ) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(tag_a),
            rhs: Operand::Reg(tag_b),
            then: same,
            otherwise: differ,
        });
        self.start(same);
        // a variant without values is equal to itself, so only the others need their own block
        for (k, (_, fields)) in variants(self.context.enums, ty).iter().enumerate() {
            let leaves: Vec<Type> = fields.iter().flat_map(|field| self.leaves(field)).collect();
            if leaves.is_empty() {
                continue;
            }
            let compared: Vec<Compared> = fields.iter().flat_map(|f| self.compared(f)).collect();
            let (this, next) = (self.new_block(), self.new_block());
            self.terminate(Terminator::CmpBranch {
                cond: Cond::Eq,
                float: false,
                lhs: Operand::Reg(tag_a),
                rhs: Operand::Imm(k as i64),
                then: this,
                otherwise: next,
            });
            self.start(this);
            // comparing changes nothing, so the values are borrowed
            let lhs = self.field_values(Operand::Reg(a), 0, leaves.len());
            let rhs = self.field_values(Operand::Reg(b), 0, leaves.len());
            self.equal_leaves(&compared, &lhs, &rhs, equal, differ);
            self.start(next);
        }
        self.jump(equal);
        self.start(equal);
        self.terminate(Terminator::Return(vec![Operand::Imm(1)]));
        self.start(differ);
        self.terminate(Terminator::Return(vec![Operand::Imm(0)]));
        Ok(Function {
            name: self.name.clone(),
            params: vec![a, b, depth],
            blocks: self.layout(),
            vreg_count: self.vreg_count,
            helper: true,
        })
    }

    /// Asks for `helper` to be made, unless something asked for it before, and returns its label.
    fn helper(&self, helper: Helper) -> String {
        let label = helper.label();
        let mut helpers = self.context.helpers.borrow_mut();
        if !helpers.contains(&helper) {
            helpers.push(helper);
        }
        label
    }

    /// Jumps to `then` if `lhs` and `rhs`, the values of two values of type `ty`, are equal, and
    /// to `otherwise` as soon as two of them differ, comparing them in turn.
    fn equal_branch(
        &mut self,
        ty: &Type,
        lhs: &[Operand],
        rhs: &[Operand],
        then: BlockId,
        otherwise: BlockId,
    ) {
        let compared = self.compared(ty);
        self.equal_leaves(&compared, lhs, rhs, then, otherwise);
    }

    /// Returns whether the struct type `ty` implements the trait `name`, as `eq` or `ord`.
    fn implements(&self, ty: &Type, name: &str) -> bool {
        let Type::Struct { name: owner, .. } = ty else {
            return false;
        };
        self.context
            .implemented
            .get(owner)
            .is_some_and(|traits| traits.iter().any(|t| t == name))
    }

    /// Returns what `==` compares in turn for values of type `ty`, in the order of their values:
    /// each value on its own, except the values of a struct that implements `eq`, which its
    /// `eq` compares together.
    fn compared(&self, ty: &Type) -> Vec<Compared> {
        if self.implements(ty, EQ) {
            return vec![Compared::Custom(ty.clone(), self.leaves(ty).len())];
        }
        match ty {
            Type::Tuple(fields) => fields.iter().flat_map(|f| self.compared(f)).collect(),
            Type::Struct { .. } => ty
                .struct_fields(self.context.structs)
                .iter()
                .flat_map(|(_, f)| self.compared(f))
                .collect(),
            _ => vec![Compared::Value(ty.clone())],
        }
    }

    /// Calls the method `name` of the struct type `ty`, which takes its values `lhs` as `self`
    /// and `rhs` as its other value, borrowing both unless it owns them, as `eq` and `cmp` do,
    /// and returns its result. `display` takes no other value, so its `rhs` is empty.
    fn compare_call(&mut self, ty: &Type, name: &str, lhs: &[Operand], rhs: &[Operand]) -> VReg {
        let Type::Struct { name: owner, args } = ty else {
            unreachable!("only a struct has its own '{name}'")
        };
        let function = format!("{owner}.{name}");
        let label = match args.is_empty() {
            true => function.clone(),
            false => self.request(Instance {
                function: function.clone(),
                args: args.clone(),
            }),
        };
        // a parameter it assigns is its own, so it gets a reference
        let owned = self
            .context
            .owned_params
            .get(&function)
            .cloned()
            .unwrap_or_default();
        let kinds: Vec<Option<RcKind>> = self.leaves(ty).iter().map(RcKind::of).collect();
        for (i, values) in [lhs, rhs].into_iter().enumerate() {
            if owned.get(i) == Some(&true) {
                for (&value, kind) in values.iter().zip(&kinds) {
                    if kind.is_some() {
                        self.push(Inst::Retain { src: value });
                    }
                }
            }
        }
        let result = self.fresh();
        self.push(Inst::Call {
            dsts: vec![result],
            callee: Callee::User(label),
            args: lhs.iter().chain(rhs).copied().collect(),
        });
        result
    }

    /// Jumps to `then` if each of `lhs` equals the one of `rhs` in the same place, where each
    /// pair is what `compared` says, and to `otherwise` at the first pair that differs:
    /// numbers and bools compare directly, strings by their contents, enum values through a
    /// comparer made for their type, and the values of a struct that implements `eq` by its
    /// `eq`.
    fn equal_leaves(
        &mut self,
        compared: &[Compared],
        lhs: &[Operand],
        rhs: &[Operand],
        then: BlockId,
        otherwise: BlockId,
    ) {
        if compared.is_empty() {
            self.jump(then);
            return;
        }
        let mut k = 0;
        for (i, part) in compared.iter().enumerate() {
            let next = if i + 1 < compared.len() {
                self.new_block()
            } else {
                then
            };
            let leaf = match part {
                Compared::Custom(ty, count) => {
                    let range = k..k + count;
                    let equal = self.compare_call(ty, "eq", &lhs[range.clone()], &rhs[range]);
                    self.terminate(Terminator::Branch {
                        cond: Operand::Reg(equal),
                        then: next,
                        otherwise,
                    });
                    k += count;
                    if next != then {
                        self.start(next);
                    }
                    continue;
                }
                Compared::Value(leaf) => leaf,
            };
            match leaf {
                Type::Str | Type::Bytes => {
                    let routine = equality_routine(leaf);
                    let equal = self.runtime_call(routine, vec![lhs[k], rhs[k]], None);
                    self.terminate(Terminator::Branch {
                        cond: equal,
                        then: next,
                        otherwise,
                    });
                }
                Type::Enum { .. } => {
                    let label = self.helper(Helper::Equal(leaf.clone()));
                    let equal = self.fresh();
                    self.push(Inst::Call {
                        dsts: vec![equal],
                        callee: Callee::Helper(label),
                        args: vec![lhs[k], rhs[k], self.compare_depth],
                    });
                    self.terminate(Terminator::Branch {
                        cond: Operand::Reg(equal),
                        then: next,
                        otherwise,
                    });
                }
                _ => self.terminate(Terminator::CmpBranch {
                    cond: Cond::Eq,
                    float: *leaf == Type::Float,
                    lhs: lhs[k],
                    rhs: rhs[k],
                    then: next,
                    otherwise,
                }),
            }
            k += 1;
            if next != then {
                self.start(next);
            }
        }
    }

    /// Prints `value`, a value of the variant `name` of the enum type `ty` whose values have the
    /// types `fields`, with its values in parentheses, the way the interpreter writes it.
    fn print_variant(&mut self, ty: &Type, name: &str, fields: &[Type], value: Operand) {
        let text = self.fresh();
        self.push(Inst::StrAddr {
            dst: text,
            text: variant_display(ty, name),
        });
        self.push(Inst::Call {
            dsts: vec![],
            callee: Callee::Print(Type::Str),
            args: vec![Operand::Reg(text)],
        });
        if fields.is_empty() {
            return;
        }
        self.print_char('(');
        let mut slot = 0;
        for (k, field) in fields.iter().enumerate() {
            if k > 0 {
                self.print_char(',');
                self.print_char(' ');
            }
            // printing changes nothing, so the values are borrowed
            let width = field.leaves(self.context.structs).len();
            let values = self.field_values(value, slot, width);
            self.print_flat(field, &values, true);
            slot += width;
        }
        self.print_char(')');
    }

    /// Reads slots `slot` up to `slot + width` of the enum value `value`, borrowed.
    fn field_values(&mut self, value: Operand, slot: usize, width: usize) -> Vec<Operand> {
        (slot..slot + width)
            .map(|slot| {
                let dst = self.fresh();
                self.push(Inst::FieldGet { dst, value, slot });
                Operand::Reg(dst)
            })
            .collect()
    }

    /// Returns the vreg of local `name`, creating it the first time.
    fn local(&mut self, name: &str) -> VReg {
        if let Some(reg) = self.locals.get(name) {
            return *reg;
        }
        let reg = self.fresh();
        self.locals.insert(name.to_string(), reg);
        reg
    }

    /// Returns the type of the stone variable `name` where this code reads it: a local of this
    /// function, or else a global.
    fn variable_type(&self, name: &str) -> Option<&Type> {
        if let Some(ty) = self.instance_variables.get(name) {
            return Some(ty);
        }
        self.context
            .variables
            .get(&(self.base.clone(), name.to_string()))
            .or_else(|| self.context.variables.get(&(None, name.to_string())))
    }

    /// Returns the variables lowering keeps for the stone variable `name`: one for each value of
    /// a tuple or struct, or `name` itself.
    ///
    /// For example, a `t` of type `tuple[int, tuple[str, float]]` is `t.0`, `t.1`, and `t.2`.
    fn leaf_names(&self, name: &str) -> Vec<String> {
        match self.variable_type(name) {
            Some(ty) if ty.is_flat() => (0..self.leaves(ty).len())
                .map(|i| leaf_name(name, i))
                .collect(),
            _ => vec![name.to_string()],
        }
    }

    /// Returns the values a value of type `ty` is made of, from [`Type::leaves`].
    fn leaves(&self, ty: &Type) -> Vec<Type> {
        ty.leaves(self.context.structs)
    }

    /// Returns how each value of `expr` is counted: one for each value of a tuple or struct, or
    /// one.
    fn leaf_kinds(&self, expr: &Expr) -> Result<Vec<Option<RcKind>>, String> {
        let ty = self.type_of(expr)?;
        Ok(self.leaves(&ty).iter().map(RcKind::of).collect())
    }

    /// Returns how variable `name` of this function, or global `name` in `main`, is counted.
    fn variable_kind(&self, name: &str) -> Option<RcKind> {
        if let Some(ty) = self.instance_variables.get(name) {
            return RcKind::of(ty);
        }
        self.context
            .variables
            .get(&(self.base.clone(), name.to_string()))
            .and_then(RcKind::of)
    }

    /// Returns how global `name` is counted.
    fn global_kind(&self, name: &str) -> Option<RcKind> {
        self.context
            .variables
            .get(&(None, name.to_string()))
            .and_then(RcKind::of)
    }

    /// Returns how the value of `expr` is counted.
    fn counted(&self, expr: &Expr) -> Option<RcKind> {
        self.span_type(expr.span).as_ref().and_then(RcKind::of)
    }

    /// Returns how many values each element of the list `expr` holds, one for each value of a
    /// tuple, and how each is counted, two bits apiece, as the list's header records them (see
    /// `builtins::list_runtime`).
    ///
    /// For example, a `list[str]` is `(1, 1)`, and a `list[tuple[int, str, list[int]]]` is
    /// `(3, 0b10_01_00)`.
    fn element_layout(&self, expr: &Expr) -> (usize, i64) {
        let kinds = self.element_kinds(expr);
        (kinds.len(), RcKind::codes(&kinds))
    }

    /// Returns how each value of an element of the list `expr` is counted: one for each value of
    /// a tuple, or one.
    fn element_kinds(&self, expr: &Expr) -> Vec<Option<RcKind>> {
        match self.span_type(expr.span) {
            Some(Type::List(elem)) => self.leaves(&elem).iter().map(RcKind::of).collect(),
            // an entry of a map is its key, then its value
            Some(Type::Map(key, value)) => std::iter::once(RcKind::of(&key))
                .chain(self.leaves(&value).iter().map(RcKind::of))
                .collect(),
            _ => vec![None],
        }
    }

    /// Returns whether each element of the list `expr` is several values lowering keeps apart:
    /// a tuple or a struct.
    fn holds_flat(&self, expr: &Expr) -> bool {
        matches!(self.span_type(expr.span), Some(Type::List(elem)) if elem.is_flat())
    }

    /// Returns the type of the keys of `expr` if it is a map.
    fn map_key(&self, expr: &Expr) -> Option<Type> {
        match self.span_type(expr.span) {
            Some(Type::Map(key, _)) => Some(*key),
            _ => None,
        }
    }

    /// Returns whether the place `expr` steps into a map's entry on its way from its variable,
    /// as `m[k]`, `m[k].x`, and `grid[i][k]` for a list of maps do, which
    /// [`Lowerer::locate`] walks.
    fn through_map(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Subscript { value, .. } => {
                self.map_key(value).is_some() || self.through_map(value)
            }
            ExprKind::Attribute { value, .. } => self.through_map(value),
            _ => false,
        }
    }

    /// Returns how the elements of the list `expr` are counted.
    fn element_kind(&self, expr: &Expr) -> Option<RcKind> {
        match self.span_type(expr.span) {
            Some(Type::List(elem)) => RcKind::of(&elem),
            _ => None,
        }
    }

    fn fresh(&mut self) -> VReg {
        let reg = VReg(self.vreg_count);
        self.vreg_count += 1;
        reg
    }

    /// Returns `into` if the caller wants the result there, or else a fresh vreg.
    fn dst(&mut self, into: Option<VReg>) -> VReg {
        into.unwrap_or_else(|| self.fresh())
    }

    /// Moves `value` into `into` when the caller asked for a destination, and returns where the
    /// result is. A temporary's reference moves with it.
    fn finish(&mut self, value: Operand, into: Option<VReg>) -> Operand {
        match into {
            Some(dst) => {
                if value != Operand::Reg(dst) {
                    self.push(Inst::Copy { dst, src: value });
                    if let Operand::Reg(src) = value
                        && let Some(kind) = self.owned.remove(&src)
                    {
                        self.owned.insert(dst, kind);
                    }
                }
                Operand::Reg(dst)
            }
            None => value,
        }
    }

    /// Records that `value`, a fresh result, owns a reference if it is counted as `kind`.
    fn own(&mut self, value: Operand, kind: Option<RcKind>) {
        if let (Operand::Reg(reg), Some(kind)) = (value, kind) {
            self.owned.insert(reg, kind);
        }
    }

    /// Returns each of `values` that is a temporary owning a reference, with how it is counted,
    /// still owned.
    fn owned_values(&self, values: &[Operand]) -> Vec<(Operand, RcKind)> {
        values
            .iter()
            .filter_map(|&value| match value {
                Operand::Reg(reg) => self.owned.get(&reg).map(|&kind| (value, kind)),
                Operand::Imm(_) => None,
            })
            .collect()
    }

    /// Releases `value` if it is a temporary that owns a reference, once nothing needs it.
    fn release_temp(&mut self, value: Operand) {
        if let Operand::Reg(reg) = value
            && let Some(kind) = self.owned.remove(&reg)
        {
            self.push(Inst::Release { src: value, kind });
        }
    }

    /// Makes `value`, counted as `kind`, carry a reference for whatever stores it: a temporary
    /// gives up its own, and anything else is retained.
    fn take(&mut self, value: Operand, kind: Option<RcKind>) {
        if kind.is_none() {
            return;
        }
        if let Operand::Reg(reg) = value
            && self.owned.remove(&reg).is_some()
        {
            return;
        }
        self.push(Inst::Retain { src: value });
    }

    /// Returns whether `value` is a local variable's vreg, which a later assignment can change.
    fn is_variable(&self, value: Operand) -> bool {
        matches!(value, Operand::Reg(reg) if self.locals.values().any(|&local| local == reg))
    }

    /// Returns `value` in a vreg that nothing else writes, copying a local variable so that
    /// reassigning the variable cannot change it.
    fn snapshot(&mut self, value: Operand) -> Operand {
        if self.is_variable(value) {
            let copy = self.fresh();
            self.push(Inst::Copy {
                dst: copy,
                src: value,
            });
            Operand::Reg(copy)
        } else {
            value
        }
    }

    fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        self.span_type(expr.span)
            .ok_or_else(|| format!("expression at {:?} has no type", expr.span.start))
    }

    fn new_block(&mut self) -> BlockId {
        self.blocks.push(PendingBlock {
            insts: Vec::new(),
            spans: Vec::new(),
            term: None,
            term_span: Span::default(),
            loop_depth: 0,
        });
        BlockId(self.blocks.len() - 1)
    }

    /// Makes `block` the current block, placing it next in layout order. If the previous block
    /// has no terminator yet, control falls through into `block`.
    fn start(&mut self, block: BlockId) {
        if self.current.is_some() {
            self.terminate(Terminator::Jump(block));
        }
        self.blocks[block.0].loop_depth = self.loop_depth;
        self.order.push(block);
        self.current = Some(block);
    }

    fn push(&mut self, inst: Inst) {
        // code after `ret`, `break`, or `cont` is unreachable, but still lowered
        let block = match self.current {
            Some(block) => block,
            None => {
                let block = self.new_block();
                self.start(block);
                block
            }
        };
        self.blocks[block.0].insts.push(inst);
        self.blocks[block.0].spans.push(self.span);
    }

    fn terminate(&mut self, term: Terminator) {
        let block = match self.current.take() {
            Some(block) => block,
            None => {
                let block = self.new_block();
                self.start(block);
                self.current = None;
                block
            }
        };
        self.blocks[block.0].term = Some(term);
        self.blocks[block.0].term_span = self.span;
    }

    fn jump(&mut self, target: BlockId) {
        self.terminate(Terminator::Jump(target));
    }

    /// Returns `values` from the function, one for each value of a tuple, first releasing the
    /// lists and strings of the loops it is in and the references its locals own, except those
    /// among `values`, which the caller takes. `main` releases every global instead and exits
    /// with 0.
    fn leave(&mut self, values: &[Operand]) {
        let mut values = values.to_vec();
        if self.returns_self {
            values.extend(self.self_leaves());
        }
        let values = values.as_slice();
        let held: Vec<(Operand, RcKind)> = self.loops.iter().rev().filter_map(|l| l.held).collect();
        for (src, kind) in held {
            self.push(Inst::Release { src, kind });
        }
        self.release_held(0);
        if self.name.is_some() {
            let moved = std::mem::take(&mut self.moved_locals);
            for (reg, kind) in self.counted_locals.clone() {
                // a local moved into a call no longer holds a reference
                if values.contains(&Operand::Reg(reg)) || moved.contains(&reg) {
                    continue;
                }
                self.push(Inst::Release {
                    src: Operand::Reg(reg),
                    kind,
                });
            }
            self.terminate(Terminator::Return(values.to_vec()));
            return;
        }
        for name in self.context.globals {
            let Some(kind) = self.global_kind(name) else {
                continue;
            };
            let global = self.fresh();
            self.push(Inst::LoadGlobal {
                dst: global,
                name: name.clone(),
                checked: false,
            });
            self.push(Inst::Release {
                src: Operand::Reg(global),
                kind,
            });
        }
        self.terminate(Terminator::Return(vec![Operand::Imm(0)]));
    }

    /// Returns the values of `self` in a method, each a local.
    fn self_leaves(&self) -> Vec<Operand> {
        self.leaf_names("self")
            .iter()
            .filter_map(|name| self.locals.get(name))
            .map(|&local| Operand::Reg(local))
            .collect()
    }

    /// Releases each value a `match` holds inside at least `loops` loops, innermost first, where
    /// `ret` leaves them all and `break` or `cont` the innermost loop.
    fn release_held(&mut self, loops: usize) {
        let held: Vec<Operand> = self
            .held
            .iter()
            .rev()
            .filter(|(_, depth)| *depth >= loops)
            .map(|(value, _)| *value)
            .collect();
        for src in held {
            self.push(Inst::Release {
                src,
                kind: RcKind::Enum,
            });
        }
    }

    /// Renumbers the blocks into the order they were started.
    fn layout(&mut self) -> Vec<Block> {
        // a block nothing started can only be an unused target, so it goes last
        for id in 0..self.blocks.len() {
            if !self.order.contains(&BlockId(id)) {
                self.order.push(BlockId(id));
            }
        }
        let mut position = vec![0; self.blocks.len()];
        for (pos, id) in self.order.iter().enumerate() {
            position[id.0] = pos;
        }
        let renumber = |id: BlockId| BlockId(position[id.0]);

        let mut pending: Vec<Option<PendingBlock>> = std::mem::take(&mut self.blocks)
            .into_iter()
            .map(Some)
            .collect();
        self.order
            .iter()
            .map(|id| {
                let block = pending[id.0].take().expect("each block is placed once");
                let term = match block
                    .term
                    .unwrap_or(Terminator::Return(vec![Operand::Imm(0)]))
                {
                    Terminator::Jump(target) => Terminator::Jump(renumber(target)),
                    Terminator::Branch {
                        cond,
                        then,
                        otherwise,
                    } => Terminator::Branch {
                        cond,
                        then: renumber(then),
                        otherwise: renumber(otherwise),
                    },
                    Terminator::CmpBranch {
                        cond,
                        float,
                        lhs,
                        rhs,
                        then,
                        otherwise,
                    } => Terminator::CmpBranch {
                        cond,
                        float,
                        lhs,
                        rhs,
                        then: renumber(then),
                        otherwise: renumber(otherwise),
                    },
                    term @ (Terminator::Return(_) | Terminator::Fail(..)) => term,
                };
                Block {
                    insts: block.insts,
                    spans: block.spans,
                    term,
                    term_span: block.term_span,
                    loop_depth: block.loop_depth,
                }
            })
            .collect()
    }

    /// Lowers `stmt`, whose instructions point at it unless an expression inside it is more
    /// precise.
    fn stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        let outer = std::mem::replace(&mut self.span, stmt.span);
        let result = self.stmt_unspanned(stmt);
        // a change through a map's entry may have evaluated a key to a new string
        for key in std::mem::take(&mut self.place_keys) {
            if self.current.is_some() {
                self.release_temp(key);
            }
        }
        if result.is_ok() && self.current.is_some() {
            self.release_handles(stmt);
        }
        self.span = outer;
        result
    }

    /// Releases each variable that holds a socket or listener and that `stmt` used for the last
    /// time, leaving it null, so the connection closes right after the statement, where the
    /// interpreter drops it, and the other end sees it close at the same point in both backends.
    /// A variable `stmt` moved into a call is left to the callee.
    fn release_handles(&mut self, stmt: &Stmt) {
        let holds = |name: &str| {
            self.variable_type(variable_of(name)).is_some_and(|ty| {
                holds_handle(
                    ty,
                    self.context.structs,
                    self.context.enums,
                    &mut HashSet::new(),
                )
            })
        };
        if !self.context.handles {
            return;
        }
        let names: Vec<String> = self
            .last_uses(stmt)
            .into_iter()
            .filter(|name| holds(name))
            .collect();
        for name in names {
            match self.locals.get(&name) {
                Some(&local) => {
                    if self.moved_locals.contains(&local) {
                        continue;
                    }
                    let Some(&(_, kind)) =
                        self.counted_locals.iter().find(|(reg, _)| *reg == local)
                    else {
                        continue;
                    };
                    self.push(Inst::Release {
                        src: Operand::Reg(local),
                        kind,
                    });
                    self.push(Inst::Copy {
                        dst: local,
                        src: Operand::Imm(0),
                    });
                }
                None => {
                    let Some(kind) = self.global_kind(&name) else {
                        continue;
                    };
                    if self.moved_globals.contains(&name) {
                        continue;
                    }
                    let global = self.fresh();
                    self.push(Inst::LoadGlobal {
                        dst: global,
                        name: name.clone(),
                        checked: false,
                    });
                    self.push(Inst::Release {
                        src: Operand::Reg(global),
                        kind,
                    });
                    self.push(Inst::StoreGlobal {
                        name,
                        src: Operand::Imm(0),
                    });
                }
            }
        }
    }

    fn stmt_unspanned(&mut self, stmt: &Stmt) -> Result<(), String> {
        match &stmt.kind {
            StmtKind::Assign { targets, value } if self.is_flat(value) => {
                self.assign_tuple(stmt, targets, value)?;
            }

            StmtKind::Assign { targets, value } => {
                let last_uses = self.last_uses(stmt);
                // a call can also take a target's old value, which the assignment replaces, but
                // not what a target's indexes read after it
                if (self.is_stone_call(value) || self.is_elementwise(value))
                    && targets.iter().all(is_variable_target)
                {
                    self.movable = last_uses.clone();
                    for target in targets {
                        for source in self.flat_sources(target).into_iter().flatten() {
                            if self.is_counted_variable(&source) {
                                self.movable.push(source);
                            }
                        }
                    }
                }
                let kind = self.counted(value);
                if kind.is_none()
                    && let [target] = targets.as_slice()
                    && let ExprKind::Name { id, .. } = &target.kind
                    && let Some(&local) = self.locals.get(id)
                {
                    self.expr_into(value, Some(local))?;
                } else {
                    let result = self.expr(value)?;
                    // every target gets a reference of its own, the last one the temporary's
                    for (i, target) in targets.iter().enumerate() {
                        if i + 1 < targets.len() {
                            if kind.is_some() {
                                self.push(Inst::Retain { src: result });
                            }
                        } else {
                            self.take_from(value, result, kind, &last_uses);
                        }
                        // a `?` in the target's indexes drops the reference it would store
                        if let Some(kind) = kind {
                            self.pending.push((result, kind));
                        }
                        self.assign(target, result, kind)?;
                        if kind.is_some() {
                            self.pending.pop();
                        }
                    }
                }
                self.clear_moved();
            }

            StmtKind::AugAssign { target, op, value } => {
                self.aug_assign(stmt, target, *op, value)?;
            }

            StmtKind::Expr { value } => {
                let last_uses = self.last_uses(stmt);
                if self.is_stone_call(value) {
                    self.movable = last_uses.clone();
                }
                // `xs.append(row)` can take `row` at its last use
                self.statement_last_uses = last_uses;
                let results = self.expr_flat(value)?;
                self.statement_last_uses.clear();
                for result in results {
                    self.release_temp(result);
                }
                self.clear_moved();
            }

            StmtKind::Return { value } => {
                let values = match value {
                    // `ret` at the top level ends the program normally
                    Some(value) if self.name.is_none() => {
                        for result in self.expr_flat(value)? {
                            self.release_temp(result);
                        }
                        vec![Operand::Imm(0)]
                    }
                    Some(value) => {
                        // returning releases every local, so a call can take any of them
                        if self.is_stone_call(value) || self.is_elementwise(value) {
                            self.movable = self.last_uses(stmt);
                        }
                        let kinds = self.leaf_kinds(value)?;
                        let results = self.expr_flat(value)?;
                        // a counted local gives its own reference to the caller, once, so that
                        // `ret (s, s)` gives a second one, and `self`, which a method that
                        // changes it returns too, gives its own there
                        let mut given: HashSet<Operand> = match self.returns_self {
                            true => self.self_leaves().into_iter().collect(),
                            false => HashSet::new(),
                        };
                        for (&result, kind) in results.iter().zip(kinds) {
                            let local = self
                                .counted_locals
                                .iter()
                                .any(|(reg, _)| result == Operand::Reg(*reg));
                            if !(local && given.insert(result)) {
                                self.take(result, kind);
                            }
                        }
                        results
                    }
                    None => vec![Operand::Imm(0)],
                };
                self.leave(&values);
            }

            StmtKind::If { test, body, orelse } => {
                let then = self.new_block();
                let end = self.new_block();
                let otherwise = if orelse.is_empty() {
                    end
                } else {
                    self.new_block()
                };
                self.cond(test, then, otherwise)?;
                self.start(then);
                for stmt in body {
                    self.stmt(stmt)?;
                }
                if !orelse.is_empty() {
                    if self.current.is_some() {
                        self.jump(end);
                    }
                    self.start(otherwise);
                    for stmt in orelse {
                        self.stmt(stmt)?;
                    }
                }
                self.start(end);
            }

            StmtKind::While { test, body } => {
                let header = self.new_block();
                let loop_body = self.new_block();
                let exit = self.new_block();
                self.loop_depth += 1;
                self.start(header);
                self.cond(test, loop_body, exit)?;
                self.start(loop_body);
                self.loops.push(Loop {
                    exit,
                    header,
                    held: None,
                });
                for stmt in body {
                    self.stmt(stmt)?;
                }
                self.loops.pop();
                if self.current.is_some() {
                    self.jump(header);
                }
                self.loop_depth -= 1;
                self.start(exit);
            }

            StmtKind::For { target, iter, body } => self.for_loop(target, iter, body)?,

            StmtKind::Break => {
                if let Some(exit) = self.loops.last().map(|l| l.exit) {
                    self.release_held(self.loops.len());
                    self.jump(exit);
                }
            }

            StmtKind::Continue => {
                if let Some(header) = self.loops.last().map(|l| l.header) {
                    self.release_held(self.loops.len());
                    self.jump(header);
                }
            }

            StmtKind::FunctionDef { .. } | StmtKind::ImplDef { .. } | StmtKind::TraitDef { .. } => {
                return Err("functions must be defined at the top level".to_string());
            }
            StmtKind::EnumDef { .. } | StmtKind::StructDef { .. } => {}
            StmtKind::Match { subject, cases } => self.match_stmt(subject, cases)?,
            StmtKind::Use { .. } => {
                return Err("use is only allowed at the top level of a file".to_string());
            }
        }
        Ok(())
    }

    /// Lowers a `match`: its subject once, then each case's pattern in turn, a chain of tag
    /// checks down the variants it names, and the first case whose pattern fits binds its names
    /// and runs. A subject that is not a variable is held until the `match` ends, so the values
    /// bound from it are retained before anything can release it.
    ///
    /// For example, `case some(x):` checks `enum_tag v0` against 1, then binds `x` to
    /// `field_get v0, 0`.
    fn match_stmt(&mut self, subject: &Expr, cases: &[MatchCase]) -> Result<(), String> {
        let ty = self.type_of(subject)?;
        let value = self.expr(subject)?;
        // a case can assign the variable it matches, so it is read through a copy
        let value = self.snapshot(value);
        let held = match value {
            Operand::Reg(reg) if self.owned.remove(&reg).is_some() => {
                self.held.push((value, self.loops.len()));
                true
            }
            _ => false,
        };
        let end = self.new_block();
        for case in cases {
            let next = self.new_block();
            self.pattern_test(&case.pattern, value, &ty, next);
            // every value is read and retained before any name is assigned, since assigning
            // one can release the value the others come from
            let mut bindings = Vec::new();
            self.pattern_bindings(&case.pattern, &[value], &ty, &mut bindings);
            for (_, values, kinds) in &bindings {
                for (&value, &kind) in values.iter().zip(kinds) {
                    self.take(value, kind);
                }
            }
            for (name, values, kinds) in bindings {
                let leaves = self.leaf_names(&name);
                for ((leaf, value), kind) in leaves.iter().zip(values).zip(kinds) {
                    self.assign_name(leaf, value, kind);
                }
            }
            for stmt in &case.body {
                self.stmt(stmt)?;
            }
            if self.current.is_some() {
                self.jump(end);
            }
            self.start(next);
        }
        // the checker made sure some case fits every value
        self.terminate(Terminator::Fail(
            "no case of the match fits the value".to_string(),
            vec![],
        ));
        self.start(end);
        if held {
            self.held.pop();
            self.push(Inst::Release {
                src: value,
                kind: RcKind::Enum,
            });
        }
        Ok(())
    }

    /// Checks that `value`, of type `ty`, fits `pattern`, going on in a new block if it does and
    /// to `otherwise` if not. Only variants check anything, each its tag, then its values that
    /// are variants in turn.
    fn pattern_test(&mut self, pattern: &Pattern, value: Operand, ty: &Type, otherwise: BlockId) {
        let PatternKind::Variant { path, fields } = &pattern.kind else {
            return;
        };
        let all = variants(self.context.enums, ty);
        let Some(tag) = path
            .last()
            .and_then(|(name, _)| all.iter().position(|(variant, _)| variant == name))
        else {
            return;
        };
        if all.len() > 1 {
            let found = self.fresh();
            self.push(Inst::EnumTag { dst: found, value });
            let fits = self.new_block();
            self.terminate(Terminator::CmpBranch {
                cond: Cond::Eq,
                float: false,
                lhs: Operand::Reg(found),
                rhs: Operand::Imm(tag as i64),
                then: fits,
                otherwise,
            });
            self.start(fits);
        }
        let mut slot = 0;
        for (field, field_ty) in fields.iter().zip(&all[tag].1) {
            if let PatternKind::Variant { .. } = field.kind {
                // a variant's type is an enum, which is one slot
                let inner = self.fresh();
                self.push(Inst::FieldGet {
                    dst: inner,
                    value,
                    slot,
                });
                self.pattern_test(field, Operand::Reg(inner), field_ty, otherwise);
            }
            slot += self.leaves(field_ty).len();
        }
    }

    /// Pushes each name `pattern` binds, with the values of `values`, of type `ty`, it takes,
    /// borrowed, and how each is counted.
    fn pattern_bindings(
        &mut self,
        pattern: &Pattern,
        values: &[Operand],
        ty: &Type,
        out: &mut Vec<Binding>,
    ) {
        match &pattern.kind {
            PatternKind::Wildcard => {}
            PatternKind::Name(name) => {
                let kinds = self.leaves(ty).iter().map(RcKind::of).collect();
                out.push((name.clone(), values.to_vec(), kinds));
            }
            PatternKind::Variant { path, fields } => {
                let all = variants(self.context.enums, ty);
                let Some(tag) = path
                    .last()
                    .and_then(|(name, _)| all.iter().position(|(variant, _)| variant == name))
                else {
                    return;
                };
                let mut slot = 0;
                for (field, field_ty) in fields.iter().zip(&all[tag].1) {
                    let width = self.leaves(field_ty).len();
                    if !matches!(field.kind, PatternKind::Wildcard) {
                        let field_values = self.field_values(values[0], slot, width);
                        self.pattern_bindings(field, &field_values, field_ty, out);
                    }
                    slot += width;
                }
            }
        }
    }

    /// Makes a value of the variant `tag` of the enum type `ty` holding `args`, evaluated left to
    /// right: a new value with a count of 1, or the static value of a variant without values.
    fn variant(
        &mut self,
        ty: &Type,
        tag: usize,
        args: &[Expr],
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let all = variants(self.context.enums, ty);
        let kinds: Vec<Option<RcKind>> = all[tag]
            .1
            .iter()
            .flat_map(|field| self.leaves(field))
            .map(|leaf| RcKind::of(&leaf))
            .collect();
        if kinds.is_empty() {
            let dst = self.fresh();
            self.push(Inst::VariantStatic { dst, tag });
            return Ok(self.finish(Operand::Reg(dst), into));
        }
        let mut values = Vec::new();
        for arg in args {
            values.extend(self.expr_flat(arg)?);
        }
        // the value keeps a reference to what it holds
        for (&value, &kind) in values.iter().zip(&kinds) {
            self.take(value, kind);
        }
        let dst = self.fresh();
        self.new_variant(dst, tag, values, &kinds);
        self.own(Operand::Reg(dst), Some(RcKind::Enum));
        Ok(self.finish(Operand::Reg(dst), into))
    }

    /// Makes a new value of the variant `tag` in `dst` holding `values`, counted as `kinds`
    /// say, which already carry the references it keeps.
    ///
    /// For example, `some("a")` is `dst = call stone.enum_new(1, 1, 1)` and
    /// `field_init dst[0], v0`.
    fn new_variant(
        &mut self,
        dst: VReg,
        tag: usize,
        values: Vec<Operand>,
        kinds: &[Option<RcKind>],
    ) {
        self.push(Inst::Call {
            dsts: vec![dst],
            callee: Callee::Runtime("stone.enum_new"),
            args: vec![
                Operand::Imm(tag as i64),
                Operand::Imm(RcKind::codes(kinds)),
                Operand::Imm(kinds.len() as i64),
            ],
        });
        for (slot, src) in values.into_iter().enumerate() {
            self.push(Inst::FieldInit {
                value: Operand::Reg(dst),
                slot,
                src,
            });
        }
    }

    /// Lowers an option that is `some` of what `values` gives when `lhs cond rhs` holds, and
    /// `none` otherwise, owned either way, into `into` if given. `values` runs only when the
    /// condition holds and gives each value with how it is counted, already carrying the
    /// reference the option keeps.
    ///
    /// For example, `s.find(t)` is `some(position)` when the position is at least 0.
    fn option_if(
        &mut self,
        (cond, lhs, rhs): (Cond, Operand, Operand),
        into: Option<VReg>,
        values: impl FnOnce(&mut Self) -> Vec<(Operand, Option<RcKind>)>,
    ) -> Operand {
        let dst = self.dst(into);
        let (holds, empty, end) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond,
            float: false,
            lhs,
            rhs,
            then: holds,
            otherwise: empty,
        });
        self.start(holds);
        let (values, kinds): (Vec<Operand>, Vec<Option<RcKind>>) = values(self).into_iter().unzip();
        self.new_variant(dst, 1, values, &kinds);
        self.jump(end);
        self.start(empty);
        // the static none, retained so both ways give an owned value
        self.push(Inst::VariantStatic { dst, tag: 0 });
        self.push(Inst::Retain {
            src: Operand::Reg(dst),
        });
        self.jump(end);
        self.start(end);
        self.own(Operand::Reg(dst), Some(RcKind::Enum));
        Operand::Reg(dst)
    }

    /// Returns the tag of the variant `variant` of the enum the program calls `enum_name`.
    fn tag_of(&self, enum_name: &str, variant: &str) -> Result<usize, String> {
        self.context
            .enums
            .get(enum_name)
            .and_then(|all| all.variants.iter().position(|(name, _)| name == variant))
            .ok_or_else(|| format!("'{enum_name}' has no variant '{variant}'"))
    }

    /// Returns, for the method `method` (`unwrap`, `unwrap_or`, or `unwrap_err`) of an option
    /// or a result of type `ty`, the tag of the variant whose value it gives, the position of
    /// that value's type among the enum's type arguments, and the error it stops the program
    /// with on the other variant, with the position of the other variant's value for a result,
    /// whose failure goes on with that value.
    ///
    /// For example, `unwrap` of a `result[int, str]` gives `ok`'s value, the first argument, and
    /// stops with `cannot unwrap err: ` and the `err`'s value, the second.
    fn unwrapped(&self, ty: &Type, method: &str) -> Result<Unwrapped, String> {
        let Type::Enum { name, .. } = ty else {
            return Err("only an option or a result holds a value".to_string());
        };
        Ok(match (name.as_str(), method) {
            (RESULT, "unwrap_err") => (self.tag_of(RESULT, "err")?, 1, UNWRAP_OK, Some(0)),
            (RESULT, _) => (self.tag_of(RESULT, "ok")?, 0, UNWRAP_ERR, Some(1)),
            _ => (self.tag_of(OPTION, "some")?, 0, UNWRAP_NONE, None),
        })
    }

    /// Lowers `receiver.unwrap()`, `receiver.unwrap_err()`, or `receiver.unwrap_or(default)` with
    /// a `default`, to the values the option or result holds, owned, one for each value of a
    /// tuple: the receiver is evaluated, then the default, and `unwrap` of `none` or of an `err`,
    /// or `unwrap_err` of an `ok`, stops the program.
    fn option_value(
        &mut self,
        receiver: &Expr,
        method: &str,
        default: Option<&Expr>,
    ) -> Result<Vec<Operand>, String> {
        let ty = self.type_of(receiver)?;
        let (holding, position, failure, other) = self.unwrapped(&ty, method)?;
        let Type::Enum { args, .. } = &ty else {
            return Err("only an option or a result holds a value".to_string());
        };
        let held = args.get(position).cloned().unwrap_or(Type::Int);
        let kinds: Vec<Option<RcKind>> = self.leaves(&held).iter().map(RcKind::of).collect();
        let option = self.expr(receiver)?;
        let defaults = match default {
            Some(default) => {
                let values = self.expr_flat(default)?;
                // the default carries a reference, which a `some` gives up
                for (&value, &kind) in values.iter().zip(&kinds) {
                    self.take(value, kind);
                }
                Some(values)
            }
            None => None,
        };
        let tag = self.fresh();
        self.push(Inst::EnumTag {
            dst: tag,
            value: option,
        });
        let results: Vec<VReg> = kinds.iter().map(|_| self.fresh()).collect();
        let (holds, empty, end) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(tag),
            rhs: Operand::Imm(holding as i64),
            then: holds,
            otherwise: empty,
        });
        self.start(holds);
        let values = self.field_values(option, 0, kinds.len());
        for ((&result, value), &kind) in results.iter().zip(values).zip(&kinds) {
            self.push(Inst::Copy {
                dst: result,
                src: value,
            });
            if kind.is_some() {
                self.push(Inst::Retain {
                    src: Operand::Reg(result),
                });
            }
        }
        if let Some(defaults) = &defaults {
            for (&value, &kind) in defaults.iter().zip(&kinds) {
                if let Some(kind) = kind {
                    self.push(Inst::Release { src: value, kind });
                }
            }
        }
        self.jump(end);
        self.start(empty);
        match &defaults {
            Some(defaults) => {
                for (&result, &value) in results.iter().zip(defaults) {
                    self.push(Inst::Copy {
                        dst: result,
                        src: value,
                    });
                }
                self.jump(end);
            }
            None => {
                if let Some(other) = other {
                    // the message goes on with the other variant's value, as print writes it
                    let other_ty = args.get(other).cloned().unwrap_or(Type::Int);
                    let count = self.leaves(&other_ty).len();
                    let values = self.field_values(option, 0, count);
                    let outer = self.runtime_call("stone.text_begin", vec![], None);
                    self.print_text(&format!("{failure}: "));
                    self.print_flat(&other_ty, &values, false);
                    let message = self.runtime_call("stone.text_end", vec![outer], None);
                    self.push(Inst::Call {
                        dsts: vec![],
                        callee: Callee::Runtime("stone.fail"),
                        args: vec![message],
                    });
                }
                // the call above stops the program first
                self.terminate(Terminator::Fail(failure.to_string(), vec![]));
            }
        }
        self.start(end);
        self.release_temp(option);
        let results: Vec<Operand> = results.into_iter().map(Operand::Reg).collect();
        for (&result, &kind) in results.iter().zip(&kinds) {
            self.own(result, kind);
        }
        Ok(results)
    }

    /// Lowers `value?` to the values an `ok` or a `some` holds, owned, one for each value of a
    /// tuple, after returning an `err` or `none` from the function. That way out releases what
    /// the statement owns so far, as `ret` releases the locals: the temporaries in `owned`,
    /// which the way on still owns, and the references in `pending`.
    ///
    /// An `err` returns the very value `?` found, since a variant's layout depends only on its
    /// own values, so an `err` of a `result[int, str]` is one of a `result[float, str]` too, and
    /// the static `none` is every option's.
    fn try_values(&mut self, value: &Expr) -> Result<Vec<Operand>, String> {
        let ty = self.type_of(value)?;
        let Type::Enum { name, args } = &ty else {
            return Err("'?' needs a result or an option".to_string());
        };
        let leaving = match name.as_str() {
            RESULT => self.tag_of(RESULT, "err")?,
            _ => self.tag_of(OPTION, "none")?,
        };
        let held = args.first().cloned().unwrap_or(Type::Int);
        let kinds: Vec<Option<RcKind>> = self.leaves(&held).iter().map(RcKind::of).collect();
        let found = self.expr(value)?;
        let tag = self.fresh();
        self.push(Inst::EnumTag {
            dst: tag,
            value: found,
        });
        let (leave, holds) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(tag),
            rhs: Operand::Imm(leaving as i64),
            then: leave,
            otherwise: holds,
        });

        self.start(leave);
        let Operand::Reg(found_reg) = found else {
            return Err("an enum value is in a register".to_string());
        };
        let mut releases: Vec<(VReg, RcKind)> = self
            .owned
            .iter()
            .filter(|(reg, _)| **reg != found_reg)
            .map(|(&reg, &kind)| (reg, kind))
            .collect();
        releases.sort_by_key(|(reg, _)| reg.0);
        for (reg, kind) in releases {
            self.push(Inst::Release {
                src: Operand::Reg(reg),
                kind,
            });
        }
        for (src, kind) in self.pending.clone() {
            self.push(Inst::Release { src, kind });
        }
        // the caller gets a reference: a temporary's or a counted local's own, else a new one
        let local = self.counted_locals.iter().any(|(reg, _)| *reg == found_reg);
        if !local && !self.owned.contains_key(&found_reg) {
            self.push(Inst::Retain { src: found });
        }
        // the way on still has the statement's moves to make
        let moved = self.moved_locals.clone();
        self.leave(&[found]);
        self.moved_locals = moved;

        self.start(holds);
        let values = self.field_values(found, 0, kinds.len());
        let results: Vec<Operand> = values
            .into_iter()
            .zip(&kinds)
            .map(|(value, &kind)| {
                let result = self.fresh();
                self.push(Inst::Copy {
                    dst: result,
                    src: value,
                });
                if kind.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(result),
                    });
                }
                Operand::Reg(result)
            })
            .collect();
        self.release_temp(found);
        for (&result, &kind) in results.iter().zip(&kinds) {
            self.own(result, kind);
        }
        Ok(results)
    }

    /// Stores `value` in an assignment target: a local, a global, a list element, or a struct's
    /// field. A value counted as `kind` already carries the reference the target takes, and the
    /// target's old value is released once it is replaced.
    fn assign(
        &mut self,
        target: &Expr,
        value: Operand,
        kind: Option<RcKind>,
    ) -> Result<(), String> {
        match &target.kind {
            ExprKind::Name { id, .. } => self.assign_name(id, value, kind),
            ExprKind::Subscript { value: map, .. } if self.map_key(map).is_some() => {
                self.map_store(target, &[value])?
            }
            ExprKind::Subscript {
                value: list, slice, ..
            } => {
                // the value was evaluated first, then the indexes, like the interpreter
                let indexes = self.place_indexes(list)?;
                let index = self.expr(slice)?;
                self.store_element(target, &indexes, index, value, kind)?;
            }
            ExprKind::Attribute { .. } => self.store_place(target, &[value], &[kind])?,
            _ => return Err("cannot assign to this expression".to_string()),
        }
        Ok(())
    }

    /// Stores `value` in the variable `name`, a local or a global, or a value of a tuple variable,
    /// such as `t.0`, releasing its old value if it is counted as `kind`.
    fn assign_name(&mut self, name: &str, value: Operand, kind: Option<RcKind>) {
        if let Some(&local) = self.locals.get(name) {
            // a value moved into the call that computed the new one is not ours to release
            if let Some(kind) = kind
                && !self.moved_locals.remove(&local)
            {
                self.push(Inst::Release {
                    src: Operand::Reg(local),
                    kind,
                });
            }
            if value != Operand::Reg(local) {
                self.push(Inst::Copy {
                    dst: local,
                    src: value,
                });
            }
        } else if self.context.global_set.contains(name) {
            let kind = kind.filter(|_| !self.moved_globals.remove(name));
            self.store_global(name, value, kind);
        }
    }

    /// Lowers `targets = value` for a tuple `value`, whose values each target takes: a tuple
    /// variable each of its own, and names to unpack into one each, as in `q, r = divide(7, 2)`.
    ///
    /// Every value is computed before any target changes, so `a, b = (b, a)` swaps. A local
    /// variable among the values is copied first, since assigning one target could change it.
    fn assign_tuple(&mut self, stmt: &Stmt, targets: &[Expr], value: &Expr) -> Result<(), String> {
        let last_uses = self.last_uses(stmt);
        if self.is_stone_call(value) && targets.iter().all(is_variable_target) {
            self.movable = last_uses.clone();
            for target in targets {
                for leaf in self.flat_sources(target).into_iter().flatten() {
                    if self.is_counted_variable(&leaf) {
                        self.movable.push(leaf);
                    }
                }
            }
        }
        let kinds = self.leaf_kinds(value)?;
        let sources = self.flat_sources(value);
        let values = self.expr_flat(value)?;
        let values: Vec<Operand> = values.into_iter().map(|v| self.snapshot(v)).collect();
        // every target gets a reference of its own, the last one each temporary's
        for (i, target) in targets.iter().enumerate() {
            for (j, (&value, &kind)) in values.iter().zip(&kinds).enumerate() {
                if i + 1 < targets.len() {
                    if kind.is_some() {
                        self.push(Inst::Retain { src: value });
                    }
                } else {
                    self.take_leaf(sources[j].as_deref(), value, kind, &last_uses);
                }
            }
            self.assign_flat(target, &values, &kinds)?;
        }
        self.clear_moved();
        Ok(())
    }

    /// Stores the values of a tuple or struct, which already carry the references they need, in
    /// `target`: a variable, names to unpack them into, at any depth, a list element, or a
    /// field.
    fn assign_flat(
        &mut self,
        target: &Expr,
        values: &[Operand],
        kinds: &[Option<RcKind>],
    ) -> Result<(), String> {
        match &target.kind {
            ExprKind::Name { id, .. } => {
                for ((leaf, &value), &kind) in self.leaf_names(id).iter().zip(values).zip(kinds) {
                    self.assign_name(leaf, value, kind);
                }
            }
            ExprKind::Tuple { elts, .. } => {
                let mut start = 0;
                for elt in elts {
                    let end = start + self.leaves(&self.type_of(elt)?).len();
                    self.assign_flat(elt, &values[start..end], &kinds[start..end])?;
                    start = end;
                }
            }
            ExprKind::Subscript { value: map, .. } if self.map_key(map).is_some() => {
                self.map_store(target, values)?
            }
            ExprKind::Subscript { value: list, .. } if self.holds_flat(list) => {
                self.store_values(target, values, kinds)?
            }
            ExprKind::Subscript { .. } => self.assign(target, values[0], kinds[0])?,
            ExprKind::Attribute { .. } => self.store_place(target, values, kinds)?,
            _ => return Err("only names can be unpacked into".to_string()),
        }
        Ok(())
    }

    /// Lowers the augmented assignment `stmt`, `target op= value`.
    ///
    /// A name has no indexes to evaluate twice, so `x += v` lowers as `x = x + v`, which writes
    /// an int's vreg directly and lets `a += b` on lists write over `a`. An element evaluates its
    /// indexes once, then reads the old element, borrowed since nothing the value runs can change
    /// the list holding it, then the value, then computes the new element and stores it. For
    /// example, `xs[i] *= 3` lowers to `v2 = list_load v0, v1`, `v3 = mul v2, 3`,
    /// `v0 = list_unique v0`, and `list_store v0, v1, v3`.
    fn aug_assign(
        &mut self,
        stmt: &Stmt,
        target: &Expr,
        op: Operator,
        value: &Expr,
    ) -> Result<(), String> {
        if has_field(target) || self.through_map(target) {
            return self.aug_assign_place(target, op, value);
        }
        let ExprKind::Subscript {
            value: list, slice, ..
        } = &target.kind
        else {
            // the checker gives the operation the statement's span
            let operation = ExprKind::BinOp {
                op,
                left: Box::new(target.clone()),
                right: Box::new(value.clone()),
            };
            let assign = StmtKind::Assign {
                targets: vec![target.clone()],
                value: Box::new(Expr::new(operation, stmt.span)),
            };
            return self.stmt_unspanned(&Stmt::new(assign, stmt.span));
        };
        let indexes = self.place_indexes(list)?;
        let index = self.expr(slice)?;
        // an index out of range points at its own subscript, as when reading it
        let mut current = self.expr(place_root(list))?;
        let spans = place_spans(list).into_iter().chain([target.span]);
        for (&index, span) in indexes.iter().chain([&index]).zip(spans) {
            let outer = std::mem::replace(&mut self.span, span);
            let element = self.fresh();
            self.push(Inst::ListLoad {
                dst: element,
                list: current,
                index,
            });
            self.span = outer;
            current = Operand::Reg(element);
        }
        let old = current;
        let ty = self.type_of(target)?;
        let kind = RcKind::of(&ty);
        let new = match &ty {
            Type::List(elem) => {
                let float = **elem == Type::Float;
                let rhs = self.fused(value)?;
                let tree = Fused::Binary {
                    op: bin_op(op),
                    float,
                    lhs: Box::new(Fused::List(FusedList {
                        value: old,
                        float,
                        name: None,
                    })),
                    rhs: Box::new(rhs),
                };
                self.elementwise_tree(&tree, float, &[], kind)?
            }
            _ => {
                let rhs = self.expr(value)?;
                self.binary(op, &ty, old, rhs, None)
            }
        };
        self.take(new, kind);
        self.store_element(target, &indexes, index, new, kind)
    }

    /// Lowers `target op= value` for a target reached through a struct's field or a map's entry,
    /// such as `p.x += 1.0`, `ps[i].x *= 2.0`, or `m[k] += 1`: the indexes and keys once, then
    /// the old value, which an index out of range or a missing key fails at, then the value, then
    /// the new value is stored.
    fn aug_assign_place(
        &mut self,
        target: &Expr,
        op: Operator,
        value: &Expr,
    ) -> Result<(), String> {
        let indexes = self.place_indexes(target)?;
        let location = self.locate(target, &indexes)?;
        // borrowed, since nothing the value runs can change the variable holding it
        let old = self.location_values(&location)[0];
        let ty = self.type_of(target)?;
        let kind = RcKind::of(&ty);
        let new = match &ty {
            Type::List(elem) => {
                let float = **elem == Type::Float;
                let rhs = self.fused(value)?;
                let tree = Fused::Binary {
                    op: bin_op(op),
                    float,
                    lhs: Box::new(Fused::List(FusedList {
                        value: old,
                        float,
                        name: None,
                    })),
                    rhs: Box::new(rhs),
                };
                self.elementwise_tree(&tree, float, &[], kind)?
            }
            _ => {
                let rhs = self.expr(value)?;
                self.binary(op, &ty, old, rhs, None)
            }
        };
        self.take(new, kind);
        self.store_location(location, &[new], &[kind]);
        Ok(())
    }

    /// Stores `values`, which already carry the references they need, in `target`, a place
    /// reached through a struct's field, such as `p.x` or `ps[i].x`, evaluating its indexes first.
    fn store_place(
        &mut self,
        target: &Expr,
        values: &[Operand],
        kinds: &[Option<RcKind>],
    ) -> Result<(), String> {
        let indexes = self.place_indexes(target)?;
        let location = self.locate(target, &indexes)?;
        self.store_location(location, values, kinds);
        Ok(())
    }

    /// Walks from the variable `place_expr` starts from through its steps, with the values of its
    /// indexes from [`Lowerer::place_indexes`], to where the value it names lives, making each
    /// list on the way unique, as [`Lowerer::unique_place`] does, and checking each index.
    ///
    /// For example, `p.y` of a local `Point` is the variable `p.1`, and `ps[i].y` is slot
    /// `i * 2 + 1` of the list `ps`, made unique.
    fn locate(&mut self, place_expr: &Expr, indexes: &[Operand]) -> Result<Location, String> {
        let Some((root, steps)) = place(place_expr) else {
            return Err("only a value in a variable can be changed".to_string());
        };
        let mut ty = self
            .variable_type(root)
            .cloned()
            .ok_or_else(|| format!("'{root}' has no type"))?;
        let mut location = Location::Leaves(self.leaf_names(root));
        let mut indexes = indexes.iter().copied();
        let outer = self.span;
        for (step, span) in steps.into_iter().zip(place_spans(place_expr)) {
            match step {
                Step::Field(name) => {
                    let (range, field) = ty
                        .struct_field(name, self.context.structs)
                        .ok_or_else(|| format!("{ty} has no field '{name}'"))?;
                    location = match location {
                        Location::Leaves(names) => Location::Leaves(names[range].to_vec()),
                        Location::Slots { list, first, .. } => Location::Slots {
                            list,
                            first: self.slot(first, range.start),
                            count: range.len(),
                        },
                    };
                    ty = field;
                }
                Step::Index(_) if matches!(ty, Type::Map(..)) => {
                    let key = indexes.next().ok_or("a key was not evaluated")?;
                    let Type::Map(key_ty, value) = ty else {
                        unreachable!("the step is into a map")
                    };
                    let map = self.unique_location(location)?;
                    // a missing key points at its own subscript
                    self.span = span;
                    let first = self.map_find(map, key, &key_ty, None);
                    self.fail_missing_key(first, key, &key_ty);
                    self.span = outer;
                    // the key is slot 0 of an entry
                    location = Location::Slots {
                        list: map,
                        first: self.slot(first, 1),
                        count: self.leaves(&value).len(),
                    };
                    ty = *value;
                }
                Step::Index(_) => {
                    let index = indexes.next().ok_or("an index was not evaluated")?;
                    let Type::List(elem) = ty else {
                        return Err(format!("{ty} cannot be indexed"));
                    };
                    let list = self.unique_location(location)?;
                    let width = self.leaves(&elem).len();
                    // an index out of range points at its own subscript
                    self.span = span;
                    let first = self.element_slot(list, index, width);
                    self.span = outer;
                    location = Location::Slots {
                        list,
                        first,
                        count: width,
                    };
                    ty = *elem;
                }
            }
        }
        Ok(location)
    }

    /// Makes the list at `location`, one value, unique, writing it back where it came from, and
    /// returns it, borrowed. Only `main` changes globals.
    fn unique_location(&mut self, location: Location) -> Result<Operand, String> {
        match location {
            Location::Leaves(names) => {
                let [name] = names.as_slice() else {
                    return Err("only a list can be indexed".to_string());
                };
                if let Some(&local) = self.locals.get(name) {
                    self.push(Inst::ListUnique {
                        dst: local,
                        src: Operand::Reg(local),
                        maybe_null: false,
                    });
                    return Ok(Operand::Reg(local));
                }
                if self.name.is_some() || !self.context.global_set.contains(name) {
                    return Err(format!(
                        "a function cannot change the global '{}'",
                        variable_of(name)
                    ));
                }
                let loaded = self.fresh();
                self.push(Inst::LoadGlobal {
                    dst: loaded,
                    name: name.clone(),
                    checked: false,
                });
                let unique = self.fresh();
                self.push(Inst::ListUnique {
                    dst: unique,
                    src: Operand::Reg(loaded),
                    maybe_null: false,
                });
                self.push(Inst::StoreGlobal {
                    name: name.clone(),
                    src: Operand::Reg(unique),
                });
                Ok(Operand::Reg(unique))
            }
            Location::Slots { list, first, .. } => {
                let row = self.fresh();
                self.push(Inst::ListGet {
                    dst: row,
                    list,
                    index: first,
                    offset: 0,
                });
                self.push(Inst::ListUnique {
                    dst: row,
                    src: Operand::Reg(row),
                    maybe_null: false,
                });
                // the slot's reference moves to the unique row, which may be a copy
                self.push(Inst::ListSet {
                    list,
                    index: first,
                    value: Operand::Reg(row),
                    offset: 0,
                });
                Ok(Operand::Reg(row))
            }
        }
    }

    /// Reads the values at `location`, borrowed.
    fn location_values(&mut self, location: &Location) -> Vec<Operand> {
        match location {
            Location::Leaves(names) => names
                .iter()
                .map(|name| {
                    if let Some(&local) = self.locals.get(name) {
                        return Operand::Reg(local);
                    }
                    let dst = self.fresh();
                    self.push(Inst::LoadGlobal {
                        dst,
                        name: name.clone(),
                        checked: false,
                    });
                    Operand::Reg(dst)
                })
                .collect(),
            Location::Slots { list, first, count } => (0..*count)
                .map(|field| {
                    let index = self.slot(*first, field);
                    let dst = self.fresh();
                    self.push(Inst::ListGet {
                        dst,
                        list: *list,
                        index,
                        offset: 0,
                    });
                    Operand::Reg(dst)
                })
                .collect(),
        }
    }

    /// Stores `values`, counted as `kinds`, which already carry the references they need, at
    /// `location`, releasing the values they replace.
    fn store_location(&mut self, location: Location, values: &[Operand], kinds: &[Option<RcKind>]) {
        match location {
            Location::Leaves(names) => {
                for ((name, &value), &kind) in names.iter().zip(values).zip(kinds) {
                    self.assign_name(name, value, kind);
                }
            }
            Location::Slots { list, first, .. } => {
                let mut old = Vec::new();
                for (field, (&value, &kind)) in values.iter().zip(kinds).enumerate() {
                    let index = self.slot(first, field);
                    if let Some(kind) = kind {
                        let dst = self.fresh();
                        self.push(Inst::ListGet {
                            dst,
                            list,
                            index,
                            offset: 0,
                        });
                        old.push((dst, kind));
                    }
                    self.push(Inst::ListSet {
                        list,
                        index,
                        value,
                        offset: 0,
                    });
                }
                for (src, kind) in old {
                    self.push(Inst::Release {
                        src: Operand::Reg(src),
                        kind,
                    });
                }
            }
        }
    }

    /// Stores `value` at `index` of the list that the subscript `target` changes, whose other
    /// indexes, from [`Lowerer::place_indexes`], are `indexes`, making every list on the way
    /// unique first. A value counted as `kind` carries the reference the element takes, and the
    /// old element is released.
    fn store_element(
        &mut self,
        target: &Expr,
        indexes: &[Operand],
        index: Operand,
        value: Operand,
        kind: Option<RcKind>,
    ) -> Result<(), String> {
        let ExprKind::Subscript { value: list, .. } = &target.kind else {
            return Err("only an element of a list can be changed".to_string());
        };
        let list = self.unique_place(list, indexes)?;
        let old = kind.map(|_| self.fresh());
        let outer = std::mem::replace(&mut self.span, target.span);
        self.push(Inst::ListStore {
            old,
            list,
            index,
            value,
        });
        self.span = outer;
        if let (Some(old), Some(kind)) = (old, kind) {
            self.push(Inst::Release {
                src: Operand::Reg(old),
                kind,
            });
        }
        Ok(())
    }

    /// Evaluates the indexes that lead from a variable to `list`, which a statement is about to
    /// change, outermost first. For example, `grid[i][j]` and `ps[i].items[j]` evaluate `i` and
    /// `j`.
    fn place_indexes(&mut self, list: &Expr) -> Result<Vec<Operand>, String> {
        let Some((_, steps)) = place(list) else {
            return Err("only a value in a variable can be changed".to_string());
        };
        let mut indexes = Vec::new();
        for step in steps {
            if let Step::Index(index) = step {
                let index = self.expr(index)?;
                if matches!(index, Operand::Reg(reg) if self.owned.contains_key(&reg)) {
                    self.place_keys.push(index);
                }
                indexes.push(index);
            }
        }
        Ok(indexes)
    }

    /// Walks from the variable `list` starts from through `indexes`, from
    /// [`Lowerer::place_indexes`], and returns `list` once nothing else refers to it, borrowed
    /// from whatever holds it.
    ///
    /// Each list on the way is made unique and written back where it came from, since making it
    /// unique may copy it. For example, `grid[0].append(1)` in a function lowers to
    /// `v0 = list_unique v0`, `v1 = list_load v0, 0`, `v1 = list_unique v1`, and
    /// `list_store v0, 0, v1` before the append. Only `main` changes globals.
    fn unique_place(&mut self, list: &Expr, indexes: &[Operand]) -> Result<Operand, String> {
        if has_field(list) || self.through_map(list) {
            let location = self.locate(list, indexes)?;
            return self.unique_location(location);
        }
        let Some((root, _)) = place(list) else {
            return Err("only a list in a variable can be changed".to_string());
        };
        let mut current = if let Some(&local) = self.locals.get(root) {
            self.push(Inst::ListUnique {
                dst: local,
                src: Operand::Reg(local),
                maybe_null: false,
            });
            Operand::Reg(local)
        } else if self.name.is_none() && self.context.global_set.contains(root) {
            let loaded = self.fresh();
            self.push(Inst::LoadGlobal {
                dst: loaded,
                name: root.to_string(),
                checked: false,
            });
            let unique = self.fresh();
            self.push(Inst::ListUnique {
                dst: unique,
                src: Operand::Reg(loaded),
                maybe_null: false,
            });
            self.push(Inst::StoreGlobal {
                name: root.to_string(),
                src: Operand::Reg(unique),
            });
            Operand::Reg(unique)
        } else {
            return Err(format!("a function cannot change the global '{root}'"));
        };
        // an index out of range points at its own subscript
        let outer = self.span;
        for (&index, span) in indexes.iter().zip(place_spans(list)) {
            self.span = span;
            let row = self.fresh();
            self.push(Inst::ListLoad {
                dst: row,
                list: current,
                index,
            });
            self.push(Inst::ListUnique {
                dst: row,
                src: Operand::Reg(row),
                maybe_null: false,
            });
            // the slot's reference moves to the unique row, which may be a copy
            self.push(Inst::ListStore {
                old: None,
                list: current,
                index,
                value: Operand::Reg(row),
            });
            current = Operand::Reg(row);
        }
        self.span = outer;
        Ok(current)
    }

    /// Writes global `name`, releasing its old value if it is counted as `kind`.
    fn store_global(&mut self, name: &str, value: Operand, kind: Option<RcKind>) {
        let old = kind.map(|_| self.fresh());
        if let Some(old) = old {
            self.push(Inst::LoadGlobal {
                dst: old,
                name: name.to_string(),
                checked: false,
            });
        }
        self.push(Inst::StoreGlobal {
            name: name.to_string(),
            src: value,
        });
        if let (Some(old), Some(kind)) = (old, kind) {
            self.push(Inst::Release {
                src: Operand::Reg(old),
                kind,
            });
        }
    }

    /// Lowers `for target in iter:`, counting with hidden vregs that the body cannot assign, so
    /// assigning the loop variable never changes the iteration.
    ///
    /// A range's bounds are evaluated once, left to right. A list's length is reread every
    /// iteration, so a list that grows while it is iterated keeps going, like in the interpreter.
    /// A string steps by whole characters: `stone.str_char` returns the one at the position, and
    /// its length is the step. The loop holds a reference to the list or string until it ends, so
    /// reassigning the variable it came from cannot free it.
    fn for_loop(&mut self, target: &Expr, iter: &Expr, body: &[Stmt]) -> Result<(), String> {
        let header = self.new_block();
        let loop_body = self.new_block();
        let exit = self.new_block();

        let next = self.fresh();
        let iterable = match range_args(iter) {
            Some([limit]) => {
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                let end = self.expr(limit)?;
                Iterable::Range(self.snapshot(end))
            }
            Some([start, limit]) => {
                self.expr_into(start, Some(next))?;
                let end = self.expr(limit)?;
                Iterable::Range(self.snapshot(end))
            }
            Some(_) => return Err("range() takes 1 or 2 arguments".to_string()),
            None if matches!(self.type_of(iter)?, Type::Str | Type::Bytes) => {
                let bytes = self.type_of(iter)? == Type::Bytes;
                let text = self.expr(iter)?;
                let text = self.snapshot(text);
                self.take(text, Some(RcKind::Str));
                let len = self.runtime_call("stone.str_len", vec![text], None);
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                Iterable::Str(text, len, bytes)
            }
            None => {
                let list = self.expr(iter)?;
                let list = self.snapshot(list);
                if self.map_key(iter).is_some() {
                    self.close_up(list);
                }
                self.take(list, Some(RcKind::List));
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                Iterable::List(list)
            }
        };
        let (held, elem) = match iterable {
            Iterable::List(list) => (Some((list, RcKind::List)), self.element_kind(iter)),
            Iterable::Str(text, _, false) => (Some((text, RcKind::Str)), Some(RcKind::Str)),
            // a byte is an int
            Iterable::Str(text, _, true) => (Some((text, RcKind::Str)), None),
            Iterable::Range(_) => (None, None),
        };

        // each element of a list of tuples or structs is several values, and a map's entries
        // are its key and then its value's, of which the loop takes the key
        let width = self.element_layout(iter).0;
        let keyed = self.map_key(iter).is_some();
        let flat = self.holds_flat(iter) || keyed;
        self.loop_depth += 1;
        self.start(header);
        let end = match iterable {
            Iterable::List(list) if flat => self.element_count(list, width),
            Iterable::List(list) => {
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                Operand::Reg(len)
            }
            Iterable::Str(_, len, _) => len,
            Iterable::Range(end) => end,
        };
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(next),
            rhs: end,
            then: loop_body,
            otherwise: exit,
        });

        self.start(loop_body);
        if let Iterable::List(list) = iterable
            && flat
        {
            // each value is retained for the variable that takes it
            let kinds = self.element_kinds(iter);
            let leaves = if keyed { 0..1 } else { 0..width };
            let values = self.load_slots(list, Operand::Reg(next), width, leaves.clone(), &kinds);
            self.assign_flat(target, &values, &kinds[leaves])?;
            self.push(Inst::Binary {
                op: BinOp::Add,
                dst: next,
                lhs: Operand::Reg(next),
                rhs: Operand::Imm(1),
            });
            return self.loop_body(exit, header, held, body);
        }
        let local = match &target.kind {
            ExprKind::Name { id, .. } => self.locals.get(id).copied(),
            _ => return Err("a 'for' loop's variable must be a name".to_string()),
        };
        let value = match iterable {
            Iterable::List(list) => {
                // a counted element is retained for the variable, which releases its old value
                let dst = match elem {
                    Some(_) => self.fresh(),
                    None => self.dst(local),
                };
                self.push(Inst::ListGet {
                    dst,
                    list,
                    index: Operand::Reg(next),
                    offset: 0,
                });
                if elem.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                }
                Operand::Reg(dst)
            }
            Iterable::Str(..) => Operand::Reg(self.fresh()),
            Iterable::Range(_) => Operand::Reg(next),
        };
        let step = match iterable {
            Iterable::Str(text, _, true) => {
                let Operand::Reg(dst) = value else {
                    unreachable!("a byte is a register")
                };
                self.push(Inst::Call {
                    dsts: vec![dst],
                    callee: Callee::Runtime("stone.bytes_at"),
                    args: vec![text, Operand::Reg(next)],
                });
                Operand::Imm(1)
            }
            Iterable::Str(text, _, false) => {
                // the character is a new reference, which the variable takes
                let Operand::Reg(dst) = value else {
                    unreachable!("a character is a register")
                };
                self.push(Inst::Call {
                    dsts: vec![dst],
                    callee: Callee::Runtime("stone.str_char"),
                    args: vec![text, Operand::Reg(next)],
                });
                self.runtime_call("stone.str_len", vec![value], None)
            }
            _ => Operand::Imm(1),
        };
        self.assign(target, value, elem)?;
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: step,
        });
        self.loop_body(exit, header, held, body)
    }

    /// Lowers the body of a `for` loop whose variable is assigned, then the loop's end, which
    /// releases the list or string it holds.
    fn loop_body(
        &mut self,
        exit: BlockId,
        header: BlockId,
        held: Option<(Operand, RcKind)>,
        body: &[Stmt],
    ) -> Result<(), String> {
        self.loops.push(Loop { exit, header, held });
        for stmt in body {
            self.stmt(stmt)?;
        }
        self.loops.pop();
        if self.current.is_some() {
            self.jump(header);
        }
        self.loop_depth -= 1;
        self.start(exit);
        if let Some((src, kind)) = held {
            self.push(Inst::Release { src, kind });
        }
        Ok(())
    }

    /// Lowers a test, going to `then` if it is true and to `otherwise` if not, and ends the
    /// current block.
    ///
    /// Comparisons branch on the flags directly, and `and`, `or`, and `not` become branches, so
    /// `if a < b and c:` never materializes a bool.
    fn cond(&mut self, test: &Expr, then: BlockId, otherwise: BlockId) -> Result<(), String> {
        match &test.kind {
            // a single == or != of tuples, structs, or enum values branches as it compares, then
            // releases the temporaries it compared on either way out
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } if ops.len() == 1
                && matches!(
                    ops[0],
                    crate::ast::CompOp::Equal | crate::ast::CompOp::NotEqual
                )
                && self
                    .type_of(&comparators[0])
                    .is_ok_and(|ty| is_compound(&ty)) =>
            {
                let ty = self.type_of(&comparators[0])?;
                let lhs = self.expr_flat(left)?;
                let rhs = self.expr_flat(&comparators[0])?;
                let (then, otherwise) = match ops[0] {
                    crate::ast::CompOp::NotEqual => (otherwise, then),
                    _ => (then, otherwise),
                };
                let values: Vec<Operand> = lhs.iter().chain(&rhs).copied().collect();
                let owned = self.owned_values(&values);
                if owned.is_empty() {
                    self.equal_branch(&ty, &lhs, &rhs, then, otherwise);
                    return Ok(());
                }
                for (value, _) in &owned {
                    if let Operand::Reg(reg) = value {
                        self.owned.remove(reg);
                    }
                }
                let (yes, no) = (self.new_block(), self.new_block());
                self.equal_branch(&ty, &lhs, &rhs, yes, no);
                for (block, target) in [(yes, then), (no, otherwise)] {
                    self.start(block);
                    for &(src, kind) in &owned {
                        self.push(Inst::Release { src, kind });
                    }
                    self.jump(target);
                }
            }

            // strings are compared by a call, which compare handles with their references, and
            // tuples, structs, and enum values by several
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } if !ops.iter().any(crate::ast::CompOp::is_membership)
                && comparators.iter().all(|c| {
                    self.type_of(c)
                        .is_ok_and(|ty| !matches!(ty, Type::Str | Type::Bytes) && !is_compound(&ty))
                }) =>
            {
                // a < b < c means a < b and b < c, with b evaluated once and c skipped once false
                let mut lhs = self.expr(left)?;
                for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
                    let rhs = self.expr(comparator)?;
                    let float = self.type_of(comparator)? == Type::Float;
                    let next = if i + 1 < ops.len() {
                        self.new_block()
                    } else {
                        then
                    };
                    let cond = Cond::of(op).ok_or("'in' is not a comparison of two values")?;
                    self.terminate(Terminator::CmpBranch {
                        cond,
                        float,
                        lhs,
                        rhs,
                        then: next,
                        otherwise,
                    });
                    if next != then {
                        self.start(next);
                    }
                    lhs = rhs;
                }
            }

            ExprKind::BoolOp { op, values } if !values.is_empty() => {
                let (last, rest) = values.split_last().expect("values is not empty");
                for value in rest {
                    let next = self.new_block();
                    match op {
                        BoolOp::And => self.cond(value, next, otherwise)?,
                        BoolOp::Or => self.cond(value, then, next)?,
                    }
                    self.start(next);
                }
                self.cond(last, then, otherwise)?;
            }

            ExprKind::UnaryOp {
                op: UnaryOp::Not,
                operand,
            } => self.cond(operand, otherwise, then)?,

            ExprKind::Constant { value, .. } if let Constant::Bool(b) = **value => {
                self.jump(if b { then } else { otherwise });
            }

            _ => {
                let cond = self.expr(test)?;
                self.terminate(Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                });
            }
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<Operand, String> {
        self.expr_into(expr, None)
    }

    /// Lowers `expr` and returns where its value is. With `into`, the value ends up in that vreg,
    /// and instructions that read all their inputs before writing write it directly.
    ///
    /// A counted value comes back borrowed, or as a temporary in [`Lowerer::owned`] that the
    /// caller must release or give away.
    fn expr_into(&mut self, expr: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        let outer = std::mem::replace(&mut self.span, expr.span);
        let result = self.expr_into_unspanned(expr, into);
        self.span = outer;
        result
    }

    fn expr_into_unspanned(&mut self, expr: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        if self.is_flat(expr) {
            return Err(
                "a tuple or struct has several values, which `expr_flat` lowers".to_string(),
            );
        }
        let kind = self.counted(expr);
        if into.is_some() && kind.is_some() {
            // computed apart, so the destination's old value is still there to release
            let value = self.expr(expr)?;
            return Ok(self.finish(value, into));
        }
        match &expr.kind {
            ExprKind::Lambda { .. } => {
                Err("a lambda must be linked before it is lowered".to_string())
            }
            ExprKind::Try { value } => {
                let values = self.try_values(value)?;
                Ok(self.finish(values[0], into))
            }
            ExprKind::JoinedStr { values } => self.joined(values, into, kind),
            ExprKind::Dict { keys, values } => {
                let (width, codes) = self.element_layout(expr);
                let kinds = self.element_kinds(expr);
                let key_ty = self.map_key(expr).ok_or("a map literal is a map")?;
                let map = self.fresh();
                self.push(Inst::Call {
                    dsts: vec![map],
                    callee: Callee::Runtime("stone.list_new_wide"),
                    args: vec![
                        Operand::Imm(0),
                        Operand::Imm(codes),
                        Operand::Imm(width as i64),
                    ],
                });
                // owned from the start, so a `?` in a later entry frees it and what it holds
                self.own(Operand::Reg(map), kind);
                // each key, then its value, left to right, and a later key wins
                for (key_expr, value) in keys.iter().zip(values) {
                    let key = self.expr(key_expr)?;
                    let values = self.expr_flat(value)?;
                    for (&value, &kind) in values.iter().zip(&kinds[1..]) {
                        self.take(value, kind);
                    }
                    self.map_put(Operand::Reg(map), key, &key_ty, &values, &kinds);
                    self.release_temp(key);
                }
                Ok(Operand::Reg(map))
            }
            ExprKind::Constant { value, .. } => {
                let value = match &**value {
                    Constant::Int(n) => Operand::Imm(*n),
                    Constant::Float(x) => Operand::Imm(x.to_bits() as i64),
                    Constant::Bool(b) => Operand::Imm(*b as i64),
                    Constant::Str(s) => {
                        // literals are never freed, so they are borrowed
                        let dst = self.fresh();
                        self.push(Inst::StrAddr {
                            dst,
                            text: s.clone(),
                        });
                        Operand::Reg(dst)
                    }
                    Constant::Bytes(bytes) => {
                        let dst = self.fresh();
                        self.push(Inst::BytesAddr {
                            dst,
                            bytes: bytes.clone(),
                        });
                        Operand::Reg(dst)
                    }
                    other => return Err(format!("unsupported constant value {:?}", other)),
                };
                Ok(self.finish(value, into))
            }

            ExprKind::Name { id, .. } => {
                if let Some(function) = self.context.function_values.get(&expr.span) {
                    let copy = self.callee_label(function, expr.span);
                    return Ok(self.function_value(function, &copy, expr.span, into));
                }
                // a parameter this copy binds holds its function
                if let Some(bound) = self.constants.get(id).cloned() {
                    return Ok(self.function_value(&bound.base, &bound.label, expr.span, into));
                }
                if let Some(&local) = self.locals.get(id) {
                    return Ok(self.finish(Operand::Reg(local), into));
                }
                if let Some(&literal) = self.context.constant_globals.get(id) {
                    return Ok(self.finish(Operand::Imm(literal), into));
                }
                if self.context.global_set.contains(id) {
                    let dst = self.dst(into);
                    // top-level code is checked to assign before reading, but a function can run
                    // before a global it reads is assigned
                    self.push(Inst::LoadGlobal {
                        dst,
                        name: id.clone(),
                        checked: self.name.is_some(),
                    });
                    return Ok(Operand::Reg(dst));
                }
                Ok(self.finish(Operand::Imm(0), into))
            }

            ExprKind::BinOp { .. } | ExprKind::UnaryOp { .. } if self.is_elementwise(expr) => {
                self.elementwise(expr, kind)
            }

            ExprKind::BinOp { op, left, right } => {
                let ty = self.type_of(expr)?;
                let lhs = self.expr(left)?;
                let rhs = self.expr(right)?;
                Ok(self.binary(*op, &ty, lhs, rhs, into))
            }

            ExprKind::UnaryOp { op, operand } => {
                if *op == UnaryOp::UnaryAdd {
                    let value = self.expr(operand)?;
                    return Ok(self.finish(value, into));
                }
                let float = self.type_of(operand)? == Type::Float;
                if *op == UnaryOp::UnarySub {
                    if let Some(n) = constant_int(expr) {
                        return Ok(self.finish(Operand::Imm(n), into));
                    }
                    if let ExprKind::Constant { value, .. } = &operand.kind
                        && let Constant::Float(x) = **value
                    {
                        // the same bits flipping the sign bit gives
                        return Ok(self.finish(Operand::Imm((-x).to_bits() as i64), into));
                    }
                }
                let src = self.expr(operand)?;
                let dst = self.dst(into);
                self.push(match op {
                    UnaryOp::Not => Inst::Not { dst, src },
                    _ if float => Inst::FloatNeg { dst, src },
                    _ => Inst::Neg { dst, src },
                });
                Ok(Operand::Reg(dst))
            }

            ExprKind::BoolOp { values, .. } if values.is_empty() => {
                Ok(self.finish(Operand::Imm(0), into))
            }

            ExprKind::BoolOp { op, values } => {
                // a fresh vreg, since a later value may read the variable being assigned
                let result = self.fresh();
                let end = self.new_block();
                for (i, value) in values.iter().enumerate() {
                    self.expr_into(value, Some(result))?;
                    if i + 1 < values.len() {
                        let next = self.new_block();
                        let (then, otherwise) = match op {
                            BoolOp::And => (next, end),
                            BoolOp::Or => (end, next),
                        };
                        self.terminate(Terminator::Branch {
                            cond: Operand::Reg(result),
                            then,
                            otherwise,
                        });
                        self.start(next);
                    }
                }
                self.start(end);
                Ok(self.finish(Operand::Reg(result), into))
            }

            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => self.compare(left, ops, comparators, into),

            ExprKind::Call { func, args, .. } => {
                if let Some(conversion) = self.context.conversions.get(&expr.span) {
                    return Ok(self.conversion(expr, conversion, &args[0], into, kind)?[0]);
                }
                if self.context.indirect_calls.contains(&expr.span) {
                    let kinds = self.leaf_kinds(expr)?;
                    return Ok(self.value_call(func, expr.span, args, into, &kinds)?[0]);
                }
                if let ExprKind::Attribute { value, .. } = &func.kind
                    && let Some(method) = self.context.methods.get(&expr.span)
                {
                    return Ok(self.struct_call(expr, method, value, args, into)?[0]);
                }
                if let ExprKind::Attribute { value, attr, .. } = &func.kind {
                    if let ExprKind::Name { id, .. } = &value.kind
                        && self.context.enums.contains_key(id)
                        && may_name_enum(value)
                    {
                        let ty = self.type_of(expr)?;
                        let tag = self.tag_of(id, attr)?;
                        return self.variant(&ty, tag, args, into);
                    }
                    // the prelude's function moves the list in and gives it back sorted, as a
                    // method changing `self` does
                    if attr == "sort_by_key" {
                        self.changing_call(
                            ("prelude.sort_by_key", expr.span),
                            value,
                            args,
                            None,
                            &[],
                        )?;
                        return Ok(self.finish(Operand::Imm(0), into));
                    }
                    return self.method(value, attr, args, into, kind);
                }
                let ExprKind::Name { id, .. } = &func.kind else {
                    return Err("only a function or a function value can be called".to_string());
                };
                self.call(id, expr.span, args, into, kind)
            }

            ExprKind::Attribute { value, attr, .. } => match &value.kind {
                ExprKind::Name { id, .. }
                    if self.context.enums.contains_key(id) && may_name_enum(value) =>
                {
                    let ty = self.type_of(expr)?;
                    let tag = self.tag_of(id, attr)?;
                    self.variant(&ty, tag, &[], into)
                }
                // one value, since the field is not a tuple or struct itself
                _ => {
                    let field = self.struct_field(value, attr)?;
                    Ok(self.finish(field[0], into))
                }
            },

            ExprKind::Subscript { value, slice, .. } if self.is_flat(value) => {
                // one value, since the field is not a tuple itself
                let field = self.tuple_field(value, slice)?;
                Ok(self.finish(field[0], into))
            }

            ExprKind::Subscript { value, slice, .. } if self.map_key(value).is_some() => {
                let values = self.map_values(expr, 0..1)?;
                Ok(self.finish(values[0], into))
            }

            ExprKind::Subscript { value, slice, .. } => {
                let indexed = self.type_of(value)?;
                let string = indexed == Type::Str;
                if let ExprKind::Slice { lower, upper, step } = &slice.kind {
                    let label = match indexed {
                        Type::Str => "stone.str_slice_of",
                        Type::Bytes => "stone.bytes_slice_of",
                        _ => "stone.list_slice",
                    };
                    return self.slice(value, [lower, upper, step], label, into, kind);
                }
                if indexed == Type::Bytes {
                    return self.bytes_index(value, slice, into);
                }
                if string {
                    return self.string_index(value, slice, into, kind);
                }
                let list = self.list_operand(value, &[slice])?;
                let index = self.expr(slice)?;
                let dst = self.dst(into);
                self.push(Inst::ListLoad { dst, list, index });
                // a later call could replace the element in the list, so it gets a reference
                if kind.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                    self.own(Operand::Reg(dst), kind);
                }
                self.release_temp(list);
                Ok(Operand::Reg(dst))
            }

            ExprKind::List { elts, .. } => {
                let (width, kinds) = self.element_layout(expr);
                let leaf_kinds = self.element_kinds(expr);
                let mut values = Vec::new();
                for elt in elts {
                    values.extend(self.expr_flat(elt)?);
                }
                for (i, value) in values.iter().enumerate() {
                    self.take(*value, leaf_kinds[i % width]);
                }
                let list = self.fresh();
                // a list of tuples records how wide its elements are
                let (label, mut args) = match width {
                    1 => ("stone.list_new", vec![]),
                    _ => ("stone.list_new_wide", vec![Operand::Imm(width as i64)]),
                };
                args.insert(0, Operand::Imm(kinds));
                args.insert(0, Operand::Imm(values.len() as i64));
                self.push(Inst::Call {
                    dsts: vec![list],
                    callee: Callee::Runtime(label),
                    args,
                });
                for (index, value) in values.into_iter().enumerate() {
                    self.push(Inst::ListInit {
                        list: Operand::Reg(list),
                        index,
                        value,
                    });
                }
                self.own(Operand::Reg(list), kind);
                Ok(Operand::Reg(list))
            }

            ExprKind::Tuple { .. } => Err("a tuple has several values".to_string()),
            ExprKind::Slice { .. } => Err("a slice can only appear in brackets".to_string()),
        }
    }

    /// Lowers arithmetic on lists to one loop over their elements, with no list for any
    /// intermediate result and no index checks, as the interpreter's `elementwise` defines it:
    /// every operand that is not itself arithmetic on lists is evaluated once, left to right, then
    /// the lists' lengths are checked, then the whole expression is computed for each element in
    /// turn.
    ///
    /// The result goes in a new list, unless an operand is a variable in [`Lowerer::movable`] with
    /// the result's element type, which moves into `stone.list_reuse`: when nothing else refers
    /// to its list, the loop writes the result over it, so `a = a * 2 + b` allocates nothing.
    fn elementwise(&mut self, expr: &Expr, kind: Option<RcKind>) -> Result<Operand, String> {
        // taken first, so that no call among the operands moves what the loop reads
        let movable = std::mem::take(&mut self.movable);
        let tree = self.fused(expr)?;
        let float = matches!(self.type_of(expr)?, Type::List(elem) if *elem == Type::Float);
        self.elementwise_tree(&tree, float, &movable, kind)
    }

    /// Computes `tree`, from [`Lowerer::fused`], for each element, as [`Lowerer::elementwise`]
    /// describes, into a list of floats if `float` and of ints otherwise. The result may reuse
    /// the list of a variable in `movable`.
    fn elementwise_tree(
        &mut self,
        tree: &Fused,
        float: bool,
        movable: &[String],
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let mut lists = Vec::new();
        tree.lists(&mut lists);
        // a list used twice, as in `a * a`, is checked and read once
        let mut seen = HashSet::new();
        lists.retain(|leaf| seen.insert(leaf.value));
        let first = lists
            .first()
            .ok_or("arithmetic on lists has no list")?
            .value;
        let len = self.fresh();
        self.push(Inst::ListLen {
            dst: len,
            list: first,
        });
        for leaf in &lists[1..] {
            let other = self.fresh();
            self.push(Inst::ListLen {
                dst: other,
                list: leaf.value,
            });
            self.fail_unless_with(
                Cond::Eq,
                Operand::Reg(len),
                Operand::Reg(other),
                LENGTHS_DIFFER,
                vec![Operand::Reg(len), Operand::Reg(other)],
            );
        }
        let reused = lists.iter().find(|leaf| {
            leaf.float == float
                && leaf
                    .name
                    .as_ref()
                    .is_some_and(|name| movable.contains(name))
        });
        let result = self.fresh();
        let call = match reused {
            Some(leaf) if self.mark_moved(leaf.name.as_deref().unwrap_or_default()) => Inst::Call {
                dsts: vec![result],
                callee: Callee::Runtime("stone.list_reuse"),
                args: vec![leaf.value, Operand::Reg(len)],
            },
            _ => Inst::Call {
                dsts: vec![result],
                callee: Callee::Runtime("stone.list_new"),
                args: vec![Operand::Reg(len), Operand::Imm(0)],
            },
        };
        self.push(call);
        let temporaries: Vec<Operand> = lists.iter().map(|leaf| leaf.value).collect();

        // pairs of elements first, when the operations allow, then one at a time from where that
        // stopped
        let mut start = Operand::Imm(0);
        if tree.packable(float) && tree.size() <= MAX_PACKED_SIZE {
            let mut data = HashMap::new();
            for leaf in &lists {
                let dst = self.fresh();
                self.push(Inst::ListData {
                    dst,
                    list: leaf.value,
                });
                data.insert(leaf.value, Operand::Reg(dst));
            }
            let out = self.fresh();
            self.push(Inst::ListData {
                dst: out,
                list: Operand::Reg(result),
            });
            let done = self.fresh();
            self.push(Inst::Packed {
                done,
                float,
                kernel: tree.kernel(&data),
                out: Operand::Reg(out),
                len: Operand::Reg(len),
            });
            start = Operand::Reg(done);
        }
        let index = self.fresh();
        self.push(Inst::Copy {
            dst: index,
            src: start,
        });
        let (header, body, exit) = (self.new_block(), self.new_block(), self.new_block());
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(index),
            rhs: Operand::Reg(len),
            then: body,
            otherwise: exit,
        });
        self.start(body);
        let value = self.element(tree, Operand::Reg(index), &mut HashMap::new());
        self.push(Inst::ListSet {
            list: Operand::Reg(result),
            index: Operand::Reg(index),
            value,
            offset: 0,
        });
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: index,
            lhs: Operand::Reg(index),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        for value in temporaries {
            self.release_temp(value);
        }
        self.own(Operand::Reg(result), kind);
        Ok(Operand::Reg(result))
    }

    /// Computes `lhs op rhs` on two numbers or, for `+`, two strings, both of type `ty`, into
    /// `into` if given, and releases either operand that is a temporary. A string result is a
    /// temporary.
    ///
    /// For example, adding two ints is `v2 = add v0, v1`, and two strings call
    /// `stone.str_concat`.
    fn binary(
        &mut self,
        op: Operator,
        ty: &Type,
        lhs: Operand,
        rhs: Operand,
        into: Option<VReg>,
    ) -> Operand {
        let dst = self.dst(into);
        let op = bin_op(op);
        self.push(match ty {
            // only `+` applies to strings and bytes
            Type::Str | Type::Bytes => Inst::Call {
                dsts: vec![dst],
                callee: Callee::Runtime("stone.str_concat"),
                args: vec![lhs, rhs],
            },
            Type::Float => Inst::FloatBinary { op, dst, lhs, rhs },
            _ => Inst::Binary { op, dst, lhs, rhs },
        });
        self.release_temp(lhs);
        self.release_temp(rhs);
        self.own(Operand::Reg(dst), RcKind::of(ty));
        Operand::Reg(dst)
    }

    /// Evaluates the operands of the arithmetic on lists `expr`, left to right, into a tree that
    /// [`Lowerer::element`] computes one element of. An operand that is a number, including
    /// arithmetic on numbers such as `2 + 3` in `a * (2 + 3)`, is computed here, once.
    fn fused(&mut self, expr: &Expr) -> Result<Fused, String> {
        let ty = self.type_of(expr)?;
        if let Type::List(elem) = &ty {
            let float = **elem == Type::Float;
            match &expr.kind {
                ExprKind::BinOp { op, left, right } => {
                    let lhs = self.fused(left)?;
                    let rhs = self.fused(right)?;
                    return Ok(Fused::Binary {
                        op: bin_op(*op),
                        float,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                }
                ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd,
                    operand,
                } => return self.fused(operand),
                ExprKind::UnaryOp {
                    op: UnaryOp::UnarySub,
                    operand,
                } => {
                    let operand = self.fused(operand)?;
                    return Ok(Fused::Neg {
                        float,
                        operand: Box::new(operand),
                    });
                }
                _ => {}
            }
        }
        let value = self.expr(expr)?;
        Ok(match ty {
            Type::List(elem) => Fused::List(FusedList {
                value,
                float: *elem == Type::Float,
                name: match &expr.kind {
                    ExprKind::Name { id, .. } => Some(id.clone()),
                    _ => None,
                },
            }),
            _ => Fused::Number(value),
        })
    }

    /// Computes the element at `index` of `tree`, from [`Lowerer::fused`], whose lists all have
    /// more than `index` elements, with the same instructions as arithmetic on single numbers.
    /// `loaded` holds the element already read from each list.
    fn element(
        &mut self,
        tree: &Fused,
        index: Operand,
        loaded: &mut HashMap<Operand, Operand>,
    ) -> Operand {
        match tree {
            Fused::Number(value) => *value,
            Fused::List(leaf) => {
                if let Some(&element) = loaded.get(&leaf.value) {
                    return element;
                }
                let dst = self.fresh();
                self.push(Inst::ListGet {
                    dst,
                    list: leaf.value,
                    index,
                    offset: 0,
                });
                loaded.insert(leaf.value, Operand::Reg(dst));
                Operand::Reg(dst)
            }
            Fused::Binary {
                op,
                float,
                lhs,
                rhs,
            } => {
                let lhs = self.element(lhs, index, loaded);
                let rhs = self.element(rhs, index, loaded);
                let dst = self.fresh();
                self.push(match float {
                    true => Inst::FloatBinary {
                        op: *op,
                        dst,
                        lhs,
                        rhs,
                    },
                    false => Inst::Binary {
                        op: *op,
                        dst,
                        lhs,
                        rhs,
                    },
                });
                Operand::Reg(dst)
            }
            Fused::Neg { float, operand } => {
                let src = self.element(operand, index, loaded);
                let dst = self.fresh();
                self.push(match float {
                    true => Inst::FloatNeg { dst, src },
                    false => Inst::Neg { dst, src },
                });
                Operand::Reg(dst)
            }
        }
    }

    /// Stops the program with the runtime error `message` unless `lhs cond rhs` holds for ints.
    fn fail_unless(&mut self, cond: Cond, lhs: Operand, rhs: Operand, message: &str) {
        self.fail_unless_with(cond, lhs, rhs, message, Vec::new());
    }

    /// Stops the program with the runtime error `template` filled with `values` unless
    /// `lhs cond rhs` holds for ints.
    ///
    /// For example, checking that two lengths are equal fails with
    /// `fail "lists have different lengths ({} and {})", v2, v3`.
    fn fail_unless_with(
        &mut self,
        cond: Cond,
        lhs: Operand,
        rhs: Operand,
        template: &str,
        values: Vec<Operand>,
    ) {
        let (holds, fails) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond,
            float: false,
            lhs,
            rhs,
            then: holds,
            otherwise: fails,
        });
        self.start(fails);
        self.terminate(Terminator::Fail(template.to_string(), values));
        self.start(holds);
    }

    /// Returns where `index` points in a list of `len` elements, counting a negative index from
    /// the end, and stops the program with `message` unless that is an element, or with `end`,
    /// an element or the end of the list. A message with holes, such as [`INDEX_RANGE`], is
    /// filled with the index as written and the length.
    ///
    /// For example, `xs.insert(-1, x)` puts `x` at position `len - 1`, and `xs.insert(len, x)` at
    /// the end, but `xs.pop(len)` fails.
    fn list_position(&mut self, index: Operand, len: Operand, end: bool, message: &str) -> Operand {
        let position = self.fresh();
        self.push(Inst::Copy {
            dst: position,
            src: index,
        });
        let (negative, check) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(position),
            rhs: Operand::Imm(0),
            then: negative,
            otherwise: check,
        });
        self.start(negative);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: position,
            lhs: Operand::Reg(position),
            rhs: len,
        });
        self.start(check);
        let values = match message.contains("{}") {
            true => vec![index, len],
            false => Vec::new(),
        };
        let start = Operand::Imm(0);
        self.fail_unless_with(
            Cond::Ge,
            Operand::Reg(position),
            start,
            message,
            values.clone(),
        );
        let below = if end { Cond::Le } else { Cond::Lt };
        self.fail_unless_with(below, Operand::Reg(position), len, message, values);
        Operand::Reg(position)
    }

    /// Lowers the bytes index `bytes[index]`, as [`Lowerer::string_index`] lowers a string's,
    /// into a check that the index is in range and a call of `stone.bytes_at`, which gives the
    /// byte as an int.
    fn bytes_index(
        &mut self,
        bytes: &Expr,
        index: &Expr,
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let bytes = self.list_operand(bytes, &[index])?;
        let index = self.expr(index)?;
        let len = self.runtime_call("stone.str_len", vec![bytes], None);
        let position = self.list_position(index, len, false, BYTES_INDEX_RANGE);
        let result = self.runtime_call("stone.bytes_at", vec![bytes, position], into);
        self.release_temp(bytes);
        Ok(result)
    }

    /// Lowers the string index `text[index]`, evaluating the string and then the index, into a
    /// check that the index is in range followed by a call to `stone.str_at`, which fails if the
    /// byte there is part of a longer character.
    ///
    /// For example, `s[i]` becomes `v2 = call stone.str_len(v0)`, the steps of
    /// [`Lowerer::list_position`], and `v4 = call stone.str_at(v0, v3)`.
    fn string_index(
        &mut self,
        text: &Expr,
        index: &Expr,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let text = self.list_operand(text, &[index])?;
        let index = self.expr(index)?;
        let len = self.runtime_call("stone.str_len", vec![text], None);
        let position = self.list_position(index, len, false, STRING_INDEX_RANGE);
        let result = self.runtime_call("stone.str_at", vec![text, position], into);
        self.release_temp(text);
        self.own(result, kind);
        Ok(result)
    }

    /// Lowers the slice `list[lower:upper:step]` of a list or a string, evaluating it and then
    /// each bound that is there, then failing on a step of 0, into a call to `label`:
    /// `stone.list_slice` or `stone.str_slice_of`. A missing bound is passed as 0 with its bit set
    /// in the last argument: 1 for `lower`, 2 for `upper`, and 4 for `step`.
    ///
    /// For example, `xs[1:]` becomes `v1 = call stone.list_slice(v0, 1, 0, 0, 6)`.
    fn slice(
        &mut self,
        list: &Expr,
        bounds: [&Option<Box<Expr>>; 3],
        label: &'static str,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let present: Vec<&Expr> = bounds.iter().filter_map(|b| b.as_deref()).collect();
        let list = self.list_operand(list, &present)?;
        let mut args = vec![list];
        let mut omitted = 0;
        for (bit, bound) in bounds.into_iter().enumerate() {
            match bound {
                Some(bound) => args.push(self.expr(bound)?),
                None => {
                    args.push(Operand::Imm(0));
                    omitted |= 1 << bit;
                }
            }
        }
        if let Some(step) = bounds[2]
            && constant_int(step).is_none_or(|n| n == 0)
        {
            let (zero, nonzero) = (self.new_block(), self.new_block());
            self.terminate(Terminator::CmpBranch {
                cond: Cond::Eq,
                float: false,
                lhs: args[3],
                rhs: Operand::Imm(0),
                then: zero,
                otherwise: nonzero,
            });
            self.start(zero);
            self.terminate(Terminator::Fail(SLICE_STEP_ZERO.to_string(), Vec::new()));
            self.start(nonzero);
        }
        args.push(Operand::Imm(omitted));
        let result = self.runtime_call(label, args, into);
        self.release_temp(list);
        self.own(result, kind);
        Ok(result)
    }

    /// Lowers `list`, the list an operation reads once `before` are evaluated.
    ///
    /// An element of another list is borrowed rather than retained when `before` makes no calls,
    /// since nothing else can change a list until the operation reads it, so `a[i][k]` adds no
    /// reference to the row `a[i]`. The list it came from must be borrowed too, since releasing
    /// a temporary like the result of `f()` in `f()[0][1]` could free the row with it.
    fn list_operand(&mut self, list: &Expr, before: &[&Expr]) -> Result<Operand, String> {
        let ExprKind::Subscript { value, slice, .. } = &list.kind else {
            return self.expr(list);
        };
        // a slice is a new list, which the operation must release, and a tuple's field is one of
        // its values
        if matches!(slice.kind, ExprKind::Slice { .. })
            || before.iter().any(|expr| has_call(expr))
            || self.is_flat(value)
            || self.map_key(value).is_some()
        {
            return self.expr(list);
        }
        let outer = self.list_operand(value, &[slice])?;
        let index = self.expr(slice)?;
        let dst = self.fresh();
        self.push(Inst::ListLoad {
            dst,
            list: outer,
            index,
        });
        if matches!(outer, Operand::Reg(reg) if self.owned.contains_key(&reg)) {
            self.push(Inst::Retain {
                src: Operand::Reg(dst),
            });
            self.own(Operand::Reg(dst), Some(RcKind::List));
            self.release_temp(outer);
        }
        Ok(Operand::Reg(dst))
    }

    /// Lowers `container`, a map, list, or string of type `ty` that a test of membership reads
    /// once `before` are evaluated, borrowing a list's element as [`Lowerer::list_operand`] does.
    fn container(
        &mut self,
        container: &Expr,
        ty: &Type,
        before: &[&Expr],
    ) -> Result<Operand, String> {
        match ty {
            Type::List(_) | Type::Map(..) => self.list_operand(container, before),
            _ => self.expr(container),
        }
    }

    /// Lowers a test of whether `container`, a value of type `ty`, holds the value whose values
    /// are `needle`, as `needle in container` and `container.contains(needle)` make, to 1 or 0:
    /// a map's key through `stone.map_find`, a string's text through `stone.str_position`, and
    /// a list's element by a loop that compares each element with `needle` as `==` does, from
    /// the first, stopping at the first that is equal. The elements stay borrowed from the list.
    ///
    /// For example, `x in xs` of a `list[int]`, with `xs` in `v0` and `x` in `v1`, loops over
    /// `v5 = list_get v0, v4` and `br_eq v1, v5`.
    fn contains_values(
        &mut self,
        container: Operand,
        ty: &Type,
        needle: &[Operand],
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let elem = match ty {
            Type::Map(key, _) => {
                let first = self.map_find(container, needle[0], key, None);
                return Ok(self.at_least_zero(first, into));
            }
            Type::Str | Type::Bytes => {
                // the routine gives -1 when the text never appears
                let routine = match ty {
                    Type::Str => "stone.str_position",
                    _ => "stone.bytes_find",
                };
                let position = self.runtime_call(routine, vec![container, needle[0]], None);
                return Ok(self.at_least_zero(position, into));
            }
            Type::List(elem) => (**elem).clone(),
            _ => return Err("'in' needs a str, bytes, list, or map".to_string()),
        };
        let width = self.leaves(&elem).len();
        let count = self.element_count(container, width);
        // a fresh vreg, since the loop still reads `needle` after it writes the result
        let result = self.fresh();
        let index = self.fresh();
        self.push(Inst::Copy {
            dst: index,
            src: Operand::Imm(0),
        });
        let (header, body, next) = (self.new_block(), self.new_block(), self.new_block());
        let (found, missing, done) = (self.new_block(), self.new_block(), self.new_block());
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(index),
            rhs: count,
            then: body,
            otherwise: missing,
        });
        self.start(body);
        let uncounted = vec![None; width];
        let element = self.load_slots(container, Operand::Reg(index), width, 0..width, &uncounted);
        self.equal_branch(&elem, needle, &element, found, next);
        self.start(next);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: index,
            lhs: Operand::Reg(index),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(found);
        self.push(Inst::Copy {
            dst: result,
            src: Operand::Imm(1),
        });
        self.jump(done);
        self.start(missing);
        self.push(Inst::Copy {
            dst: result,
            src: Operand::Imm(0),
        });
        self.start(done);
        Ok(self.finish(Operand::Reg(result), into))
    }

    /// Writes 1 if `value` is at least 0 and 0 if not, as for a position that is -1 when
    /// something is missing.
    fn at_least_zero(&mut self, value: Operand, into: Option<VReg>) -> Operand {
        let dst = self.dst(into);
        self.push(Inst::Compare {
            cond: Cond::Ge,
            float: false,
            dst,
            lhs: value,
            rhs: Operand::Imm(0),
        });
        Operand::Reg(dst)
    }

    /// Lowers `needle in container` or `needle not in container`, evaluating the value it looks
    /// for before the container, to 1 or 0.
    fn membership(
        &mut self,
        needle: &Expr,
        op: &crate::ast::CompOp,
        container: &Expr,
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let values = self.expr_flat(needle)?;
        let ty = self.type_of(container)?;
        let held = self.container(container, &ty, &[])?;
        let negated = *op == crate::ast::CompOp::NotIn;
        let found =
            self.contains_values(held, &ty, &values, (!negated).then_some(into).flatten())?;
        for value in values {
            self.release_temp(value);
        }
        self.release_temp(held);
        if !negated {
            return Ok(found);
        }
        let dst = self.dst(into);
        self.push(Inst::Not { dst, src: found });
        Ok(Operand::Reg(dst))
    }

    /// Lowers a comparison used as a value, which is 1 or 0. Each counted operand is released
    /// once the links that read it are done, including when a chain stops early.
    fn compare(
        &mut self,
        left: &Expr,
        ops: &[crate::ast::CompOp],
        comparators: &[Expr],
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        if let ([op], [container]) = (ops, comparators)
            && op.is_membership()
        {
            return self.membership(left, op, container, into);
        }
        // a single link can write its destination directly, but a chain writes it more than once
        let result = if ops.len() == 1 {
            self.dst(into)
        } else {
            self.fresh()
        };
        let end = (ops.len() > 1).then(|| self.new_block());
        // a tuple or struct is several values, and each other value one
        let mut lhs_values = self.expr_flat(left)?;
        for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
            let rhs_values = self.expr_flat(comparator)?;
            let (lhs, rhs) = (lhs_values[0], rhs_values[0]);
            let cond = Cond::of(op).ok_or("'in' is not a comparison of two values")?;
            match self.type_of(comparator)? {
                // ordered by its `cmp`, whose ordering's tag says which comes first
                ty if !matches!(cond, Cond::Eq | Cond::Ne) && self.implements(&ty, ORD) => {
                    let order = self.compare_call(&ty, "cmp", &lhs_values, &rhs_values);
                    let tag = self.fresh();
                    self.push(Inst::EnumTag {
                        dst: tag,
                        value: Operand::Reg(order),
                    });
                    // `less` is 0, `equal` 1, and `greater` 2
                    let (cond, against) = match cond {
                        Cond::Lt => (Cond::Eq, 0),
                        Cond::Le => (Cond::Ne, 2),
                        Cond::Gt => (Cond::Eq, 2),
                        _ => (Cond::Ne, 0),
                    };
                    self.push(Inst::Compare {
                        cond,
                        float: false,
                        dst: result,
                        lhs: Operand::Reg(tag),
                        rhs: Operand::Imm(against),
                    });
                    self.push(Inst::Release {
                        src: Operand::Reg(order),
                        kind: RcKind::Enum,
                    });
                }
                // compared value by value, which only == and != do
                ty if is_compound(&ty) => {
                    let (yes, no, join) = (self.new_block(), self.new_block(), self.new_block());
                    self.equal_branch(&ty, &lhs_values, &rhs_values, yes, no);
                    let equal = i64::from(cond == Cond::Eq);
                    self.start(yes);
                    self.push(Inst::Copy {
                        dst: result,
                        src: Operand::Imm(equal),
                    });
                    self.jump(join);
                    self.start(no);
                    self.push(Inst::Copy {
                        dst: result,
                        src: Operand::Imm(1 - equal),
                    });
                    self.start(join);
                }
                // strings and bytes compare by contents
                ty @ (Type::Str | Type::Bytes) if cond == Cond::Ne => {
                    let equal = self.fresh();
                    self.push(Inst::Call {
                        dsts: vec![equal],
                        callee: Callee::Runtime(equality_routine(&ty)),
                        args: vec![lhs, rhs],
                    });
                    self.push(Inst::Not {
                        dst: result,
                        src: Operand::Reg(equal),
                    });
                }
                ty @ (Type::Str | Type::Bytes) if cond == Cond::Eq => self.push(Inst::Call {
                    dsts: vec![result],
                    callee: Callee::Runtime(equality_routine(&ty)),
                    args: vec![lhs, rhs],
                }),
                // ordered by a call that returns -1, 0, or 1
                ty @ (Type::Str | Type::Bytes) => {
                    let order = self.runtime_call(order_routine(&ty), vec![lhs, rhs], None);
                    self.push(Inst::Compare {
                        cond,
                        float: false,
                        dst: result,
                        lhs: order,
                        rhs: Operand::Imm(0),
                    });
                }
                ty => self.push(Inst::Compare {
                    cond,
                    float: ty == Type::Float,
                    dst: result,
                    lhs,
                    rhs,
                }),
            }
            for value in lhs_values {
                self.release_temp(value);
            }
            match end {
                Some(end) if i + 1 < ops.len() => {
                    // the next link reads rhs, so stopping here releases it on the way out
                    let owned = self.owned_values(&rhs_values);
                    let next = self.new_block();
                    let stop = if owned.is_empty() {
                        end
                    } else {
                        self.new_block()
                    };
                    self.terminate(Terminator::Branch {
                        cond: Operand::Reg(result),
                        then: next,
                        otherwise: stop,
                    });
                    if !owned.is_empty() {
                        self.start(stop);
                        for (src, kind) in owned {
                            self.push(Inst::Release { src, kind });
                        }
                        self.jump(end);
                    }
                    self.start(next);
                }
                _ => {
                    for &value in &rhs_values {
                        self.release_temp(value);
                    }
                }
            }
            lhs_values = rhs_values;
        }
        if let Some(end) = end {
            self.start(end);
        }
        Ok(self.finish(Operand::Reg(result), into))
    }

    /// Makes the ordering `lhs.cmp(rhs)` gives for two ints, floats, or strings, of type `ty`:
    /// `less`, `equal`, or `greater`, the static variants with tags 0, 1, and 2. Floats are in
    /// the order `sort` uses: nan after every other float, all nans equal, and `-0.0` before
    /// `0.0`, which compare equal, so their sign bits decide.
    fn ordering(
        &mut self,
        ty: &Type,
        lhs: Operand,
        rhs: Operand,
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let dst = self.dst(into);
        let blocks = [self.new_block(), self.new_block(), self.new_block()];
        let end = self.new_block();
        let [less, equal, greater] = blocks;
        let branch = |this: &mut Self, cond, float, lhs, rhs, then, otherwise| {
            this.terminate(Terminator::CmpBranch {
                cond,
                float,
                lhs,
                rhs,
                then,
                otherwise,
            });
        };
        match ty {
            Type::Int => {
                let not_less = self.new_block();
                branch(self, Cond::Lt, false, lhs, rhs, less, not_less);
                self.start(not_less);
                branch(self, Cond::Gt, false, lhs, rhs, greater, equal);
            }
            Type::Str | Type::Bytes => {
                let order = self.runtime_call(order_routine(ty), vec![lhs, rhs], None);
                let not_less = self.new_block();
                branch(
                    self,
                    Cond::Lt,
                    false,
                    order,
                    Operand::Imm(0),
                    less,
                    not_less,
                );
                self.start(not_less);
                branch(
                    self,
                    Cond::Gt,
                    false,
                    order,
                    Operand::Imm(0),
                    greater,
                    equal,
                );
            }
            Type::Float => {
                let names = [(); 6].map(|_| self.new_block());
                let [
                    lhs_nan,
                    lhs_number,
                    not_less,
                    same,
                    lhs_negative,
                    lhs_positive,
                ] = names;
                // nan is the only float unequal to itself
                branch(self, Cond::Ne, true, lhs, lhs, lhs_nan, lhs_number);
                self.start(lhs_nan);
                branch(self, Cond::Ne, true, rhs, rhs, equal, greater);
                self.start(lhs_number);
                let rhs_number = self.new_block();
                branch(self, Cond::Ne, true, rhs, rhs, less, rhs_number);
                self.start(rhs_number);
                branch(self, Cond::Lt, true, lhs, rhs, less, not_less);
                self.start(not_less);
                branch(self, Cond::Gt, true, lhs, rhs, greater, same);
                // equal numbers, which only zeros' sign bits tell apart, read as ints
                self.start(same);
                branch(
                    self,
                    Cond::Lt,
                    false,
                    lhs,
                    Operand::Imm(0),
                    lhs_negative,
                    lhs_positive,
                );
                self.start(lhs_negative);
                branch(self, Cond::Lt, false, rhs, Operand::Imm(0), equal, less);
                self.start(lhs_positive);
                branch(self, Cond::Lt, false, rhs, Operand::Imm(0), greater, equal);
            }
            other => return Err(format!("cannot order {other}")),
        }
        for (tag, block) in blocks.into_iter().enumerate() {
            self.start(block);
            self.push(Inst::VariantStatic { dst, tag });
            self.push(Inst::Retain {
                src: Operand::Reg(dst),
            });
            self.jump(end);
        }
        self.start(end);
        Ok(Operand::Reg(dst))
    }

    /// Lowers the method call `receiver.name(args)`, evaluating the receiver before the arguments
    /// like the interpreter. Its result is counted as `kind`.
    ///
    /// For example, `xs.len()` on a list becomes `v1 = len v0`, and on a string it calls
    /// `stone.str_len`.
    fn method(
        &mut self,
        receiver: &Expr,
        name: &str,
        args: &[Expr],
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        match name {
            "cmp" => {
                let ty = self.type_of(receiver)?;
                let lhs = self.expr(receiver)?;
                let rhs = self.expr(&args[0])?;
                let order = self.ordering(&ty, lhs, rhs, into)?;
                self.release_temp(lhs);
                self.release_temp(rhs);
                self.own(order, Some(RcKind::Enum));
                Ok(order)
            }
            // as `str` of the value writes it
            "display" => {
                let ty = self.type_of(receiver)?;
                match ty {
                    Type::Str => {
                        let value = self.expr(receiver)?;
                        Ok(self.finish(value, into))
                    }
                    _ => self.builtin_conversion((&Type::Str, &ty), receiver, into, kind),
                }
            }
            // a type that compares part by part compares as `==` does
            "eq" => {
                let ty = self.type_of(receiver)?;
                let lhs = self.expr_flat(receiver)?;
                let rhs = self.expr_flat(&args[0])?;
                let (yes, no, join) = (self.new_block(), self.new_block(), self.new_block());
                let result = self.dst(into);
                self.equal_branch(&ty, &lhs, &rhs, yes, no);
                self.start(yes);
                self.push(Inst::Copy {
                    dst: result,
                    src: Operand::Imm(1),
                });
                self.jump(join);
                self.start(no);
                self.push(Inst::Copy {
                    dst: result,
                    src: Operand::Imm(0),
                });
                self.start(join);
                for value in lhs.into_iter().chain(rhs) {
                    self.release_temp(value);
                }
                Ok(self.finish(Operand::Reg(result), into))
            }
            "is_some" | "is_none" | "is_ok" | "is_err" => {
                let tag_of = match name {
                    "is_some" => self.tag_of(OPTION, "some")?,
                    "is_none" => self.tag_of(OPTION, "none")?,
                    "is_ok" => self.tag_of(RESULT, "ok")?,
                    _ => self.tag_of(RESULT, "err")?,
                };
                let option = self.expr(receiver)?;
                let tag = self.fresh();
                self.push(Inst::EnumTag {
                    dst: tag,
                    value: option,
                });
                let dst = self.dst(into);
                self.push(Inst::Compare {
                    cond: Cond::Eq,
                    float: false,
                    dst,
                    lhs: Operand::Reg(tag),
                    rhs: Operand::Imm(tag_of as i64),
                });
                self.release_temp(option);
                Ok(Operand::Reg(dst))
            }
            "unwrap" | "unwrap_or" | "unwrap_err" => {
                let values = self.option_value(receiver, name, args.first())?;
                Ok(self.finish(values[0], into))
            }
            "ok" | "err" => {
                let ty = self.type_of(receiver)?;
                let Type::Enum {
                    args: type_args, ..
                } = &ty
                else {
                    return Err("only a result has 'ok' and 'err'".to_string());
                };
                let position = usize::from(name == "err");
                let held = type_args.get(position).cloned().unwrap_or(Type::Int);
                let kinds: Vec<Option<RcKind>> =
                    self.leaves(&held).iter().map(RcKind::of).collect();
                let tag_of = self.tag_of(RESULT, name)?;
                let result = self.expr(receiver)?;
                let tag = self.fresh();
                self.push(Inst::EnumTag {
                    dst: tag,
                    value: result,
                });
                let option = self.option_if(
                    (Cond::Eq, Operand::Reg(tag), Operand::Imm(tag_of as i64)),
                    into,
                    |lowerer| {
                        let values = lowerer.field_values(result, 0, kinds.len());
                        // the option keeps a reference of its own
                        for (&value, kind) in values.iter().zip(&kinds) {
                            if kind.is_some() {
                                lowerer.push(Inst::Retain { src: value });
                            }
                        }
                        values.into_iter().zip(kinds).collect()
                    },
                );
                self.release_temp(result);
                Ok(option)
            }
            "contains" => {
                let [needle] = args else {
                    return Err("contains() takes one value".to_string());
                };
                // the receiver, then the value it looks for
                let ty = self.type_of(receiver)?;
                let container = self.container(receiver, &ty, &[needle])?;
                let needle = self.expr_flat(needle)?;
                let found = self.contains_values(container, &ty, &needle, into)?;
                for value in needle {
                    self.release_temp(value);
                }
                self.release_temp(container);
                Ok(found)
            }
            "get" => {
                let key_ty = self.map_key(receiver).ok_or("only a map has 'get'")?;
                let kinds = self.element_kinds(receiver);
                let map = self.list_operand(receiver, &args.iter().collect::<Vec<_>>())?;
                let key = self.expr(&args[0])?;
                let first = self.map_find(map, key, &key_ty, None);
                self.release_temp(key);
                let option = self.option_if((Cond::Ge, first, Operand::Imm(0)), into, |this| {
                    // the key is slot 0 of an entry, and reading a value retains it
                    let values = this.load_slots(map, first, 1, 1..kinds.len(), &kinds);
                    values.into_iter().zip(kinds[1..].iter().copied()).collect()
                });
                self.release_temp(map);
                Ok(option)
            }
            "len" if self.holds_flat(receiver) || self.map_key(receiver).is_some() => {
                let width = self.element_layout(receiver).0;
                let list = self.list_operand(receiver, &[])?;
                let mut count = self.element_count(list, width);
                if self.map_key(receiver).is_some() && self.context.shrinks_maps {
                    // entries removed but not closed up yet still take their place
                    let holes = self.runtime_call("stone.map_holes", vec![list], None);
                    let live = self.fresh();
                    self.push(Inst::Binary {
                        op: BinOp::Sub,
                        dst: live,
                        lhs: count,
                        rhs: holes,
                    });
                    count = Operand::Reg(live);
                }
                self.release_temp(list);
                Ok(self.finish(count, into))
            }
            "len" if matches!(self.type_of(receiver)?, Type::List(_)) => {
                let list = self.list_operand(receiver, &[])?;
                let dst = self.dst(into);
                self.push(Inst::ListLen { dst, list });
                self.release_temp(list);
                Ok(Operand::Reg(dst))
            }
            "append" | "insert" if self.holds_flat(receiver) => {
                self.add_element(receiver, name, args)?;
                Ok(self.finish(Operand::Imm(0), into))
            }
            // a string's bytes are laid out as bytes are, so they share it
            "bytes" => {
                let text = self.expr(receiver)?;
                Ok(self.finish(text, into))
            }
            "text" => {
                let bytes = self.expr(receiver)?;
                let result = self.runtime_call("stone.bytes_text", vec![bytes], into);
                self.release_temp(bytes);
                self.own(result, kind);
                Ok(result)
            }
            "len" => {
                let text = self.expr(receiver)?;
                let dst = self.dst(into);
                self.push(Inst::Call {
                    dsts: vec![dst],
                    callee: Callee::Runtime("stone.str_len"),
                    args: vec![text],
                });
                self.release_temp(text);
                Ok(Operand::Reg(dst))
            }
            "append" => {
                // the receiver's indexes, then the arguments, then the change
                let elem = self.element_kind(receiver);
                let indexes = self.place_indexes(receiver)?;
                let mut values = Vec::new();
                for arg in args {
                    let value = self.expr(arg)?;
                    // the list keeps a reference to what it holds
                    let last_uses = std::mem::take(&mut self.statement_last_uses);
                    self.take_from(arg, value, elem, &last_uses);
                    self.statement_last_uses = last_uses;
                    values.push(value);
                }
                let list = self.unique_place(receiver, &indexes)?;
                values.insert(0, list);
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.list_append"),
                    args: values,
                });
                // append returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "pop" if args.is_empty() => self.pop_last(receiver, into),
            "pop" if self.map_key(receiver).is_some() => self.map_pop(receiver, &args[0], into),
            "keys" => self.map_column(receiver, 0..1, into),
            "values" => {
                let width = self.element_layout(receiver).0;
                self.map_column(receiver, 1..width, into)
            }
            "items" => {
                // a map's entries are laid out as a list of (key, value) tuples, with an index the
                // list never reads
                let map = self.expr(receiver)?;
                self.close_up(map);
                self.take(map, Some(RcKind::List));
                let list = self.fresh();
                self.push(Inst::Copy {
                    dst: list,
                    src: map,
                });
                self.own(Operand::Reg(list), Some(RcKind::List));
                Ok(self.finish(Operand::Reg(list), into))
            }
            "pop" => {
                // the receiver's indexes, then the index, then the change
                let indexes = self.place_indexes(receiver)?;
                let index = self.expr(&args[0])?;
                let list = self.unique_place(receiver, &indexes)?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                self.fail_unless(Cond::Ne, Operand::Reg(len), Operand::Imm(0), POP_EMPTY);
                let position = self.list_position(index, Operand::Reg(len), false, POP_RANGE);
                // the list's reference to the element moves to the result
                let result = self.runtime_call("stone.list_pop", vec![list, position], into);
                self.own(result, kind);
                Ok(result)
            }
            "insert" => {
                let elem = self.element_kind(receiver);
                let indexes = self.place_indexes(receiver)?;
                let [index, item] = args else {
                    return Err("insert() takes an index and a value".to_string());
                };
                let index = self.expr(index)?;
                let value = self.expr(item)?;
                // the list keeps a reference to what it holds
                let last_uses = std::mem::take(&mut self.statement_last_uses);
                self.take_from(item, value, elem, &last_uses);
                self.statement_last_uses = last_uses;
                let list = self.unique_place(receiver, &indexes)?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                let position = self.list_position(index, Operand::Reg(len), true, INDEX_RANGE);
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.list_insert"),
                    args: vec![list, position, value],
                });
                Ok(self.finish(Operand::Imm(0), into))
            }
            "extend" => {
                let indexes = self.place_indexes(receiver)?;
                let [other] = args else {
                    return Err("extend() takes a list".to_string());
                };
                // the elements are retained, so the call only borrows the other list
                let other = self.expr(other)?;
                let list = self.unique_place(receiver, &indexes)?;
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.list_extend"),
                    args: vec![list, other],
                });
                self.release_temp(other);
                Ok(self.finish(Operand::Imm(0), into))
            }
            "sort" => {
                // sorting only moves the elements, so their counts stay as they are
                let label = match self.span_type(receiver.span) {
                    Some(Type::List(elem)) => match *elem {
                        Type::Int => "stone.list_sort_int",
                        Type::Float => "stone.list_sort_float",
                        Type::Str => "stone.list_sort_str",
                        // others sort by their `cmp`, in the prelude's sort, which moves the
                        // list in and gives it back sorted, as a method changing `self` does
                        other => {
                            let label = self.request(Instance {
                                function: PRELUDE_SORT.to_string(),
                                args: vec![other],
                            });
                            self.dispatched = Some((receiver.span, label));
                            self.changing_call(
                                (PRELUDE_SORT, receiver.span),
                                receiver,
                                &[],
                                None,
                                &[],
                            )?;
                            return Ok(self.finish(Operand::Imm(0), into));
                        }
                    },
                    _ => return Err("sort() is called on a list".to_string()),
                };
                let indexes = self.place_indexes(receiver)?;
                let list = self.unique_place(receiver, &indexes)?;
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime(label),
                    args: vec![list],
                });
                Ok(self.finish(Operand::Imm(0), into))
            }
            "strip" | "split" | "splitlines" | "lower" | "upper" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let label = match (name, values.len()) {
                    ("strip", _) => "stone.str_strip",
                    ("splitlines", _) => "stone.str_splitlines",
                    ("lower", _) => "stone.str_lower",
                    ("upper", _) => "stone.str_upper",
                    (_, 1) => "stone.str_split_ws",
                    _ => "stone.str_split",
                };
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
            }
            "find" | "rfind" => {
                let text = self.expr(receiver)?;
                let part = self.expr(&args[0])?;
                let label = match name {
                    "find" => "stone.str_position",
                    _ => "stone.str_rposition",
                };
                let position = self.runtime_call(label, vec![text, part], None);
                self.release_temp(text);
                self.release_temp(part);
                // the routine gives -1 when the text never appears
                Ok(
                    self.option_if((Cond::Ge, position, Operand::Imm(0)), into, |_| {
                        vec![(position, None)]
                    }),
                )
            }
            "join" | "count" | "replace" | "startswith" | "endswith" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                if name == "replace" && args.len() == 2 {
                    // without a count, every match is replaced
                    values.push(Operand::Imm(-1));
                }
                let label = match name {
                    "join" => "stone.str_join",
                    "count" => "stone.str_count",
                    "replace" => "stone.str_replace",
                    "startswith" => "stone.str_starts",
                    _ => "stone.str_ends",
                };
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
            }
            "send" | "read_line" | "read" | "read_all" | "accept" | "port" | "close"
            | "set_timeout" | "send_bytes" | "read_bytes" | "read_exact" | "read_all_bytes" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let label = match name {
                    "send" => "stone.net_send",
                    "read_line" => "stone.net_read_line",
                    "read" => "stone.net_read",
                    "read_all" => "stone.net_read_all",
                    "accept" => "stone.net_accept",
                    "port" => "stone.net_port",
                    "set_timeout" => "stone.net_set_timeout",
                    // bytes are laid out as strings are, so they are sent the same way
                    "send_bytes" => "stone.net_send",
                    "read_bytes" => "stone.net_read_bytes",
                    "read_exact" => "stone.net_read_exact",
                    "read_all_bytes" => "stone.net_read_all_bytes",
                    _ => "stone.net_close",
                };
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
            }
            _ => Err(format!("there is no method '{name}'")),
        }
    }

    /// Lowers a call to a function of the `math` module, which never calls the runtime: each
    /// becomes compares and branches that do what `stdlib::math` does, in the same order.
    ///
    /// For example, `math.abs(x)` of an int fails if `x` is the smallest int, then negates it if
    /// it is negative, and `math.max(xs)` loops over the list, keeping an element only when it is
    /// greater than the one kept so far.
    fn math(&mut self, name: &str, args: &[Expr], into: Option<VReg>) -> Result<Operand, String> {
        let float = match args {
            [arg] => match self.type_of(arg)? {
                Type::List(elem) => *elem == Type::Float,
                ty => ty == Type::Float,
            },
            [first, ..] => self.type_of(first)? == Type::Float,
            [] => return Err(format!("'{name}' takes an argument")),
        };
        match name {
            "math.abs" => {
                let x = self.expr(&args[0])?;
                // the result is written last, since `x` may be the variable it is assigned to
                let result = self.fresh();
                let (negate, keep, done) = (self.new_block(), self.new_block(), self.new_block());
                if float {
                    // `0.0 - x` rather than flipping the sign, so -0.0 becomes 0.0
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Le,
                        float: true,
                        lhs: x,
                        rhs: Operand::Imm(0),
                        then: negate,
                        otherwise: keep,
                    });
                    self.start(negate);
                    self.push(Inst::FloatBinary {
                        op: BinOp::Sub,
                        dst: result,
                        lhs: Operand::Imm(0),
                        rhs: x,
                    });
                } else {
                    let (overflow, check) = (self.new_block(), self.new_block());
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Eq,
                        float: false,
                        lhs: x,
                        rhs: Operand::Imm(i64::MIN),
                        then: overflow,
                        otherwise: check,
                    });
                    self.start(overflow);
                    self.terminate(Terminator::Fail(math::ABS_OVERFLOW.to_string(), Vec::new()));
                    self.start(check);
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Lt,
                        float: false,
                        lhs: x,
                        rhs: Operand::Imm(0),
                        then: negate,
                        otherwise: keep,
                    });
                    self.start(negate);
                    self.push(Inst::Neg {
                        dst: result,
                        src: x,
                    });
                }
                self.jump(done);
                self.start(keep);
                self.push(Inst::Copy {
                    dst: result,
                    src: x,
                });
                self.start(done);
                Ok(self.finish(Operand::Reg(result), into))
            }
            "math.min" | "math.max" => {
                // a later value replaces the kept one only if it is strictly better, as in Python
                let better = if name == "math.max" {
                    Cond::Gt
                } else {
                    Cond::Lt
                };
                let result = self.fresh();
                if let [list] = args {
                    let empty = if name == "math.max" {
                        math::MAX_OF_EMPTY
                    } else {
                        math::MIN_OF_EMPTY
                    };
                    self.fold_list(list, better, float, empty, result)?;
                } else {
                    let mut values = Vec::new();
                    for arg in args {
                        values.push(self.expr(arg)?);
                    }
                    self.push(Inst::Copy {
                        dst: result,
                        src: values[0],
                    });
                    for &value in &values[1..] {
                        let (replace, next) = (self.new_block(), self.new_block());
                        self.terminate(Terminator::CmpBranch {
                            cond: better,
                            float,
                            lhs: value,
                            rhs: Operand::Reg(result),
                            then: replace,
                            otherwise: next,
                        });
                        self.start(replace);
                        self.push(Inst::Copy {
                            dst: result,
                            src: value,
                        });
                        self.start(next);
                    }
                }
                Ok(self.finish(Operand::Reg(result), into))
            }
            "math.sqrt" => {
                let mut x = self.expr(&args[0])?;
                if !float {
                    let converted = self.fresh();
                    self.push(Inst::IntToFloat {
                        dst: converted,
                        src: x,
                    });
                    x = Operand::Reg(converted);
                }
                let (negative, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Lt,
                    float: true,
                    lhs: x,
                    rhs: Operand::Imm(0),
                    then: negative,
                    otherwise: ok,
                });
                self.start(negative);
                self.terminate(Terminator::Fail(math::DOMAIN_ERROR.to_string(), Vec::new()));
                self.start(ok);
                let dst = self.dst(into);
                self.push(Inst::FloatSqrt { dst, src: x });
                Ok(Operand::Reg(dst))
            }
            "math.floor" => {
                let x = self.expr(&args[0])?;
                if !float {
                    return Ok(self.finish(x, into));
                }
                // truncate toward zero, which fails on nan or overflow, then step down if that
                // went up, which only happens for a negative number with a fraction
                let truncated = self.fresh();
                self.push(Inst::FloatToInt {
                    dst: truncated,
                    src: x,
                });
                let back = self.fresh();
                self.push(Inst::IntToFloat {
                    dst: back,
                    src: Operand::Reg(truncated),
                });
                let (down, done) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Lt,
                    float: true,
                    lhs: x,
                    rhs: Operand::Reg(back),
                    then: down,
                    otherwise: done,
                });
                self.start(down);
                self.push(Inst::Binary {
                    op: BinOp::Sub,
                    dst: truncated,
                    lhs: Operand::Reg(truncated),
                    rhs: Operand::Imm(1),
                });
                self.start(done);
                Ok(self.finish(Operand::Reg(truncated), into))
            }
            _ => Err(format!("there is no function '{name}'")),
        }
    }

    /// Folds the numbers in the list `list` into `result` for `math.min` or `math.max`, keeping
    /// an element only when it compares `better` than the one kept so far, and failing with
    /// `empty` for an empty list.
    fn fold_list(
        &mut self,
        list: &Expr,
        better: Cond,
        float: bool,
        empty: &str,
        result: VReg,
    ) -> Result<(), String> {
        let list = self.list_operand(list, &[])?;
        let len = self.fresh();
        self.push(Inst::ListLen { dst: len, list });
        let (fail, first) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(len),
            rhs: Operand::Imm(0),
            then: fail,
            otherwise: first,
        });
        self.start(fail);
        self.terminate(Terminator::Fail(empty.to_string(), Vec::new()));
        self.start(first);
        self.push(Inst::ListGet {
            dst: result,
            list,
            index: Operand::Imm(0),
            offset: 0,
        });
        let index = self.fresh();
        self.push(Inst::Copy {
            dst: index,
            src: Operand::Imm(1),
        });

        let header = self.new_block();
        let body = self.new_block();
        let replace = self.new_block();
        let next = self.new_block();
        let exit = self.new_block();
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(index),
            rhs: Operand::Reg(len),
            then: body,
            otherwise: exit,
        });
        self.start(body);
        let element = self.fresh();
        self.push(Inst::ListGet {
            dst: element,
            list,
            index: Operand::Reg(index),
            offset: 0,
        });
        self.terminate(Terminator::CmpBranch {
            cond: better,
            float,
            lhs: Operand::Reg(element),
            rhs: Operand::Reg(result),
            then: replace,
            otherwise: next,
        });
        self.start(replace);
        self.push(Inst::Copy {
            dst: result,
            src: Operand::Reg(element),
        });
        self.start(next);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: index,
            lhs: Operand::Reg(index),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        self.release_temp(list);
        Ok(())
    }

    /// Calls the runtime routine `label` with `args`, returning its result.
    fn runtime_call(
        &mut self,
        label: &'static str,
        args: Vec<Operand>,
        into: Option<VReg>,
    ) -> Operand {
        let dst = self.dst(into);
        self.push(Inst::Call {
            dsts: vec![dst],
            callee: Callee::Runtime(label),
            args,
        });
        Operand::Reg(dst)
    }

    /// Lowers a call to a builtin or a stone function, whose result is counted as `kind`. The
    /// call borrows its arguments, and temporaries among them are released after it.
    fn call(
        &mut self,
        name: &str,
        call: Span,
        args: &[Expr],
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        match name {
            "print" | "eprint" => {
                // every argument is evaluated before anything prints, like the interpreter
                let mut values = Vec::new();
                for arg in args {
                    values.push((self.expr_flat(arg)?, self.type_of(arg)?));
                }
                // the whole line is built first, so a print that fails writes nothing
                let outer = self.runtime_call("stone.text_begin", vec![], None);
                let count = values.len();
                for (i, (leaves, ty)) in values.iter().enumerate() {
                    self.print_flat(ty, leaves, false);
                    if i + 1 < count {
                        self.print_char(' ');
                    }
                }
                let write = match name {
                    "print" => "stone.text_print",
                    _ => "stone.text_eprint",
                };
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime(write),
                    args: vec![outer],
                });
                for value in values.into_iter().flat_map(|(leaves, _)| leaves) {
                    self.release_temp(value);
                }
                // print returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "input" => {
                // no prompt is a null pointer
                let prompt = match args {
                    [prompt] => self.expr(prompt)?,
                    _ => Operand::Imm(0),
                };
                // a null line is the end of the input
                let line = self.runtime_call("stone.input", vec![prompt], None);
                self.release_temp(prompt);
                let (ok, err) = (self.tag_of(RESULT, "ok")?, self.tag_of(RESULT, "err")?);
                let dst = self.dst(into);
                let (read, ended, end) = (self.new_block(), self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Ne,
                    float: false,
                    lhs: line,
                    rhs: Operand::Imm(0),
                    then: read,
                    otherwise: ended,
                });
                self.start(read);
                // the new line's reference goes to the ok
                self.new_variant(dst, ok, vec![line], &[Some(RcKind::Str)]);
                self.jump(end);
                self.start(ended);
                let message = self.fresh();
                self.push(Inst::StrAddr {
                    dst: message,
                    text: END_OF_INPUT.to_string(),
                });
                self.push(Inst::Retain {
                    src: Operand::Reg(message),
                });
                self.new_variant(dst, err, vec![Operand::Reg(message)], &[Some(RcKind::Str)]);
                self.jump(end);
                self.start(end);
                self.own(Operand::Reg(dst), Some(RcKind::Enum));
                Ok(Operand::Reg(dst))
            }
            "eof" => Ok(self.runtime_call("stone.eof", vec![], into)),
            "args" => {
                let result = self.runtime_call("stone.args", vec![], into);
                self.own(result, kind);
                Ok(result)
            }
            "os.exit" => {
                let code = self.expr(&args[0])?;
                // it never returns, but the call is a statement like any other
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.os_exit"),
                    args: vec![code],
                });
                Ok(self.finish(Operand::Imm(0), into))
            }
            _ if name.starts_with("math.") => self.math(name, args, into),
            "random.randint" => {
                let low = self.expr(&args[0])?;
                let high = self.expr(&args[1])?;
                let (empty, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Gt,
                    float: false,
                    lhs: low,
                    rhs: high,
                    then: empty,
                    otherwise: ok,
                });
                self.start(empty);
                self.terminate(Terminator::Fail(
                    random::EMPTY_RANGE.to_string(),
                    Vec::new(),
                ));
                self.start(ok);
                // wraps to 0, which the runtime takes as 2^64, for the whole range of ints
                let span = self.fresh();
                self.push(Inst::Binary {
                    op: BinOp::Sub,
                    dst: span,
                    lhs: high,
                    rhs: low,
                });
                self.push(Inst::Binary {
                    op: BinOp::Add,
                    dst: span,
                    lhs: Operand::Reg(span),
                    rhs: Operand::Imm(1),
                });
                let offset =
                    self.runtime_call("stone.random_below", vec![Operand::Reg(span)], None);
                let dst = self.dst(into);
                self.push(Inst::Binary {
                    op: BinOp::Add,
                    dst,
                    lhs: low,
                    rhs: offset,
                });
                Ok(Operand::Reg(dst))
            }
            "random.choice" => {
                let list = self.list_operand(&args[0], &[])?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                let (empty, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Eq,
                    float: false,
                    lhs: Operand::Reg(len),
                    rhs: Operand::Imm(0),
                    then: empty,
                    otherwise: ok,
                });
                self.start(empty);
                self.terminate(Terminator::Fail(
                    random::EMPTY_CHOICE.to_string(),
                    Vec::new(),
                ));
                self.start(ok);
                let index = self.runtime_call("stone.random_below", vec![Operand::Reg(len)], None);
                let dst = self.dst(into);
                self.push(Inst::ListGet {
                    dst,
                    list,
                    index,
                    offset: 0,
                });
                // the element gets a reference of its own, as when a subscript reads it
                if kind.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                    self.own(Operand::Reg(dst), kind);
                }
                self.release_temp(list);
                Ok(Operand::Reg(dst))
            }
            "time.sleep" => {
                let mut seconds = self.expr(&args[0])?;
                if self.type_of(&args[0])? == Type::Int {
                    let converted = self.fresh();
                    self.push(Inst::IntToFloat {
                        dst: converted,
                        src: seconds,
                    });
                    seconds = Operand::Reg(converted);
                }
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.time_sleep"),
                    args: vec![seconds],
                });
                // sleep returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "os.env" => {
                let name = self.expr(&args[0])?;
                let value = self.runtime_call("stone.os_env", vec![name], None);
                self.release_temp(name);
                // the routine gives a new string, or 0 when the variable is not set
                Ok(
                    self.option_if((Cond::Ne, value, Operand::Imm(0)), into, |_| {
                        vec![(value, Some(RcKind::Str))]
                    }),
                )
            }
            _ if let Some(label) = module_routine(name) => {
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
            }
            _ => Ok(self.user_call(name, call, args, into, &[kind])?[0]),
        }
    }

    /// Calls a function of an `impl` block as `method` resolved `call`, returning one value for
    /// each value of the result: an associated function with `args`, or a method with `receiver`
    /// as `self` and then `args`. A method that does not change `self` borrows the receiver, and
    /// one that does changes it, as [`Lowerer::changing_call`] lowers.
    fn struct_call(
        &mut self,
        call: &Expr,
        method: &Method,
        receiver: &Expr,
        args: &[Expr],
        into: Option<VReg>,
    ) -> Result<Vec<Operand>, String> {
        let kinds = self.leaf_kinds(call)?;
        let function = match &method.implementations {
            // through a bound, the receiver's type in this copy picks the method
            Some(_) => {
                let ty = self
                    .span_type(receiver.span)
                    .ok_or("receiver has no type")?;
                let trait_args: Vec<Type> =
                    method.trait_args.iter().map(|a| self.fill(a)).collect();
                // a number, a string, or a type compared part by part has the builtin method
                if matches!(method.function.as_str(), "eq.eq" | "ord.cmp")
                    && method.resolve(&ty, &trait_args).is_none()
                {
                    let name = method.function.rsplit('.').next().unwrap_or_default();
                    let value = self.method(receiver, name, args, into, kinds[0])?;
                    return Ok(vec![value]);
                }
                let (function, args) = method.resolve(&ty, &trait_args).ok_or_else(|| {
                    format!(
                        "'{ty}' does not implement the trait of '{}'",
                        method.function
                    )
                })?;
                let label = match args.is_empty() {
                    true => function.clone(),
                    false => self.request(Instance {
                        function: function.clone(),
                        args,
                    }),
                };
                self.dispatched = Some((call.span, label));
                function
            }
            None => method.function.clone(),
        };
        let function = function.as_str();
        if !method.receiver {
            return self.user_call(function, call.span, args, into, &kinds);
        }
        if !method.changes {
            let mut all = Vec::with_capacity(args.len() + 1);
            all.push(receiver.clone());
            all.extend(args.iter().cloned());
            // the prelude's `clamp` gives first whether its bounds are in order
            if function == PRELUDE_CLAMP {
                let mut counted = vec![None];
                counted.extend(&kinds);
                let values = self.user_call(function, call.span, &all, None, &counted)?;
                self.fail_unless(Cond::Ne, values[0], Operand::Imm(0), CLAMP_RANGE);
                return Ok(values[1..].to_vec());
            }
            return self.user_call(function, call.span, &all, into, &kinds);
        }
        self.changing_call((function, call.span), receiver, args, into, &kinds)
    }

    /// Calls `function`, a method that changes `self`, on `receiver`, a variable or a place in
    /// one, returning one value for each of `kinds`, how the result's values are counted.
    ///
    /// As for `append`, the receiver's indexes are evaluated, then the arguments, then every list
    /// on the way to the receiver is made unique. The receiver's values then move into the call,
    /// which returns them changed after its result, and are stored back where they came from.
    /// A global the method may read keeps its values while it runs, so they are retained
    /// instead, and the old ones released once the new ones are stored.
    fn changing_call(
        &mut self,
        (function, call): (&str, Span),
        receiver: &Expr,
        args: &[Expr],
        into: Option<VReg>,
        kinds: &[Option<RcKind>],
    ) -> Result<Vec<Operand>, String> {
        let Some((root, _)) = place(receiver) else {
            return Err("only a value in a variable can be changed".to_string());
        };
        // the receiver moves in, so the function must own it, as one that assigns it does
        if self
            .context
            .owned_params
            .get(function)
            .is_some_and(|owned| owned.first() != Some(&true))
        {
            return Err(format!(
                "'{function}' changes its receiver, so it must own it"
            ));
        }
        // taken first, so that no call among the arguments moves what a later one reads, and
        // never the receiver, which moves into the call apart
        let mut movable = std::mem::take(&mut self.movable);
        movable.retain(|name| variable_of(name) != root);
        let (label, bound) = self.specialized_label(function, call, args, 1);
        let indexes = self.place_indexes(receiver)?;
        let mut values = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            match bound.contains(&i) {
                true => values.push(vec![]),
                false => values.push(self.expr_flat(arg)?),
            }
        }
        let given = self.give_references(function, args, &values, &movable, 1)?;
        let location = self.locate(receiver, &indexes)?;
        let this = self.location_values(&location);
        let this_kinds = self.leaf_kinds(receiver)?;
        let keep = self.name.is_none() && self.context.global_reads[function].contains(root);
        if keep {
            for (&value, kind) in this.iter().zip(&this_kinds) {
                if kind.is_some() {
                    self.push(Inst::Retain { src: value });
                }
            }
        }
        let mut dsts = vec![self.dst(into)];
        dsts.extend((1..kinds.len() + this.len()).map(|_| self.fresh()));
        let mut call_args = this.clone();
        call_args.extend(values.iter().flatten());
        self.push(Inst::Call {
            dsts: dsts.clone(),
            callee: Callee::User(label),
            args: call_args,
        });
        for (value, given) in values
            .into_iter()
            .flatten()
            .zip(given.into_iter().flatten())
        {
            if !given {
                self.release_temp(value);
            }
        }
        let (results, changed) = dsts.split_at(kinds.len());
        let changed: Vec<Operand> = changed.iter().map(|&reg| Operand::Reg(reg)).collect();
        // moved, the old values are the call's, so only kept ones are released
        let stored_kinds = match keep {
            true => this_kinds,
            false => vec![None; this.len()],
        };
        self.store_location(location, &changed, &stored_kinds);
        let results: Vec<Operand> = results.iter().map(|&reg| Operand::Reg(reg)).collect();
        for (&result, &kind) in results.iter().zip(kinds) {
            self.own(result, kind);
        }
        Ok(results)
    }

    /// Makes a function value of the stone function `name`, of the type at `span`: the address
    /// of its copy `copy`, or of a function that gives each parameter it owns a reference first,
    /// since a call through a value borrows them all.
    ///
    /// For example, `double` is `v1 = fn_addr fn.double`.
    fn function_value(
        &mut self,
        name: &str,
        copy: &str,
        span: Span,
        into: Option<VReg>,
    ) -> Operand {
        let copy = copy.to_string();
        let owned = self
            .context
            .owned_params
            .get(name)
            .cloned()
            .unwrap_or_default();
        let label = match (owned.contains(&true), self.span_type(span)) {
            (true, Some(Type::Function { params, ret })) => self.helper(Helper::FunctionValue {
                function: copy,
                owned,
                params,
                ret: *ret,
            }),
            _ => function_label(&copy),
        };
        let dst = self.dst(into);
        self.push(Inst::FunctionAddress { dst, label });
        Operand::Reg(dst)
    }

    /// Makes the function [`Helper::FunctionValue`] describes, which calls `function` with its
    /// own arguments, retaining the counted values of each parameter `owned` says the function
    /// owns, and returns what it returns.
    fn function_value_thunk(
        mut self,
        function: &str,
        owned: &[bool],
        params: &[Type],
        ret: &Type,
    ) -> Result<Function, String> {
        let entry = self.new_block();
        self.start(entry);
        let mut args = Vec::new();
        for (param, &owns) in params.iter().zip(owned) {
            for leaf in self.leaves(param) {
                let reg = self.fresh();
                if owns && RcKind::of(&leaf).is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(reg),
                    });
                }
                args.push(reg);
            }
        }
        let dsts: Vec<VReg> = self.leaves(ret).iter().map(|_| self.fresh()).collect();
        self.push(Inst::Call {
            dsts: dsts.clone(),
            callee: Callee::User(function.to_string()),
            args: args.iter().map(|&reg| Operand::Reg(reg)).collect(),
        });
        self.terminate(Terminator::Return(
            dsts.into_iter().map(Operand::Reg).collect(),
        ));
        Ok(Function {
            name: self.name.clone(),
            params: args,
            blocks: self.layout(),
            vreg_count: self.vreg_count,
            helper: true,
        })
    }

    /// Calls the function value `func` evaluates to with `args`, returning one value for each of
    /// `kinds` as [`Lowerer::user_call`] does. The value is evaluated first, then the arguments,
    /// which the call borrows, since which function it calls is known only while it runs.
    fn value_call(
        &mut self,
        func: &Expr,
        call: Span,
        args: &[Expr],
        into: Option<VReg>,
        kinds: &[Option<RcKind>],
    ) -> Result<Vec<Operand>, String> {
        // a parameter this copy binds is called directly
        if let ExprKind::Name { id, .. } = &func.kind
            && let Some(bound) = self.constants.get(id).cloned()
        {
            self.dispatched = Some((call, bound.label));
            return self.user_call(&bound.base, call, args, into, kinds);
        }
        // nothing moves into a call that borrows, nor into a call among the arguments
        self.movable.clear();
        let function = self.expr(func)?;
        let mut values = Vec::new();
        for arg in args {
            values.push(self.expr_flat(arg)?);
        }
        let mut dsts = vec![self.dst(into)];
        dsts.extend((1..kinds.len()).map(|_| self.fresh()));
        self.push(Inst::Call {
            dsts: dsts.clone(),
            callee: Callee::Value(function),
            args: values.concat(),
        });
        for value in values.into_iter().flatten() {
            self.release_temp(value);
        }
        let results: Vec<Operand> = dsts.into_iter().map(Operand::Reg).collect();
        for (&result, &kind) in results.iter().zip(kinds) {
            self.own(result, kind);
        }
        Ok(results)
    }

    /// Calls the stone function `name`, which returns one value for each of `kinds`, how each is
    /// counted: several for a tuple, the first in `into` if given. The call borrows its arguments,
    /// each tuple as its values, and temporaries among them are released after it.
    /// Lowers the conversion `call`, of `arg`, to its values: a builtin conversion, or a call
    /// of the function of an impl of `from` or `try_from`, which this copy's types pick for a
    /// type parameter. `kind` says how a value a builtin conversion gives is counted.
    fn conversion(
        &mut self,
        call: &Expr,
        conversion: &Conversion,
        arg: &Expr,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Vec<Operand>, String> {
        let target = self.fill(&conversion.target);
        let source = self.fill(&conversion.source);
        let converter = conversion
            .resolve(&target, &source)
            .ok_or_else(|| format!("cannot convert {source} to {target}"))?;
        let Converter::Function(function, copy) = converter else {
            if target == source {
                // every type converts from itself, whatever its values
                return match target.is_flat() {
                    true => self.expr_flat(arg),
                    false => Ok(vec![self.expr(arg).map(|value| self.finish(value, into))?]),
                };
            }
            let kind = kind.or(RcKind::of(&self.type_of(call)?));
            return Ok(vec![self.builtin_conversion(
                (&target, &source),
                arg,
                into,
                kind,
            )?]);
        };
        // through a bound, the copy's target type picks its impl's function
        if conversion.function.is_none() {
            let label = match copy.is_empty() {
                true => function.clone(),
                false => self.request(Instance {
                    function: function.clone(),
                    args: copy,
                }),
            };
            self.dispatched = Some((call.span, label));
        }
        let kinds = self.leaf_kinds(call)?;
        self.user_call(
            &function,
            call.span,
            std::slice::from_ref(arg),
            into,
            &kinds,
        )
    }

    /// Lowers a builtin conversion of `arg`, a `source`, to a `target`, other than of a type to
    /// itself, its value counted as `kind`: a `try_from` gives a result.
    fn builtin_conversion(
        &mut self,
        (target, source): (&Type, &Type),
        arg: &Expr,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let runtime = match (target, source) {
            (Type::Str, Type::Int) => "stone.str_int",
            (Type::Str, Type::Float) => "stone.str_float",
            (Type::Str, Type::Bool) => "stone.str_bool",
            (Type::Str, _) => return self.display(arg, into, kind),
            (Type::Int, Type::Str) => "stone.parse_int_result",
            (Type::Float, Type::Str) => "stone.parse_float_result",
            (Type::Float, Type::Int) => {
                let src = self.expr(arg)?;
                let dst = self.dst(into);
                self.push(Inst::IntToFloat { dst, src });
                return Ok(Operand::Reg(dst));
            }
            (Type::Int, Type::Float) => return self.float_to_int_result(arg, into),
            (Type::Bytes, _) => return self.bytes_result(arg, into),
            _ => return Err(format!("cannot convert {source} to {target}")),
        };
        let src = self.expr(arg)?;
        let result = self.runtime_call(runtime, vec![src], into);
        self.release_temp(src);
        self.own(result, kind);
        Ok(result)
    }

    /// Lowers `int(x)` of a float `arg`: `ok` of the int toward zero, or `err` if it is nan or
    /// out of range, which fails both of the range's comparisons.
    fn float_to_int_result(&mut self, arg: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        let x = self.expr(arg)?;
        let (ok, err) = (self.tag_of(RESULT, "ok")?, self.tag_of(RESULT, "err")?);
        let dst = self.dst(into);
        let (above, fits, fails, end) = (
            self.new_block(),
            self.new_block(),
            self.new_block(),
            self.new_block(),
        );
        // -2^63 fits in an int but 2^63 does not
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Ge,
            float: true,
            lhs: x,
            rhs: Operand::Imm((i64::MIN as f64).to_bits() as i64),
            then: above,
            otherwise: fails,
        });
        self.start(above);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: true,
            lhs: x,
            rhs: Operand::Imm((-(i64::MIN as f64)).to_bits() as i64),
            then: fits,
            otherwise: fails,
        });
        self.start(fits);
        let whole = self.fresh();
        self.push(Inst::FloatToInt { dst: whole, src: x });
        self.new_variant(dst, ok, vec![Operand::Reg(whole)], &[None]);
        self.jump(end);
        self.start(fails);
        let message = self.literal(math::FLOAT_TO_INT);
        self.new_variant(dst, err, vec![message], &[Some(RcKind::Str)]);
        self.jump(end);
        self.start(end);
        self.own(Operand::Reg(dst), Some(RcKind::Enum));
        Ok(Operand::Reg(dst))
    }

    /// Lowers `bytes(xs)` of a list `arg`: `ok` of the bytes, or `err` naming the first value
    /// that is not a byte, as `byte out of range (value 300)`.
    fn bytes_result(&mut self, arg: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        let values = self.expr(arg)?;
        // the routine gives where the first value that is not a byte is, or -1
        let bad = self.runtime_call("stone.bytes_check", vec![values], None);
        let (ok, err) = (self.tag_of(RESULT, "ok")?, self.tag_of(RESULT, "err")?);
        let dst = self.dst(into);
        let (holds, fails, end) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: bad,
            rhs: Operand::Imm(0),
            then: holds,
            otherwise: fails,
        });
        self.start(holds);
        let bytes = self.runtime_call("stone.bytes_from_list", vec![values], None);
        self.new_variant(dst, ok, vec![bytes], &[Some(RcKind::Str)]);
        self.jump(end);
        self.start(fails);
        let value = self.fresh();
        self.push(Inst::ListGet {
            dst: value,
            list: values,
            index: bad,
            offset: 0,
        });
        // the message's pieces around the value, as `stdlib::fill` writes it
        let (before, after) = BYTE_RANGE.split_once("{}").ok_or("a message with a hole")?;
        let number = self.runtime_call("stone.str_int", vec![Operand::Reg(value)], None);
        let start = self.literal_borrowed(before);
        let text = self.runtime_call("stone.str_concat", vec![start, number], None);
        self.push(Inst::Release {
            src: number,
            kind: RcKind::Str,
        });
        let end_text = self.literal_borrowed(after);
        let message = self.runtime_call("stone.str_concat", vec![text, end_text], None);
        self.push(Inst::Release {
            src: text,
            kind: RcKind::Str,
        });
        self.new_variant(dst, err, vec![message], &[Some(RcKind::Str)]);
        self.jump(end);
        self.start(end);
        self.release_temp(values);
        self.own(Operand::Reg(dst), Some(RcKind::Enum));
        Ok(Operand::Reg(dst))
    }

    /// Returns the immortal string `text`, retained, so whatever stores it owns a reference.
    fn literal(&mut self, text: &str) -> Operand {
        let value = self.literal_borrowed(text);
        self.push(Inst::Retain { src: value });
        value
    }

    /// Returns the immortal string `text`, borrowed.
    fn literal_borrowed(&mut self, text: &str) -> Operand {
        let dst = self.fresh();
        self.push(Inst::StrAddr {
            dst,
            text: text.to_string(),
        });
        Operand::Reg(dst)
    }

    fn user_call(
        &mut self,
        name: &str,
        call: Span,
        args: &[Expr],
        into: Option<VReg>,
        kinds: &[Option<RcKind>],
    ) -> Result<Vec<Operand>, String> {
        // taken first, so that no call among the arguments moves what a later one reads
        let movable = std::mem::take(&mut self.movable);
        let (label, bound) = self.specialized_label(name, call, args, 0);
        let mut values = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            // the copy holds a function it is given, so it is no argument
            match bound.contains(&i) {
                true => values.push(vec![]),
                false => values.push(self.expr_flat(arg)?),
            }
        }
        let given = self.give_references(name, args, &values, &movable, 0)?;
        let mut dsts = vec![self.dst(into)];
        dsts.extend((1..kinds.len()).map(|_| self.fresh()));
        self.push(Inst::Call {
            dsts: dsts.clone(),
            callee: Callee::User(label),
            args: values.concat(),
        });
        for (value, given) in values
            .into_iter()
            .flatten()
            .zip(given.into_iter().flatten())
        {
            if !given {
                self.release_temp(value);
            }
        }
        let results: Vec<Operand> = dsts.into_iter().map(Operand::Reg).collect();
        for (&result, &kind) in results.iter().zip(kinds) {
            self.own(result, kind);
        }
        Ok(results)
    }

    /// Gives a reference to each argument of the stone function `name` that it owns, once every
    /// argument is evaluated, and returns which arguments got one. The first argument is
    /// parameter `first`, which is 1 when the receiver of a method that changes it is given apart.
    ///
    /// A temporary gives its own. A variable in `movable` gives its own too, which moves its
    /// value into the call, unless it is a global the function may read, or it was already given
    /// to another parameter of the same call. Anything else is retained. For example, in
    /// `xs = add(xs, 1)` the list moves into `add`, which can then append to it in place, while
    /// `ys = add(xs, 1)` retains it, so `add` copies it before appending.
    fn give_references(
        &mut self,
        name: &str,
        args: &[Expr],
        values: &[Vec<Operand>],
        movable: &[String],
        first: usize,
    ) -> Result<Vec<Vec<bool>>, String> {
        let owned = self.context.owned_params.get(name);
        let mut given = Vec::new();
        for (i, (arg, leaves)) in args.iter().zip(values).enumerate() {
            let mut leaves_given = vec![false; leaves.len()];
            let owns = owned.is_some_and(|owned| owned.get(first + i) == Some(&true));
            let kinds = self.leaf_kinds(arg)?;
            let sources = self.flat_sources(arg);
            for (j, &value) in leaves.iter().enumerate() {
                if !owns || kinds[j].is_none() {
                    continue;
                }
                leaves_given[j] = true;
                if let Operand::Reg(reg) = value
                    && self.owned.remove(&reg).is_some()
                {
                    continue;
                }
                // a global the function may read must keep its own copy while it runs
                if let Some(id) = &sources[j]
                    && movable.contains(id)
                    && (self.locals.contains_key(id)
                        || !self.context.global_reads[name].contains(variable_of(id)))
                    && self.mark_moved(id)
                {
                    continue;
                }
                self.push(Inst::Retain { src: value });
            }
            given.push(leaves_given);
        }
        Ok(given)
    }

    /// Returns the counted variables this function owns, or the counted globals in `main`, that
    /// nothing reads after `stmt`, so that `stmt` may move them instead of copying them.
    ///
    /// For example, in `ys = add(xs, 1)` followed by `print(ys)`, this is `xs`.
    fn last_uses(&self, stmt: &Stmt) -> Vec<String> {
        let live = self.live_after.get(&stmt.span);
        let mut names: Vec<String> = match &self.name {
            Some(_) => self
                .locals
                .keys()
                .filter(|name| self.is_counted_variable(name))
                .cloned()
                .collect(),
            None => self
                .context
                .globals
                .iter()
                .filter(|name| self.global_kind(name).is_some())
                .cloned()
                .collect(),
        };
        // liveness knows the stone variables, of which a tuple's values are part
        names.retain(|name| live.is_none_or(|live| !live.contains(variable_of(name))));
        names.sort();
        names
    }

    /// Returns whether `name` is a variable this code owns a counted reference in: a counted
    /// local of this function, or a counted global in `main`.
    fn is_counted_variable(&self, name: &str) -> bool {
        match self.locals.get(name) {
            Some(reg) => self.counted_locals.iter().any(|(local, _)| local == reg),
            None => self.name.is_none() && self.global_kind(name).is_some(),
        }
    }

    /// Notes that the variable `name` gave its reference away, returning false if it already
    /// had.
    fn mark_moved(&mut self, name: &str) -> bool {
        match self.locals.get(name) {
            Some(&local) => self.moved_locals.insert(local),
            None => self.moved_globals.insert(name.to_string()),
        }
    }

    /// Like [`Lowerer::take`], but a variable in `last_uses` gives its own reference instead of
    /// being retained, so that `ys = xs` at the last use of `xs` leaves one reference, not two.
    fn take_from(
        &mut self,
        expr: &Expr,
        value: Operand,
        kind: Option<RcKind>,
        last_uses: &[String],
    ) {
        if kind.is_some()
            && let ExprKind::Name { id, .. } = &expr.kind
            && last_uses.contains(id)
            && self.mark_moved(id)
        {
            return;
        }
        self.take(value, kind);
    }

    /// Like [`Lowerer::take_from`] for one value of a tuple, which came from the variable
    /// `source`, if any, such as `t.0` or `xs` in `(xs, 1)`.
    fn take_leaf(
        &mut self,
        source: Option<&str>,
        value: Operand,
        kind: Option<RcKind>,
        last_uses: &[String],
    ) {
        if kind.is_some()
            && let Some(name) = source
            && last_uses.iter().any(|used| used == name)
            && self.mark_moved(name)
        {
            return;
        }
        self.take(value, kind);
    }

    /// Returns, for each value [`Lowerer::expr_flat`] gives for `expr`, the variable it is read
    /// from, if it is one.
    ///
    /// For example, `(xs, f(), t)` with `t` a pair gives `xs`, nothing, `t.0`, and `t.1`.
    fn flat_sources(&self, expr: &Expr) -> Vec<Option<String>> {
        let unknown = || vec![None; self.type_of(expr).map_or(1, |ty| self.leaves(&ty).len())];
        match &expr.kind {
            ExprKind::Name { id, .. } => self.leaf_names(id).into_iter().map(Some).collect(),
            ExprKind::Tuple { elts, .. } => {
                elts.iter().flat_map(|elt| self.flat_sources(elt)).collect()
            }
            // a field of a variable, such as `p.items`, is some of the variable's values
            ExprKind::Attribute { value, attr, .. } => {
                let field = self
                    .type_of(value)
                    .ok()
                    .and_then(|ty| ty.struct_field(attr, self.context.structs));
                let sources = self.flat_sources(value);
                match field {
                    Some((range, _)) if range.end <= sources.len() => sources[range].to_vec(),
                    _ => unknown(),
                }
            }
            // a struct's values, in the order of its fields
            ExprKind::Call { func, keywords, .. } => match &func.kind {
                ExprKind::Name { id, .. }
                    if self.context.structs.contains_key(id) && may_name_struct(func) =>
                {
                    self.context.structs[id]
                        .fields
                        .iter()
                        .flat_map(|(field, _)| {
                            keywords
                                .iter()
                                .find(|k| k.arg == *field)
                                .map(|k| self.flat_sources(&k.value))
                                .unwrap_or_default()
                        })
                        .collect()
                }
                _ => unknown(),
            },
            _ => unknown(),
        }
    }

    /// Returns whether `expr` is a tuple or a struct, which lowers to several values.
    fn is_flat(&self, expr: &Expr) -> bool {
        self.span_type(expr.span).is_some_and(|ty| ty.is_flat())
    }

    /// Lowers `expr` to its values: one for each value of a tuple, nested tuples flattened, or
    /// its one value. Each is borrowed, like a variable's, or a temporary that owns a reference,
    /// like a call's result.
    ///
    /// For example, `(x, ("a", f()))` gives `x`'s vreg, the string's address, and the call's
    /// result.
    fn expr_flat(&mut self, expr: &Expr) -> Result<Vec<Operand>, String> {
        if !self.is_flat(expr) {
            return Ok(vec![self.expr(expr)?]);
        }
        let outer = std::mem::replace(&mut self.span, expr.span);
        let result = self.tuple_values(expr);
        self.span = outer;
        result
    }

    fn tuple_values(&mut self, expr: &Expr) -> Result<Vec<Operand>, String> {
        match &expr.kind {
            ExprKind::Try { value } => self.try_values(value),
            ExprKind::Tuple { elts, .. } => {
                let mut values = Vec::new();
                for elt in elts {
                    values.extend(self.expr_flat(elt)?);
                }
                Ok(values)
            }
            ExprKind::Name { id, .. } => {
                let mut values = Vec::new();
                for (i, leaf) in self.leaf_names(id).iter().enumerate() {
                    if let Some(&local) = self.locals.get(leaf) {
                        values.push(Operand::Reg(local));
                        continue;
                    }
                    if let Some(&literal) = self.context.constant_globals.get(leaf) {
                        values.push(Operand::Imm(literal));
                        continue;
                    }
                    // the values are assigned together, so checking the first checks them all
                    let dst = self.fresh();
                    self.push(Inst::LoadGlobal {
                        dst,
                        name: leaf.clone(),
                        checked: self.name.is_some() && i == 0,
                    });
                    values.push(Operand::Reg(dst));
                }
                Ok(values)
            }
            ExprKind::Subscript { value, slice, .. } if self.is_flat(value) => {
                self.tuple_field(value, slice)
            }
            ExprKind::Subscript { value, .. } if self.map_key(value).is_some() => {
                let width = self.element_layout(value).0;
                self.map_values(expr, 0..width - 1)
            }
            ExprKind::Subscript { value, slice, .. } => {
                let width = self.element_layout(value).0;
                self.element_values(value, slice, 0..width)
            }
            ExprKind::Attribute { value, attr, .. } => self.struct_field(value, attr),
            ExprKind::Call {
                func,
                args,
                keywords,
            } => match &func.kind {
                _ if let Some(conversion) = self.context.conversions.get(&expr.span) => {
                    self.conversion(expr, conversion, &args[0], None, None)
                }
                _ if self.context.indirect_calls.contains(&expr.span) => {
                    let kinds = self.leaf_kinds(expr)?;
                    self.value_call(func, expr.span, args, None, &kinds)
                }
                ExprKind::Name { id, .. }
                    if self.context.structs.contains_key(id) && may_name_struct(func) =>
                {
                    self.construct(id, keywords)
                }
                ExprKind::Name { id, .. } if id == "random.choice" => self.choose_element(&args[0]),
                ExprKind::Name { id, .. } => {
                    let kinds = self.leaf_kinds(expr)?;
                    self.user_call(id, expr.span, args, None, &kinds)
                }
                ExprKind::Attribute { value, .. }
                    if self.context.methods.contains_key(&expr.span) =>
                {
                    let method = &self.context.methods[&expr.span];
                    self.struct_call(expr, method, value, args, None)
                }
                ExprKind::Attribute { value, attr, .. } if attr == "pop" => {
                    self.pop_element(value, args)
                }
                ExprKind::Attribute { value, attr, .. }
                    if matches!(attr.as_str(), "unwrap" | "unwrap_or" | "unwrap_err") =>
                {
                    self.option_value(value, attr, args.first())
                }
                _ => Err("this method does not return a tuple".to_string()),
            },
            _ => Err("this expression cannot be a tuple".to_string()),
        }
    }

    /// Lowers `value[slice]` for a tuple `value`, whose index the checker made sure is a constant
    /// in range, to the field's values, releasing the temporaries of the other fields.
    ///
    /// For example, `t[1]` of a `tuple[int, tuple[str, float]]` gives `t.1` and `t.2`.
    fn tuple_field(&mut self, value: &Expr, slice: &Expr) -> Result<Vec<Operand>, String> {
        let ty = self.type_of(value)?;
        let Type::Tuple(fields) = &ty else {
            return Err("only a tuple has fields".to_string());
        };
        let index = constant_int(slice).ok_or("a tuple index is a constant")?;
        let index = if index < 0 {
            index + fields.len() as i64
        } else {
            index
        };
        let leaves = ty.field_leaves(index as usize, self.context.structs);
        // a field of a list's element is read on its own, as in `pairs[i][0]`
        if let ExprKind::Subscript {
            value: list,
            slice: position,
            ..
        } = &value.kind
            && matches!(self.span_type(list.span), Some(Type::List(_)))
        {
            let outer = std::mem::replace(&mut self.span, value.span);
            let picked = self.element_values(list, position, leaves);
            self.span = outer;
            return picked;
        }
        let mut picked = Vec::new();
        for (i, leaf) in self.expr_flat(value)?.into_iter().enumerate() {
            if leaves.contains(&i) {
                picked.push(leaf);
            } else {
                self.release_temp(leaf);
            }
        }
        Ok(picked)
    }

    /// Lowers `Name(field=value, ...)`, a value of the struct `name`, to its values in the order of
    /// its fields, evaluating the values in the order they are written. Each is borrowed or a
    /// temporary, as a tuple's values are.
    ///
    /// For example, `Point(y=f(), x=1.0)` calls `f` and gives `1.0`, then the call's result.
    fn construct(&mut self, name: &str, keywords: &[Keyword]) -> Result<Vec<Operand>, String> {
        let fields = &self.context.structs[name].fields;
        let mut given: Vec<Option<Vec<Operand>>> = vec![None; fields.len()];
        for keyword in keywords {
            let values = self.expr_flat(&keyword.value)?;
            let position = self.context.structs[name]
                .fields
                .iter()
                .position(|(field, _)| *field == keyword.arg)
                .ok_or_else(|| format!("'{name}' has no field '{}'", keyword.arg))?;
            given[position] = Some(values);
        }
        let mut values = Vec::new();
        for (field, (name, _)) in given.into_iter().zip(&self.context.structs[name].fields) {
            values.extend(field.ok_or_else(|| format!("field '{name}' has no value"))?);
        }
        Ok(values)
    }

    /// Lowers `value.attr`, a field of the struct `value`, to the field's values, releasing the
    /// temporaries of the other fields.
    ///
    /// For example, `p.y` of a local `Point` is `p.1`'s vreg, and `ps[i].y` reads only that slot
    /// of the element.
    fn struct_field(&mut self, value: &Expr, attr: &str) -> Result<Vec<Operand>, String> {
        let ty = self.type_of(value)?;
        let (leaves, _) = ty
            .struct_field(attr, self.context.structs)
            .ok_or_else(|| format!("{ty} has no field '{attr}'"))?;
        // a field of a list's element is read on its own, as in `ps[i].y`
        if let ExprKind::Subscript {
            value: list,
            slice: position,
            ..
        } = &value.kind
            && self.holds_flat(list)
        {
            let outer = std::mem::replace(&mut self.span, value.span);
            let picked = self.element_values(list, position, leaves);
            self.span = outer;
            return picked;
        }
        let mut picked = Vec::new();
        for (i, leaf) in self.expr_flat(value)?.into_iter().enumerate() {
            if leaves.contains(&i) {
                picked.push(leaf);
            } else {
                self.release_temp(leaf);
            }
        }
        Ok(picked)
    }

    /// Lowers `receiver.append(value)` or `receiver.insert(index, value)` for a list of tuples,
    /// which adds the tuple's values one slot at a time: appended in order, or inserted at the
    /// slot where the element starts, last first.
    fn add_element(&mut self, receiver: &Expr, name: &str, args: &[Expr]) -> Result<(), String> {
        let width = self.element_layout(receiver).0;
        let kinds = self.element_kinds(receiver);
        // the receiver's indexes, then the arguments, then the change
        let indexes = self.place_indexes(receiver)?;
        let (index, item) = match args {
            [item] => (None, item),
            [index, item] => (Some(self.expr(index)?), item),
            _ => return Err(format!("{name}() takes a value")),
        };
        let sources = self.flat_sources(item);
        let values = self.expr_flat(item)?;
        // the list keeps a reference to what it holds
        let last_uses = std::mem::take(&mut self.statement_last_uses);
        for ((&value, &kind), source) in values.iter().zip(&kinds).zip(&sources) {
            self.take_leaf(source.as_deref(), value, kind, &last_uses);
        }
        self.statement_last_uses = last_uses;
        let list = self.unique_place(receiver, &indexes)?;
        let Some(index) = index else {
            for value in values {
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.list_append"),
                    args: vec![list, value],
                });
            }
            return Ok(());
        };
        let count = self.element_count(list, width);
        let position = self.list_position(index, count, true, INDEX_RANGE);
        let first = self.first_slot(position, width);
        for value in values.into_iter().rev() {
            self.push(Inst::Call {
                dsts: vec![],
                callee: Callee::Runtime("stone.list_insert"),
                args: vec![list, first, value],
            });
        }
        Ok(())
    }

    /// Returns how many elements the list `list` holds, whose elements are `width` slots each.
    fn element_count(&mut self, list: Operand, width: usize) -> Operand {
        let slots = self.fresh();
        self.push(Inst::ListLen { dst: slots, list });
        if width == 1 {
            return Operand::Reg(slots);
        }
        let count = self.fresh();
        self.push(Inst::Binary {
            op: BinOp::Div,
            dst: count,
            lhs: Operand::Reg(slots),
            rhs: Operand::Imm(width as i64),
        });
        Operand::Reg(count)
    }

    /// Returns the slot where element `position` of a list starts, whose elements are `width`
    /// slots each.
    fn first_slot(&mut self, position: Operand, width: usize) -> Operand {
        if width == 1 {
            return position;
        }
        let slot = self.fresh();
        self.push(Inst::Binary {
            op: BinOp::Mul,
            dst: slot,
            lhs: position,
            rhs: Operand::Imm(width as i64),
        });
        Operand::Reg(slot)
    }

    /// Returns the slot where element `index` of `list` starts, whose elements are `width` slots
    /// each, counting a negative index from the end and stopping the program with
    /// `stdlib::INDEX_RANGE` unless that is an element, through one [`Inst::ElementSlot`], which
    /// the optimizer can tell is the same check as an earlier one.
    fn element_slot(&mut self, list: Operand, index: Operand, width: usize) -> Operand {
        let dst = self.fresh();
        self.push(Inst::ElementSlot {
            dst,
            list,
            index,
            width,
        });
        Operand::Reg(dst)
    }

    /// Returns slot `field` of the element starting at slot `first`.
    fn slot(&mut self, first: Operand, field: usize) -> Operand {
        if field == 0 {
            return first;
        }
        let slot = self.fresh();
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: slot,
            lhs: first,
            rhs: Operand::Imm(field as i64),
        });
        Operand::Reg(slot)
    }

    /// Reads `leaves`, the values of element `position` of `list` that are wanted, whose
    /// elements are `width` slots and whose values are counted as `kinds` say. Each counted value
    /// is retained, and the caller owns or stores it.
    fn load_slots(
        &mut self,
        list: Operand,
        position: Operand,
        width: usize,
        leaves: std::ops::Range<usize>,
        kinds: &[Option<RcKind>],
    ) -> Vec<Operand> {
        let first = self.first_slot(position, width);
        self.load_slots_from(list, first, leaves, kinds)
    }

    /// Reads `leaves`, the values wanted of the element of `list` that starts at slot `first`,
    /// retaining each counted value as [`Lowerer::load_slots`] does.
    fn load_slots_from(
        &mut self,
        list: Operand,
        first: Operand,
        leaves: std::ops::Range<usize>,
        kinds: &[Option<RcKind>],
    ) -> Vec<Operand> {
        let mut values = Vec::new();
        for field in leaves {
            let index = self.slot(first, field);
            let dst = self.fresh();
            self.push(Inst::ListGet {
                dst,
                list,
                index,
                offset: 0,
            });
            if kinds[field].is_some() {
                self.push(Inst::Retain {
                    src: Operand::Reg(dst),
                });
            }
            values.push(Operand::Reg(dst));
        }
        values
    }

    /// Lowers `list[index]` for a list of tuples to the values of the element in `leaves`, each
    /// a temporary that owns a reference, as when a subscript reads one value. The index is
    /// checked against the number of elements, failing as `xs[i]` does.
    fn element_values(
        &mut self,
        list_expr: &Expr,
        index: &Expr,
        leaves: std::ops::Range<usize>,
    ) -> Result<Vec<Operand>, String> {
        let width = self.element_layout(list_expr).0;
        let kinds = self.element_kinds(list_expr);
        let list = self.list_operand(list_expr, &[index])?;
        let index = self.expr(index)?;
        let first = self.element_slot(list, index, width);
        let values = self.load_slots_from(list, first, leaves.clone(), &kinds);
        for (&value, field) in values.iter().zip(leaves) {
            self.own(value, kinds[field]);
        }
        self.release_temp(list);
        Ok(values)
    }

    /// Lowers `random.choice(list)` for a list of tuples to the values of the element it picks,
    /// drawing as for any other list.
    fn choose_element(&mut self, list_expr: &Expr) -> Result<Vec<Operand>, String> {
        let width = self.element_layout(list_expr).0;
        let kinds = self.element_kinds(list_expr);
        let list = self.list_operand(list_expr, &[])?;
        let count = self.element_count(list, width);
        self.fail_unless(Cond::Ne, count, Operand::Imm(0), random::EMPTY_CHOICE);
        let position = self.runtime_call("stone.random_below", vec![count], None);
        let values = self.load_slots(list, position, width, 0..width, &kinds);
        for (&value, &kind) in values.iter().zip(&kinds) {
            self.own(value, kind);
        }
        self.release_temp(list);
        Ok(values)
    }

    /// Lowers `receiver.pop(index)` for a list of tuples, which removes an element's values one
    /// at a time from the slot where it starts, each with the list's reference to it.
    fn pop_element(&mut self, receiver: &Expr, args: &[Expr]) -> Result<Vec<Operand>, String> {
        let width = self.element_layout(receiver).0;
        let kinds = self.element_kinds(receiver);
        // the receiver's indexes, then the index, then the change
        let indexes = self.place_indexes(receiver)?;
        let [index] = args else {
            return Err("only pop(index) gives the values of an element".to_string());
        };
        let index = self.expr(index)?;
        let list = self.unique_place(receiver, &indexes)?;
        let count = self.element_count(list, width);
        self.fail_unless(Cond::Ne, count, Operand::Imm(0), POP_EMPTY);
        let position = self.list_position(index, count, false, POP_RANGE);
        let values = self.remove_element(list, position, width, &kinds);
        for (&value, &kind) in values.iter().zip(&kinds) {
            self.own(value, kind);
        }
        Ok(values)
    }

    /// Lowers `receiver.pop()`, which is `some` of the last element, holding the list's
    /// references to its values, or `none` if the list is empty, into `into` if given.
    fn pop_last(&mut self, receiver: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        let width = self.element_layout(receiver).0;
        let kinds = self.element_kinds(receiver);
        let indexes = self.place_indexes(receiver)?;
        let list = self.unique_place(receiver, &indexes)?;
        let count = self.element_count(list, width);
        Ok(
            self.option_if((Cond::Ne, count, Operand::Imm(0)), into, |this| {
                let last = this.fresh();
                this.push(Inst::Binary {
                    op: BinOp::Sub,
                    dst: last,
                    lhs: count,
                    rhs: Operand::Imm(1),
                });
                let values = this.remove_element(list, Operand::Reg(last), width, &kinds);
                values.into_iter().zip(kinds.iter().copied()).collect()
            }),
        )
    }

    /// Removes element `position` of `list`, whose elements are `width` slots each, returning
    /// its values, each with the list's reference to it.
    fn remove_element(
        &mut self,
        list: Operand,
        position: Operand,
        width: usize,
        kinds: &[Option<RcKind>],
    ) -> Vec<Operand> {
        let first = self.first_slot(position, width);
        // each value removed moves the next one down to the same slot
        kinds
            .iter()
            .map(|_| self.runtime_call("stone.list_pop", vec![list, first], None))
            .collect()
    }

    /// Stores the values of a tuple, which already carry the references the list takes, in the
    /// element `target` of a list of tuples, evaluating its indexes first, then releasing the
    /// element's old values.
    fn store_values(
        &mut self,
        target: &Expr,
        values: &[Operand],
        kinds: &[Option<RcKind>],
    ) -> Result<(), String> {
        let ExprKind::Subscript {
            value: list_expr,
            slice,
            ..
        } = &target.kind
        else {
            return Err("only an element of a list can be changed".to_string());
        };
        let width = self.element_layout(list_expr).0;
        let indexes = self.place_indexes(list_expr)?;
        let index = self.expr(slice)?;
        let list = self.unique_place(list_expr, &indexes)?;
        let outer = std::mem::replace(&mut self.span, target.span);
        let count = self.element_count(list, width);
        let position = self.list_position(index, count, false, INDEX_RANGE);
        self.span = outer;
        let first = self.first_slot(position, width);
        let mut old = Vec::new();
        for (field, (&value, &kind)) in values.iter().zip(kinds).enumerate() {
            let index = self.slot(first, field);
            if let Some(kind) = kind {
                let dst = self.fresh();
                self.push(Inst::ListGet {
                    dst,
                    list,
                    index,
                    offset: 0,
                });
                old.push((dst, kind));
            }
            self.push(Inst::ListSet {
                list,
                index,
                value,
                offset: 0,
            });
        }
        for (src, kind) in old {
            self.push(Inst::Release {
                src: Operand::Reg(src),
                kind,
            });
        }
        Ok(())
    }

    /// Returns the first slot of the entry of `key`, of type `key_ty`, in `map`, or -1 if it has
    /// none, into `into` if given.
    fn map_find(
        &mut self,
        map: Operand,
        key: Operand,
        key_ty: &Type,
        into: Option<VReg>,
    ) -> Operand {
        let string = Operand::Imm((*key_ty == Type::Str) as i64);
        self.runtime_call("stone.map_find", vec![map, key, string], into)
    }

    /// Lowers `subscript`, `map[key]`, to the values in `leaves` of the value of `key`'s entry,
    /// each a temporary that owns a reference, as [`Lowerer::element_values`] does for a list.
    /// A missing key stops the program as the interpreter does, such as with
    /// `key not found: 'pear'`.
    fn map_values(
        &mut self,
        subscript: &Expr,
        leaves: std::ops::Range<usize>,
    ) -> Result<Vec<Operand>, String> {
        let ExprKind::Subscript {
            value: map_expr,
            slice,
            ..
        } = &subscript.kind
        else {
            return Err("only a subscript reads a map".to_string());
        };
        let key_ty = self.map_key(map_expr).ok_or("only a map has entries")?;
        let kinds = self.element_kinds(map_expr);
        let map = self.list_operand(map_expr, &[slice])?;
        let key = self.expr(slice)?;
        let outer = std::mem::replace(&mut self.span, subscript.span);
        let first = self.map_find(map, key, &key_ty, None);
        self.fail_missing_key(first, key, &key_ty);
        self.span = outer;
        self.release_temp(key);
        // the key is slot 0 of an entry
        let fields = leaves.start + 1..leaves.end + 1;
        let values = self.load_slots(map, first, 1, fields.clone(), &kinds);
        for (&value, field) in values.iter().zip(fields) {
            self.own(value, kinds[field]);
        }
        self.release_temp(map);
        Ok(values)
    }

    /// Stops the program unless `first`, from [`Lowerer::map_find`], is an entry's first slot,
    /// failing as the interpreter does for the missing `key` of type `key_ty`, such as with
    /// `key not found: 'pear'`, then goes on in a new block.
    fn fail_missing_key(&mut self, first: Operand, key: Operand, key_ty: &Type) {
        let (found, missing) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Ge,
            float: false,
            lhs: first,
            rhs: Operand::Imm(0),
            then: found,
            otherwise: missing,
        });
        self.start(missing);
        match key_ty {
            Type::Str => {
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.map_missing_str"),
                    args: vec![key],
                });
                // the routine stops the program
                self.terminate(Terminator::Fail("key not found".to_string(), vec![]));
            }
            Type::Bool => {
                let (falsy, truthy) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Eq,
                    float: false,
                    lhs: key,
                    rhs: Operand::Imm(0),
                    then: falsy,
                    otherwise: truthy,
                });
                self.start(falsy);
                self.terminate(Terminator::Fail("key not found: false".to_string(), vec![]));
                self.start(truthy);
                self.terminate(Terminator::Fail("key not found: true".to_string(), vec![]));
            }
            _ => self.terminate(Terminator::Fail("key not found: {}".to_string(), vec![key])),
        }
        self.start(found);
    }

    /// Lowers `receiver.pop(key)` for a map, which is `some` of the value of `key`'s entry,
    /// holding the map's references to its values, or `none` if it has none, into `into` if
    /// given. The receiver's indexes and keys are evaluated, then the key, then the entry is
    /// removed: `stone.map_take` marks it removed where it is, the values move out, and its
    /// counted slots are zeroed, so nothing releases them again, before `stone.map_settle` closes
    /// up the map if removed entries are more than half of it.
    fn map_pop(
        &mut self,
        receiver: &Expr,
        key_expr: &Expr,
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let key_ty = self.map_key(receiver).ok_or("only a map has entries")?;
        let kinds = self.element_kinds(receiver);
        let indexes = self.place_indexes(receiver)?;
        let key = self.expr(key_expr)?;
        let map = self.unique_place(receiver, &indexes)?;
        let string = Operand::Imm((key_ty == Type::Str) as i64);
        let first = self.runtime_call("stone.map_take", vec![map, key, string], None);
        self.release_temp(key);
        Ok(
            self.option_if((Cond::Ge, first, Operand::Imm(0)), into, |this| {
                let mut removed = Vec::new();
                for (field, kind) in kinds.iter().enumerate() {
                    let index = this.slot(first, field);
                    let dst = this.fresh();
                    this.push(Inst::ListGet {
                        dst,
                        list: map,
                        index,
                        offset: 0,
                    });
                    if kind.is_some() {
                        this.push(Inst::ListSet {
                            list: map,
                            index,
                            value: Operand::Imm(0),
                            offset: 0,
                        });
                    }
                    removed.push(Operand::Reg(dst));
                }
                // the entry's key goes with it
                if let Some(kind) = kinds[0] {
                    this.push(Inst::Release {
                        src: removed[0],
                        kind,
                    });
                }
                this.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Runtime("stone.map_settle"),
                    args: vec![map],
                });
                removed[1..]
                    .iter()
                    .copied()
                    .zip(kinds[1..].iter().copied())
                    .collect()
            }),
        )
    }

    /// Closes up the entries of `map` if the program removes any from maps, before code reads them
    /// as a list.
    fn close_up(&mut self, map: Operand) {
        if self.context.shrinks_maps {
            self.push(Inst::Call {
                dsts: vec![],
                callee: Callee::Runtime("stone.map_compact"),
                args: vec![map],
            });
        }
    }

    /// Lowers `receiver.keys()` or `receiver.values()` for a map to a new list holding, for each
    /// entry in order, the values in `leaves` of it, retained, into `into` if given: `0..1` is
    /// the key, and the rest are the value's.
    fn map_column(
        &mut self,
        receiver: &Expr,
        leaves: std::ops::Range<usize>,
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        let kinds = self.element_kinds(receiver);
        let width = kinds.len();
        let taken = &kinds[leaves.clone()];
        let map = self.list_operand(receiver, &[])?;
        self.close_up(map);
        let count = self.element_count(map, width);
        let slots = self.first_slot(count, taken.len());
        let list = self.fresh();
        let codes = Operand::Imm(RcKind::codes(taken));
        let (label, args) = match taken.len() {
            1 => ("stone.list_new", vec![slots, codes]),
            n => (
                "stone.list_new_wide",
                vec![slots, codes, Operand::Imm(n as i64)],
            ),
        };
        self.push(Inst::Call {
            dsts: vec![list],
            callee: Callee::Runtime(label),
            args,
        });
        let next = self.fresh();
        self.push(Inst::Copy {
            dst: next,
            src: Operand::Imm(0),
        });
        let (header, body, exit) = (self.new_block(), self.new_block(), self.new_block());
        self.loop_depth += 1;
        self.jump(header);
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(next),
            rhs: count,
            then: body,
            otherwise: exit,
        });
        self.start(body);
        // the new list takes a reference to each counted value
        let values = self.load_slots(map, Operand::Reg(next), width, leaves, &kinds);
        let first = self.first_slot(Operand::Reg(next), taken.len());
        for (field, value) in values.into_iter().enumerate() {
            let index = self.slot(first, field);
            self.push(Inst::ListSet {
                list: Operand::Reg(list),
                index,
                value,
                offset: 0,
            });
        }
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        self.release_temp(map);
        self.own(Operand::Reg(list), Some(RcKind::List));
        Ok(self.finish(Operand::Reg(list), into))
    }

    /// Stores `values`, which already carry the references the map takes, as the value of the
    /// entry `target`, `map[key]`, evaluating the key first. An entry the map has keeps its place
    /// and releases its old values, and a new one goes at the end.
    fn map_store(&mut self, target: &Expr, values: &[Operand]) -> Result<(), String> {
        let ExprKind::Subscript {
            value: map_expr,
            slice,
            ..
        } = &target.kind
        else {
            return Err("only a subscript changes a map".to_string());
        };
        let key_ty = self.map_key(map_expr).ok_or("only a map has entries")?;
        let kinds = self.element_kinds(map_expr);
        // the value was evaluated first, then the indexes and keys on the way, then the key
        let indexes = self.place_indexes(map_expr)?;
        let key = self.expr(slice)?;
        let map = self.unique_place(map_expr, &indexes)?;
        self.map_put(map, key, &key_ty, values, &kinds);
        self.release_temp(key);
        Ok(())
    }

    /// Sets the value of `key`'s entry in `map`, which nothing else refers to, to `values`,
    /// which carry the references the map takes. The entries are counted as `kinds` say, the key
    /// first.
    fn map_put(
        &mut self,
        map: Operand,
        key: Operand,
        key_ty: &Type,
        values: &[Operand],
        kinds: &[Option<RcKind>],
    ) {
        let first = self.fresh();
        self.map_find(map, key, key_ty, Some(first));
        let (found, missing, store) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Ge,
            float: false,
            lhs: Operand::Reg(first),
            rhs: Operand::Imm(0),
            then: found,
            otherwise: missing,
        });
        self.start(missing);
        let string = Operand::Imm((*key_ty == Type::Str) as i64);
        self.runtime_call("stone.map_add", vec![map, key, string], Some(first));
        self.jump(store);
        self.start(found);
        for (field, kind) in kinds.iter().enumerate().skip(1) {
            if let Some(kind) = *kind {
                let index = self.slot(Operand::Reg(first), field);
                let old = self.fresh();
                self.push(Inst::ListGet {
                    dst: old,
                    list: map,
                    index,
                    offset: 0,
                });
                self.push(Inst::Release {
                    src: Operand::Reg(old),
                    kind,
                });
            }
        }
        self.jump(store);
        self.start(store);
        for (field, &value) in values.iter().enumerate() {
            let index = self.slot(Operand::Reg(first), field + 1);
            self.push(Inst::ListSet {
                list: map,
                index,
                value,
                offset: 0,
            });
        }
    }

    /// Prints a value of type `ty`, given as its values: a tuple as Python writes one, with its
    /// strings quoted, as strings `nested` in a list or tuple are.
    ///
    /// For example, `(1, "a")` prints `(1, 'a')`.
    fn print_flat(&mut self, ty: &Type, values: &[Operand], nested: bool) {
        match ty {
            Type::Tuple(fields) => {
                self.print_char('(');
                let mut start = 0;
                for (k, field) in fields.iter().enumerate() {
                    if k > 0 {
                        self.print_char(',');
                        self.print_char(' ');
                    }
                    let end = start + self.leaves(field).len();
                    self.print_flat(field, &values[start..end], true);
                    start = end;
                }
                self.print_char(')');
            }
            Type::List(elem) if holds_flat(elem) || holds_enum(elem) || holds_function(elem) => {
                self.print_elements(elem, values[0])
            }
            // a function value or a handle is written as its type, which this copy knows
            Type::Function { .. } | Type::Handle(_) => {
                let text = self.fresh();
                self.push(Inst::StrAddr {
                    dst: text,
                    text: type_text(ty),
                });
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Print(Type::Str),
                    args: vec![Operand::Reg(text)],
                });
            }
            Type::Map(key, value) => self.print_entries(Some(key), value, values[0]),
            // its own display says how it is written
            Type::Struct { .. } if self.implements(ty, DISPLAY) => {
                let text = Operand::Reg(self.compare_call(ty, DISPLAY, values, &[]));
                self.own(text, Some(RcKind::Str));
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Print(Type::Str),
                    args: vec![text],
                });
                self.release_temp(text);
            }
            Type::Struct { .. } | Type::Enum { .. } => {
                let label = self.helper(Helper::Print(ty.clone()));
                let mut args = values.to_vec();
                args.push(self.print_depth);
                self.push(Inst::Call {
                    dsts: vec![],
                    callee: Callee::Helper(label),
                    args,
                });
            }
            Type::Str if nested => self.push(Inst::Call {
                dsts: vec![],
                callee: Callee::Runtime("stone.print_str_quoted"),
                args: vec![values[0]],
            }),
            _ => self.push(Inst::Call {
                dsts: vec![],
                callee: Callee::Print(ty.clone()),
                args: vec![values[0]],
            }),
        }
    }

    /// Prints the list `list`, whose elements are of type `elem` and hold a tuple, as `[a, b]`,
    /// with a loop over its elements, since the runtime's list printers take one value each.
    fn print_elements(&mut self, elem: &Type, list: Operand) {
        self.print_entries(None, elem, list);
    }

    /// Prints the elements of `list` of type `elem` as [`Lowerer::print_elements`] does, or, with
    /// a `key` type, the entries of a map whose values are of type `elem`, as `{k: v, ...}`.
    fn print_entries(&mut self, key: Option<&Type>, elem: &Type, list: Operand) {
        let keyed = key.is_some() as usize;
        let width = keyed + self.leaves(elem).len();
        let (open, close) = match key {
            Some(_) => ('{', '}'),
            None => ('[', ']'),
        };
        let (header, body, separate, element, exit) = (
            self.new_block(),
            self.new_block(),
            self.new_block(),
            self.new_block(),
            self.new_block(),
        );
        if key.is_some() {
            self.close_up(list);
        }
        self.print_char(open);
        let count = self.element_count(list, width);
        let next = self.fresh();
        self.push(Inst::Copy {
            dst: next,
            src: Operand::Imm(0),
        });
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(next),
            rhs: count,
            then: body,
            otherwise: exit,
        });
        self.start(body);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(next),
            rhs: Operand::Imm(0),
            then: element,
            otherwise: separate,
        });
        self.start(separate);
        self.print_char(',');
        self.print_char(' ');
        self.start(element);
        // printing changes nothing, so the list's values are borrowed
        let kinds = vec![None; width];
        let values = self.load_slots(list, Operand::Reg(next), width, 0..width, &kinds);
        if let Some(key) = key {
            self.print_flat(key, &values[..1], true);
            self.print_char(':');
            self.print_char(' ');
        }
        self.print_flat(elem, &values[keyed..], true);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        self.print_char(close);
    }

    /// Leaves null in each variable that gave its reference away and that its statement did not
    /// assign again, so releasing it later does nothing.
    fn clear_moved(&mut self) {
        let mut locals: Vec<VReg> = self.moved_locals.drain().collect();
        locals.sort_by_key(|reg| reg.0);
        for local in locals {
            self.push(Inst::Copy {
                dst: local,
                src: Operand::Imm(0),
            });
        }
        let mut globals: Vec<String> = self.moved_globals.drain().collect();
        globals.sort();
        for name in globals {
            self.push(Inst::StoreGlobal {
                name,
                src: Operand::Imm(0),
            });
        }
    }

    /// Returns whether `expr` is arithmetic on lists, which [`Lowerer::elementwise`] lowers.
    fn is_elementwise(&self, expr: &Expr) -> bool {
        matches!(
            expr.kind,
            ExprKind::BinOp { .. }
                | ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd | UnaryOp::UnarySub,
                    ..
                }
        ) && matches!(self.span_type(expr.span), Some(Type::List(_)))
    }

    /// Returns whether `expr` is a call to a stone function, rather than a builtin or a builtin
    /// method: a function, or a function of an `impl` block, possibly followed by `?`s.
    fn is_stone_call(&self, expr: &Expr) -> bool {
        let expr = expr.without_try();
        let ExprKind::Call { func, .. } = &expr.kind else {
            return false;
        };
        match &func.kind {
            ExprKind::Name { id, .. } => self.context.owned_params.contains_key(id),
            _ => self.context.methods.contains_key(&expr.span),
        }
    }

    /// Lowers `str(value)` of a value that is not a number, bool, or string, which writes it as
    /// `print` would into a new text and makes a string of that.
    ///
    /// For example, `str([1])` evaluates `[1]`, calls `stone.text_begin`, prints the list, and
    /// takes the string `"[1]"` from `stone.text_end`.
    fn display(
        &mut self,
        value: &Expr,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let ty = self.type_of(value)?;
        let values = self.expr_flat(value)?;
        let outer = self.runtime_call("stone.text_begin", vec![], None);
        self.print_flat(&ty, &values, false);
        let text = self.runtime_call("stone.text_end", vec![outer], into);
        for value in values {
            self.release_temp(value);
        }
        self.own(text, kind);
        Ok(text)
    }

    /// Lowers a string with values in it, such as `"x is {x}"`, which evaluates every value
    /// first, left to right, then writes its text and its values in turn into a new text, as
    /// `print` would, and makes a string of that.
    ///
    /// For example, `"x is {x}"` calls `stone.text_begin`, prints `x is ` and then `x`, and takes
    /// the string from `stone.text_end`.
    fn joined(
        &mut self,
        pieces: &[Expr],
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let mut values = Vec::with_capacity(pieces.len());
        for piece in pieces {
            values.push(match &piece.kind {
                ExprKind::Constant { value, .. } if matches!(&**value, Constant::Str(_)) => None,
                _ => Some((self.expr_flat(piece)?, self.type_of(piece)?)),
            });
        }
        let outer = self.runtime_call("stone.text_begin", vec![], None);
        for (piece, value) in pieces.iter().zip(&values) {
            match value {
                Some((leaves, ty)) => self.print_flat(ty, leaves, false),
                None => {
                    if let ExprKind::Constant { value, .. } = &piece.kind
                        && let Constant::Str(text) = &**value
                    {
                        self.print_text(text);
                    }
                }
            }
        }
        let text = self.runtime_call("stone.text_end", vec![outer], into);
        for value in values.into_iter().flatten().flat_map(|(leaves, _)| leaves) {
            self.release_temp(value);
        }
        self.own(text, kind);
        Ok(text)
    }

    /// Prints `text` as it is, through the string printer.
    fn print_text(&mut self, text: &str) {
        let dst = self.fresh();
        self.push(Inst::StrAddr {
            dst,
            text: text.to_string(),
        });
        self.push(Inst::Call {
            dsts: vec![],
            callee: Callee::Print(Type::Str),
            args: vec![Operand::Reg(dst)],
        });
    }

    fn print_char(&mut self, c: char) {
        self.push(Inst::Call {
            dsts: vec![],
            callee: Callee::Runtime("stone.print_char"),
            args: vec![Operand::Imm(c as i64)],
        });
    }
}

/// Returns whether a value of type `ty` is or holds a tuple or struct, directly or in a list,
/// which lowering prints a value at a time rather than with a runtime printer.
///
/// For example, `tuple[int, str]` and `list[list[Point]]` do, and `list[int]` does not.
fn holds_flat(ty: &Type) -> bool {
    match ty {
        Type::Tuple(_) | Type::Struct { .. } | Type::Map(..) => true,
        Type::List(elem) => holds_flat(elem),
        _ => false,
    }
}

/// Returns the runtime routine that compares two strings, or two bytes, for equality: bytes may
/// hold a 0 byte, where `stone.str_eq` stops.
fn equality_routine(ty: &Type) -> &'static str {
    match ty {
        Type::Bytes => "stone.bytes_eq",
        _ => "stone.str_eq",
    }
}

/// Returns the runtime routine that orders two strings, or two bytes, giving -1, 0, or 1.
fn order_routine(ty: &Type) -> &'static str {
    match ty {
        Type::Bytes => "stone.bytes_cmp",
        _ => "stone.str_cmp",
    }
}

/// Returns whether a value of type `ty` is or holds a socket or listener, looking through lists,
/// maps, tuples, the fields of structs, and the values of enums' variants, each struct and enum
/// once, by its name in `seen`.
///
/// For example, `socket`, `list[listener]`, and `option[socket]` do, and `list[str]` does not.
fn holds_handle(ty: &Type, structs: &Structs, enums: &Enums, seen: &mut HashSet<String>) -> bool {
    match ty {
        Type::Handle(_) => true,
        Type::List(elem) => holds_handle(elem, structs, enums, seen),
        Type::Map(_, value) => holds_handle(value, structs, enums, seen),
        Type::Tuple(fields) => fields.iter().any(|f| holds_handle(f, structs, enums, seen)),
        Type::Struct { .. } => {
            seen.insert(ty.to_string())
                && ty
                    .struct_fields(structs)
                    .iter()
                    .any(|(_, f)| holds_handle(f, structs, enums, seen))
        }
        Type::Enum { .. } => {
            seen.insert(ty.to_string())
                && variants(enums, ty)
                    .iter()
                    .flat_map(|(_, fields)| fields)
                    .any(|f| holds_handle(f, structs, enums, seen))
        }
        _ => false,
    }
}

/// Returns whether a value of type `ty` is a function value or a handle, or a list of them, at
/// any depth, which lowering prints as its type rather than with a runtime printer.
fn holds_function(ty: &Type) -> bool {
    match ty {
        Type::Function { .. } | Type::Handle(_) => true,
        Type::List(elem) => holds_function(elem),
        _ => false,
    }
}

/// Returns whether `target` is a variable or values of variables an assignment replaces as a
/// whole: a name, names to unpack into, or a field of a variable, such as `p.items`, but not a
/// list element, whose indexes are read after a call.
fn is_variable_target(target: &Expr) -> bool {
    match &target.kind {
        ExprKind::Name { .. } => true,
        ExprKind::Tuple { elts, .. } => elts.iter().all(is_variable_target),
        ExprKind::Attribute { value, .. } => is_variable_target(value),
        _ => false,
    }
}

/// Returns whether the place `expr` is reached through a struct's field, as `ps[i].x` and
/// `p.items[0]` are.
fn has_field(expr: &Expr) -> bool {
    place(expr).is_some_and(|(_, steps)| steps.iter().any(|s| matches!(s, Step::Field(_))))
}

/// Where the value a change reaches lives, from [`Lowerer::locate`].
enum Location {
    /// Variables of lowering, locals or globals, one for each value, such as `p.1`.
    Leaves(Vec<String>),
    /// `count` slots of the unique list `list`, from slot `first`.
    Slots {
        list: Operand,
        first: Operand,
        count: usize,
    },
}

/// Returns whether `==` compares values of type `ty` value by value: a tuple, a struct, or an
/// enum value, rather than one number, bool, or string.
fn is_compound(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(_) | Type::Struct { .. } | Type::Enum { .. })
}

/// Returns whether a value of type `ty` is or holds an enum value, in a list or tuple, which
/// lowering prints with a function it makes for the enum's type.
///
/// For example, `option[int]` and `list[tuple[int, Shape]]` do, and `list[int]` does not.
fn holds_enum(ty: &Type) -> bool {
    match ty {
        Type::Enum { .. } => true,
        Type::List(elem) => holds_enum(elem),
        Type::Map(_, value) => holds_enum(value),
        Type::Tuple(fields) => fields.iter().any(holds_enum),
        _ => false,
    }
}

/// Returns, for each function in `body`, whether it owns each of its parameters, which it does if
/// it assigns or changes the parameter.
///
/// For example, `fn add(xs: list[int], x: int) -> list[int]: xs.append(x): ret xs` owns `xs`
/// but not `x`.
fn owned_params(body: &[Stmt], methods: &Methods) -> HashMap<String, Vec<bool>> {
    let mut owned = HashMap::new();
    let mut add = |name: String, args: &crate::ast::Arguments, body: &[Stmt]| {
        let mut assigned = Vec::new();
        collect_assigned(body, &mut assigned);
        let mut changed = HashSet::new();
        collect_changed(body, &mut changed, methods);
        let params = args
            .args
            .iter()
            .map(|arg| {
                changed.contains(&arg.arg) || assigned.iter().any(|(name, _)| *name == arg.arg)
            })
            .collect();
        owned.insert(name, params);
    };
    for stmt in body {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => add(name.clone(), args, body),
            StmtKind::ImplDef {
                name: owner,
                methods: defs,
                ..
            }
            | StmtKind::TraitDef {
                name: owner,
                methods: defs,
                ..
            } => {
                for def in defs {
                    if let StmtKind::FunctionDef {
                        name, args, body, ..
                    } = &def.kind
                    {
                        add(format!("{owner}.{name}"), args, body);
                    }
                }
            }
            _ => {}
        }
    }
    owned
}

/// What `==` compares in turn, from [`Lowerer::compared`]: one value, or the values of a struct
/// that implements `eq`, which its `eq` compares, with how many there are.
enum Compared {
    Value(Type),
    Custom(Type, usize),
}

/// Arithmetic on lists with its operands evaluated, from [`Lowerer::fused`].
enum Fused {
    /// A number, the same for every element.
    Number(Operand),
    List(FusedList),
    /// An operation on ints, or on floats if `float`.
    Binary {
        op: BinOp,
        float: bool,
        lhs: Box<Fused>,
        rhs: Box<Fused>,
    },
    Neg {
        float: bool,
        operand: Box<Fused>,
    },
}

/// A list operand of arithmetic on lists, read one element at a time.
struct FusedList {
    value: Operand,
    /// Whether its elements are floats.
    float: bool,
    /// The variable it is, if any, which can give its list to the result.
    name: Option<String>,
}

/// The most nodes a [`Fused`] tree may have to be computed in pairs, so that its numbers, its
/// intermediate results, and the few registers a packed loop needs besides fit the 13 vector
/// registers x64 has free.
const MAX_PACKED_SIZE: usize = 10;

impl Fused {
    /// Returns whether every operation in the tree can be computed two elements at a time with
    /// packed instructions, on floats if `float` and ints otherwise: adding, subtracting, and
    /// negating either, and multiplying and dividing floats.
    fn packable(&self, float: bool) -> bool {
        match self {
            Fused::Number(_) => true,
            Fused::List(leaf) => leaf.float == float,
            Fused::Binary {
                op,
                float: node,
                lhs,
                rhs,
            } => {
                let packs = match op {
                    BinOp::Add | BinOp::Sub => true,
                    BinOp::Mul | BinOp::Div => float,
                    BinOp::Rem | BinOp::Pow => false,
                };
                packs && *node == float && lhs.packable(float) && rhs.packable(float)
            }
            Fused::Neg {
                float: node,
                operand,
            } => *node == float && operand.packable(float),
        }
    }

    /// Returns how many nodes the tree has.
    fn size(&self) -> usize {
        match self {
            Fused::Number(_) | Fused::List(_) => 1,
            Fused::Binary { lhs, rhs, .. } => 1 + lhs.size() + rhs.size(),
            Fused::Neg { operand, .. } => 1 + operand.size(),
        }
    }

    /// Returns the tree as a kernel for [`Inst::Packed`], reading each list through its data
    /// pointer in `data`.
    fn kernel(&self, data: &HashMap<Operand, Operand>) -> Kernel {
        match self {
            Fused::Number(value) => Kernel::Number(*value),
            Fused::List(leaf) => Kernel::Element(data[&leaf.value]),
            Fused::Binary { op, lhs, rhs, .. } => {
                Kernel::Binary(*op, Box::new(lhs.kernel(data)), Box::new(rhs.kernel(data)))
            }
            Fused::Neg { operand, .. } => Kernel::Neg(Box::new(operand.kernel(data))),
        }
    }

    /// Adds every list in the tree to `out`, left to right.
    fn lists<'a>(&'a self, out: &mut Vec<&'a FusedList>) {
        match self {
            Fused::Number(_) => {}
            Fused::List(leaf) => out.push(leaf),
            Fused::Binary { lhs, rhs, .. } => {
                lhs.lists(out);
                rhs.lists(out);
            }
            Fused::Neg { operand, .. } => operand.lists(out),
        }
    }
}

/// Returns the IR operation for an arithmetic operator.
///
/// For example, `bin_op(Operator::Modulo)` is `BinOp::Rem`.
fn bin_op(op: Operator) -> BinOp {
    match op {
        Operator::Add => BinOp::Add,
        Operator::Subtract => BinOp::Sub,
        Operator::Multiply => BinOp::Mul,
        Operator::Divide => BinOp::Div,
        Operator::Modulo => BinOp::Rem,
        Operator::Power => BinOp::Pow,
    }
}

/// Returns whether evaluating `expr` can call a function or method, which is the only way code
/// can change a list while an expression is being evaluated.
///
/// For example, `i + 1` and `a[i]` make no calls, but `f(i)` and `xs.len()` do.
fn has_call(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Lambda { .. } => false,
        // a struct among a string's values may be written by its own `display`
        ExprKind::Call { .. } | ExprKind::JoinedStr { .. } => true,
        ExprKind::Constant { .. } | ExprKind::Name { .. } => false,
        ExprKind::BinOp { left, right, .. } => has_call(left) || has_call(right),
        ExprKind::UnaryOp { operand, .. } | ExprKind::Try { value: operand } => has_call(operand),
        ExprKind::BoolOp { values, .. }
        | ExprKind::List { elts: values, .. }
        | ExprKind::Tuple { elts: values, .. } => values.iter().any(has_call),
        ExprKind::Dict { keys, values } => keys.iter().chain(values).any(has_call),
        ExprKind::Compare {
            left, comparators, ..
        } => has_call(left) || comparators.iter().any(has_call),
        ExprKind::Subscript { value, slice, .. } => has_call(value) || has_call(slice),
        ExprKind::Attribute { value, .. } => has_call(value),
        ExprKind::Slice { lower, upper, step } => [lower, upper, step]
            .into_iter()
            .flatten()
            .any(|b| has_call(b)),
    }
}
