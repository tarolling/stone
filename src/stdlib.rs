//! The builtin functions shared by the interpreter and the compiler, and the limits both enforce.

pub mod fs;
pub mod math;
pub mod net;
pub mod os;
pub mod random;
pub mod time;

/// A module that is part of stone rather than a file of the program, such as `os`.
///
/// A program imports one like a module file, with `use os` then `os.env("HOME")`, or with
/// `use os.env` then `env("HOME")`. Linking renames each call to the function's linked name,
/// such as `os.env`, which no stone name can collide with since it holds a dot, and the checker
/// and both backends treat that name as a builtin.
pub struct BuiltinModule {
    /// The name a program imports it by, as in `use os`.
    pub name: &'static str,
    /// What the module is for, which editors show for its name.
    pub doc: &'static str,
    /// Every function of the module, each named by its linked name, such as `os.env`.
    pub functions: &'static [BuiltinDoc],
}

/// Every builtin module.
pub static MODULES: [BuiltinModule; 5] = [
    BuiltinModule {
        name: os::MODULE,
        doc: os::MODULE_DOC,
        functions: &os::DOCS,
    },
    BuiltinModule {
        name: math::MODULE,
        doc: math::MODULE_DOC,
        functions: &math::DOCS,
    },
    BuiltinModule {
        name: random::MODULE,
        doc: random::MODULE_DOC,
        functions: &random::DOCS,
    },
    BuiltinModule {
        name: time::MODULE,
        doc: time::MODULE_DOC,
        functions: &time::DOCS,
    },
    BuiltinModule {
        name: fs::MODULE,
        doc: fs::MODULE_DOC,
        functions: &fs::DOCS,
    },
];

/// The builtin modules that only stone's own modules written in stone, such as `net`, may use,
/// which editors and the reference do not list.
pub static INTERNAL_MODULES: [BuiltinModule; 1] = [BuiltinModule {
    name: net::SYS_MODULE,
    doc: net::SYS_MODULE_DOC,
    functions: &net::SYS_DOCS,
}];

/// The modules that are part of stone but written in stone, by name with their source, which a
/// program that uses one is linked with like a module file of its own.
pub static SOURCE_MODULES: [(&str, &str); 1] = [(net::MODULE, net::SOURCE)];

/// Returns the builtin module named `name`, internal ones included.
///
/// For example, `module("os")` is the `os` module, and `module("util")` is `None`.
pub fn module(name: &str) -> Option<&'static BuiltinModule> {
    MODULES
        .iter()
        .chain(&INTERNAL_MODULES)
        .find(|module| module.name == name)
}

/// Returns the source of the module written in stone named `name`, if there is one.
///
/// For example, `source_module("net")` is the text of `net.stn`.
pub fn source_module(name: &str) -> Option<&'static str> {
    SOURCE_MODULES
        .iter()
        .find(|(module, _)| *module == name)
        .map(|(_, source)| *source)
}

/// Returns the documentation for the function of a builtin module with the linked name `name`.
///
/// For example, `module_function("os.env")` has the signature `os.env(name: str) -> option[str]`, and
/// `module_function("env")` is `None`.
pub fn module_function(name: &str) -> Option<&'static BuiltinDoc> {
    let (prefix, _) = name.split_once('.')?;
    module(prefix)?
        .functions
        .iter()
        .find(|doc| doc.name == name)
}

/// The most function calls that can be active at once, in both backends.
///
/// For example, a recursive function can call itself 999 times from a top-level call, but a
/// 1,001st nested call stops the program with an error instead of overflowing the stack.
pub const MAX_CALL_DEPTH: usize = 1_000;

/// The most values one element of a list can hold once its tuples are flattened, since compiled
/// code records whether each is counted in 2 bits of one 64-bit word.
///
/// For example, a `list[tuple[int, tuple[str, float]]]` holds 3 values in each element.
pub const MAX_ELEMENT_VALUES: usize = 32;

/// Names of the builtin functions.
///
/// `range` is only valid as the iterable of a `for` loop, such as `for i in range(3):`.
pub static BUILTINS: [&str; 10] = [
    "print", "eprint", "range", "int", "float", "str", "bytes", "input", "eof", "args",
];

/// The prelude, a stone file linked into every program, which defines `option[T]`, `ordering`,
/// `result[T, E]`, the traits `eq`, `ord`, and `display`, and the sort `xs.sort()` uses for values of other
/// types.
pub const PRELUDE: &str = include_str!("prelude.stn");

/// The prelude's struct of what a command run by `os.run` did: its exit status, then what it
/// wrote to stdout and to stderr.
pub const OUTPUT: &str = "output";

/// The linked name of the prelude's sort, which `sort` uses for values that are not numbers or
/// strings.
pub const PRELUDE_SORT: &str = "prelude.sort";

/// The methods of lists that take a function, each with the linked name of the prelude's function
/// that gives it, which takes the list first: `xs.map(f)` is `prelude.map(xs, f)`.
/// `sort_by_key` changes the list, to what its function returns.
pub static PRELUDE_METHODS: [(&str, &str); 7] = [
    ("map", "prelude.map"),
    ("filter", "prelude.filter"),
    ("any", "prelude.any"),
    ("all", "prelude.all"),
    ("sort_by_key", "prelude.sort_by_key"),
    ("min_by_key", "prelude.min_by_key"),
    ("max_by_key", "prelude.max_by_key"),
];

/// Returns the linked name of the prelude's function that gives the list method `name`, if it is
/// one of [`PRELUDE_METHODS`].
///
/// For example, `prelude_method("map")` is `Some("prelude.map")`, and `prelude_method("len")` is
/// `None`.
pub fn prelude_method(name: &str) -> Option<&'static str> {
    PRELUDE_METHODS
        .iter()
        .find(|(method, _)| *method == name)
        .map(|(_, function)| *function)
}

/// The variants of the prelude's enums, which every file writes by name alone, as in `some(x)`
/// and `none`, each with its enum, in the order the prelude defines them.
pub static PRELUDE_VARIANTS: [(&str, &str); 7] = [
    ("none", "option"),
    ("some", "option"),
    ("less", "ordering"),
    ("equal", "ordering"),
    ("greater", "ordering"),
    ("ok", "result"),
    ("err", "result"),
];

/// Returns the prelude enum whose variant is written alone as `name`, such as `option` for
/// `some`, if there is one.
pub fn prelude_variant(name: &str) -> Option<&'static str> {
    PRELUDE_VARIANTS
        .iter()
        .find(|(variant, _)| *variant == name)
        .map(|(_, enum_name)| *enum_name)
}

/// Names of the builtin methods, which are called on a value, such as `xs.len()`.
pub static METHODS: [&str; 58] = [
    "len",
    "append",
    "pop",
    "insert",
    "extend",
    "sort",
    "sort_by_key",
    "map",
    "filter",
    "any",
    "all",
    "min_by_key",
    "max_by_key",
    "strip",
    "split",
    "splitlines",
    "join",
    "find",
    "rfind",
    "count",
    "replace",
    "lower",
    "upper",
    "startswith",
    "endswith",
    "is_some",
    "is_none",
    "unwrap",
    "unwrap_or",
    "is_ok",
    "is_err",
    "unwrap_err",
    "ok",
    "err",
    "parse_int",
    "parse_float",
    "contains",
    "get",
    "keys",
    "values",
    "items",
    "eq",
    "cmp",
    "display",
    "send",
    "read_line",
    "read",
    "read_all",
    "accept",
    "port",
    "close",
    "bytes",
    "text",
    "set_timeout",
    "send_bytes",
    "read_bytes",
    "read_exact",
    "read_all_bytes",
];

/// The builtin methods only a `str` has, other than `parse_int` and `parse_float`, which
/// [`METHODS`] lists too. A struct may have methods of the same names.
pub const STR_METHODS: [&str; 12] = [
    "strip",
    "split",
    "splitlines",
    "join",
    "find",
    "rfind",
    "count",
    "replace",
    "lower",
    "upper",
    "startswith",
    "endswith",
];

/// The builtin methods of a `socket` or `listener`, which [`METHODS`] lists too.
pub const HANDLE_METHODS: [&str; 12] = [
    "send",
    "read_line",
    "read",
    "read_all",
    "accept",
    "port",
    "close",
    "set_timeout",
    "send_bytes",
    "read_bytes",
    "read_exact",
    "read_all_bytes",
];

/// Returns whether the builtin method `name` changes the value it is called on, as `append` does,
/// rather than only reading it.
///
/// Such a call changes the variable that holds the value, like an assignment, so it must be a
/// statement of its own whose receiver is a variable or an element of one, such as
/// `grid[0].append(1)`. `pop`, which also returns a value, may instead be inside any expression
/// that nothing else in its statement uses the list in, as in `print(xs.pop().unwrap())`.
pub fn changes_receiver(name: &str) -> bool {
    matches!(
        name,
        "append" | "pop" | "insert" | "extend" | "sort" | "sort_by_key"
    )
}

/// Documentation for a builtin function or method, for editor tooling.
pub struct BuiltinDoc {
    pub name: &'static str,
    /// How the builtin is called, such as `int(value: int | float) -> int`.
    pub signature: &'static str,
    pub description: &'static str,
}

pub static BUILTIN_DOCS: [BuiltinDoc; 10] = [
    BuiltinDoc {
        name: "print",
        signature: "print(values...)",
        description: "Prints the values separated by spaces, followed by a newline.",
    },
    BuiltinDoc {
        name: "eprint",
        signature: "eprint(values...)",
        description: "Writes the values as `print` does, but to standard error, so they stay \
                      out of the output when it is piped into another program.",
    },
    BuiltinDoc {
        name: "range",
        signature: "range(end) | range(start, end)",
        description: "Counts from `start`, or 0, up to but not including `end`. \
                      It can only be the iterable of a `for` loop.",
    },
    BuiltinDoc {
        name: "int",
        signature: "int(value: int | float | str) -> int",
        description: "Converts a number to an int, dropping any fraction, so `int(-2.5)` is -2, \
                      or reads a decimal int from a string, such as `int(\" -42\\n\")`. \
                      It stops the program if `value` is nan, does not fit in an int, \
                      or is a string that is not an int.",
    },
    BuiltinDoc {
        name: "float",
        signature: "float(value: int | float | str) -> float",
        description: "Converts a number to a float, rounding to the nearest float if needed, \
                      or reads a float from a string, such as `float(\"2.5e3\")` or \
                      `float(\"inf\")`. It stops the program if the string is not a float.",
    },
    BuiltinDoc {
        name: "str",
        signature: "str(value) -> str",
        description: "Returns the text `print` would write for `value`, so `str(1.0)` is `\"1.0\"` \
                      and `str([\"a\"])` is `\"['a']\"`. A struct with an `impl display` block \
                      is written as its `display` method says.",
    },
    BuiltinDoc {
        name: "bytes",
        signature: "bytes(values: list[int]) -> bytes",
        description: "Returns the bytes whose values are `values`, so `bytes([104, 105])` is \
                      `b\"hi\"`. A value outside 0 to 255 stops the program with an error.",
    },
    BuiltinDoc {
        name: "input",
        signature: "input() | input(prompt: str) -> result[str, str]",
        description: "Writes `prompt`, if given, then reads a line from standard input and \
                      returns `ok` of it without its newline, or `err(\"end of input\")` once \
                      the input is over, so `line = input()?` passes the end on.",
    },
    BuiltinDoc {
        name: "eof",
        signature: "eof() -> bool",
        description: "Returns whether standard input has nothing left to read, waiting for \
                      input if none has arrived yet.",
    },
    BuiltinDoc {
        name: "args",
        signature: "args() -> list[str]",
        description: "Returns the arguments given after the program, so `stone run f.stn a b` \
                      and a built `./f a b` both see `[\"a\", \"b\"]`.",
    },
];

pub static METHOD_DOCS: [BuiltinDoc; 58] = [
    BuiltinDoc {
        name: "len",
        signature: "(str | bytes | list[T] | map[K, V]).len() -> int",
        description: "Returns the number of bytes in a string or bytes, elements in a list, or \
                      entries in a map.",
    },
    BuiltinDoc {
        name: "append",
        signature: "list[T].append(item: T)",
        description: "Adds `item` to the end of the list, changing only the variable it is \
                      called on, since lists are values. It is a statement of its own.",
    },
    BuiltinDoc {
        name: "pop",
        signature: "list[T].pop() -> option[T] | list[T].pop(index: int) -> T | \
                    map[K, V].pop(key: K) -> option[V]",
        description: "Without `index`, removes the last element and returns `some` of it, or \
                      `none` if the list is empty, so `x = xs.pop()` takes the last element off \
                      `xs`. With `index`, removes the element there and returns it, stopping \
                      the program if the list is empty or `index` is out of range; a negative \
                      `index` counts from the end. On a map, removes the entry for `key` and \
                      returns `some` of its value, or `none` if there is none, keeping the \
                      other entries in order. It may be inside any expression, as in \
                      `xs.pop().unwrap()`, if nothing else in the statement uses what it \
                      changes.",
    },
    BuiltinDoc {
        name: "insert",
        signature: "list[T].insert(index: int, item: T)",
        description: "Puts `item` at `index`, moving the elements from there on up by one, so \
                      `xs.insert(0, x)` adds `x` at the front and `xs.insert(xs.len(), x)` at the \
                      end. A negative `index` counts from the end. Unlike Python, it stops the \
                      program if `index` is out of range. It is a statement of its own.",
    },
    BuiltinDoc {
        name: "extend",
        signature: "list[T].extend(items: list[T])",
        description: "Adds every element of `items` to the end of the list, in order. It is a \
                      statement of its own.",
    },
    BuiltinDoc {
        name: "sort",
        signature: "list[T].sort()",
        description: "Puts the elements of a list of `int`, `float`, `str`, or a type that \
                      implements `ord` in ascending order. Strings compare byte by byte, and \
                      floats in the order `-inf`, the negative numbers, `-0.0`, `0.0`, the \
                      positive numbers, `inf`, then `nan`. Other values compare by their `cmp`, \
                      and equal ones keep their order. It is a statement of its own.",
    },
    BuiltinDoc {
        name: "sort_by_key",
        signature: "list[T].sort_by_key(key: fn(T) -> K)",
        description: "Puts the elements in ascending order of what `key` gives for each, which \
                      must be a type `<` orders, such as `int` or `str`. It calls `key` once \
                      for each element, in order, before it sorts, and elements with equal keys \
                      keep their order. It is a statement of its own.",
    },
    BuiltinDoc {
        name: "map",
        signature: "list[T].map(f: fn(T) -> U) -> list[U]",
        description: "Returns a new list of what `f` gives for each element, in order, so \
                      `[1, 2].map(double)` is `[2, 4]`.",
    },
    BuiltinDoc {
        name: "filter",
        signature: "list[T].filter(keep: fn(T) -> bool) -> list[T]",
        description: "Returns a new list of the elements `keep` gives true for, in order.",
    },
    BuiltinDoc {
        name: "any",
        signature: "list[T].any(test: fn(T) -> bool) -> bool",
        description: "Returns whether `test` gives true for any element, calling it on each in \
                      order until one does. It is false for an empty list.",
    },
    BuiltinDoc {
        name: "all",
        signature: "list[T].all(test: fn(T) -> bool) -> bool",
        description: "Returns whether `test` gives true for every element, calling it on each in \
                      order until one does not. It is true for an empty list.",
    },
    BuiltinDoc {
        name: "min_by_key",
        signature: "list[T].min_by_key(key: fn(T) -> K) -> option[T]",
        description: "Returns the first element with the least key, the one `sort_by_key` puts \
                      first, or `none` for an empty list. It calls `key` once for each \
                      element, in order.",
    },
    BuiltinDoc {
        name: "max_by_key",
        signature: "list[T].max_by_key(key: fn(T) -> K) -> option[T]",
        description: "Returns the last element with the greatest key, the one `sort_by_key` \
                      puts last, or `none` for an empty list. It calls `key` once for each \
                      element, in order.",
    },
    BuiltinDoc {
        name: "strip",
        signature: "str.strip() -> str",
        description: "Returns the string without the spaces, tabs, and line breaks at either end.",
    },
    BuiltinDoc {
        name: "split",
        signature: "str.split() | str.split(separator: str) -> list[str]",
        description: "Splits the string at each `separator`, keeping empty pieces, so \
                      `\"a,,b\".split(\",\")` is `[\"a\", \"\", \"b\"]`. Without a separator it \
                      splits at runs of whitespace and drops empty pieces, so \
                      `\" a  b \".split()` is `[\"a\", \"b\"]`. An empty separator stops the program.",
    },
    BuiltinDoc {
        name: "splitlines",
        signature: "str.splitlines() -> list[str]",
        description: "Splits the string into lines at each `\\n`, `\\r\\n`, or `\\r`, without \
                      their endings, so `\"a\\r\\nb\\n\".splitlines()` is `[\"a\", \"b\"]`. A \
                      final line ending ends the last line rather than starting an empty one, \
                      and an empty string has no lines.",
    },
    BuiltinDoc {
        name: "join",
        signature: "str.join(items: list[str]) -> str",
        description: "Returns the strings in `items` with this string between each pair, so \
                      `\", \".join([\"a\", \"b\"])` is `\"a, b\"`.",
    },
    BuiltinDoc {
        name: "find",
        signature: "str.find(text: str) -> option[int]",
        description: "Returns `some` of the byte index where `text` first appears in the \
                      string, or `none` if it never does, so `\"banana\".find(\"na\")` is \
                      `some(2)`. An empty `text` is found at 0.",
    },
    BuiltinDoc {
        name: "rfind",
        signature: "str.rfind(text: str) -> option[int]",
        description: "Returns `some` of the byte index where `text` last appears in the \
                      string, or `none` if it never does, so `\"banana\".rfind(\"na\")` is \
                      `some(4)`. An empty `text` is found at the end, `s.len()`.",
    },
    BuiltinDoc {
        name: "count",
        signature: "str.count(text: str) -> int",
        description: "Returns how many times `text` appears in the string without overlapping, \
                      counting from the start, so `\"aaaa\".count(\"aa\")` is 2. An empty `text` \
                      is found between every two characters and at both ends, so \
                      `\"ab\".count(\"\")` is 3.",
    },
    BuiltinDoc {
        name: "replace",
        signature: "str.replace(old: str, new: str) | str.replace(old: str, new: str, count: int) \
                    -> str",
        description: "Returns the string with each `old` that `count` would count replaced by \
                      `new`, so `\"a-b-c\".replace(\"-\", \"+\")` is `\"a+b+c\"`. With `count`, \
                      only the first `count` are replaced, or all of them if `count` is \
                      negative. An empty `old` matches between every two characters and at \
                      both ends, so `\"ab\".replace(\"\", \"-\")` is `\"-a-b-\"`.",
    },
    BuiltinDoc {
        name: "lower",
        signature: "str.lower() -> str",
        description: "Returns the string with each ASCII letter `A` to `Z` made lowercase, so \
                      `\"Hello\".lower()` is `\"hello\"`. Unlike Python, letters outside ASCII, \
                      such as `\"É\"`, stay as they are.",
    },
    BuiltinDoc {
        name: "upper",
        signature: "str.upper() -> str",
        description: "Returns the string with each ASCII letter `a` to `z` made uppercase, so \
                      `\"Hello\".upper()` is `\"HELLO\"`. Unlike Python, letters outside ASCII, \
                      such as `\"é\"`, stay as they are.",
    },
    BuiltinDoc {
        name: "startswith",
        signature: "str.startswith(prefix: str) -> bool",
        description: "Returns whether the string begins with `prefix`, so \
                      `\"stone\".startswith(\"st\")` is `true`.",
    },
    BuiltinDoc {
        name: "endswith",
        signature: "str.endswith(suffix: str) -> bool",
        description: "Returns whether the string ends with `suffix`, so \
                      `\"main.stn\".endswith(\".stn\")` is `true`.",
    },
    BuiltinDoc {
        name: "is_some",
        signature: "option[T].is_some() -> bool",
        description: "Returns whether the option holds a value, so `some(1).is_some()` is `true` \
                      and `none.is_some()` is `false`.",
    },
    BuiltinDoc {
        name: "is_none",
        signature: "option[T].is_none() -> bool",
        description: "Returns whether the option is `none`.",
    },
    BuiltinDoc {
        name: "unwrap",
        signature: "(option[T] | result[T, E]).unwrap() -> T",
        description: "Returns the value an option or an `ok` holds, so `some(3).unwrap()` and \
                      `ok(3).unwrap()` are 3. It stops the program if the option is `none` or \
                      the result is an `err`, whose error the message shows, as in \
                      `cannot unwrap err: bad`.",
    },
    BuiltinDoc {
        name: "unwrap_or",
        signature: "(option[T] | result[T, E]).unwrap_or(default: T) -> T",
        description: "Returns the value an option or an `ok` holds, or `default` if it is \
                      `none` or an `err`, so `none.unwrap_or(0)` is 0.",
    },
    BuiltinDoc {
        name: "is_ok",
        signature: "result[T, E].is_ok() -> bool",
        description: "Returns whether the result is an `ok`, so `ok(1).is_ok()` is `true`.",
    },
    BuiltinDoc {
        name: "is_err",
        signature: "result[T, E].is_err() -> bool",
        description: "Returns whether the result is an `err`.",
    },
    BuiltinDoc {
        name: "unwrap_err",
        signature: "result[T, E].unwrap_err() -> E",
        description: "Returns the error an `err` holds, so `err(\"bad\").unwrap_err()` is \
                      `\"bad\"`. It stops the program if the result is an `ok`, whose value the \
                      message shows.",
    },
    BuiltinDoc {
        name: "ok",
        signature: "result[T, E].ok() -> option[T]",
        description: "Returns `some` of the value an `ok` holds, or `none` for an `err`, so \
                      `ok(1).ok()` is `some(1)`.",
    },
    BuiltinDoc {
        name: "err",
        signature: "result[T, E].err() -> option[E]",
        description: "Returns `some` of the error an `err` holds, or `none` for an `ok`.",
    },
    BuiltinDoc {
        name: "parse_int",
        signature: "str.parse_int() -> result[int, str]",
        description: "Reads the string as `int` does, returning `ok` of the int, or `err` of \
                      the message `int` would stop the program with, so `\"42\".parse_int()` \
                      is `ok(42)` and `\"x\".parse_int()` is \
                      `err(\"invalid literal for int() with base 10: 'x'\")`.",
    },
    BuiltinDoc {
        name: "parse_float",
        signature: "str.parse_float() -> result[float, str]",
        description: "Reads the string as `float` does, returning `ok` of the float, or `err` \
                      of the message `float` would stop the program with, so \
                      `\"2.5\".parse_float()` is `ok(2.5)`.",
    },
    BuiltinDoc {
        name: "contains",
        signature: "str.contains(text: str) -> bool | bytes.contains(part: bytes) -> bool | \
                    list[T].contains(value: T) -> bool | map[K, V].contains(key: K) -> bool",
        description: "Returns whether the string has `text` in it, the bytes `part`, the list \
                      an element equal to `value`, as `==` compares them, or the map an entry \
                      for `key`, so \
                      `\"hello\".contains(\"ell\")` and `[1, 2].contains(2)` are `true`. \
                      `value in c` makes the same test, and `value not in c` the opposite.",
    },
    BuiltinDoc {
        name: "get",
        signature: "map[K, V].get(key: K) -> option[V]",
        description: "Returns `some` of the value for `key`, or `none` if the map has no entry \
                      for it, so `{\"a\": 1}.get(\"b\")` is `none`. Unlike `m[key]`, it never \
                      stops the program.",
    },
    BuiltinDoc {
        name: "keys",
        signature: "map[K, V].keys() -> list[K]",
        description: "Returns a new list of the map's keys, in the order they were added, so \
                      `{\"b\": 1, \"a\": 2}.keys()` is `[\"b\", \"a\"]`. `for k in m` goes \
                      through them too.",
    },
    BuiltinDoc {
        name: "values",
        signature: "map[K, V].values() -> list[V]",
        description: "Returns a new list of the map's values, in the order their keys were \
                      added.",
    },
    BuiltinDoc {
        name: "items",
        signature: "map[K, V].items() -> list[tuple[K, V]]",
        description: "Returns a new list of the map's entries as `(key, value)` tuples, in the \
                      order their keys were added, so `for k, v in m.items()` goes through \
                      both.",
    },
    BuiltinDoc {
        name: "eq",
        signature: "T.eq(other: T) -> bool",
        description: "Returns whether two values are equal, as `==` does, for any type that \
                      implements `eq`: every type `==` compares, and a struct whose `impl eq` \
                      block says what equal means for it.",
    },
    BuiltinDoc {
        name: "cmp",
        signature: "T.cmp(other: T) -> ordering",
        description: "Returns `less`, `equal`, or `greater` as the value comes before, with, or \
                      after `other`, for any type that implements `ord`: `int`, `float`, `str`, \
                      and a struct with an `impl ord` block. Floats are in the order `sort` \
                      uses, so `-0.0` is less than `0.0` and `nan` is greater than every other \
                      float.",
    },
    BuiltinDoc {
        name: "display",
        signature: "T.display() -> str",
        description: "Returns the text `print` writes for the value, as `str` does, for any \
                      type: every type implements `display`, and a struct whose `impl display` \
                      block says how to write it uses that.",
    },
    BuiltinDoc {
        name: "send",
        signature: "socket.send(text: str) -> result[int, str]",
        description: "Sends all of `text` over the connection, giving the number of bytes sent, \
                      or an error such as `broken pipe` if the other end has closed it.",
    },
    BuiltinDoc {
        name: "read_line",
        signature: "socket.read_line() -> result[option[str], str]",
        description: "Waits for the next line from the connection and gives it without its \
                      `\\n` or `\\r\\n`, or `none` once the other end has closed the connection \
                      and every line has been read. A last line without a newline is still a \
                      line.",
    },
    BuiltinDoc {
        name: "read",
        signature: "socket.read() -> result[str, str]",
        description: "Waits for data from the connection and gives whatever has arrived, up to \
                      64 KiB, or `\"\"` once the other end has closed it. A character split \
                      between two reads comes whole with the second.",
    },
    BuiltinDoc {
        name: "read_all",
        signature: "socket.read_all() -> result[str, str]",
        description: "Reads from the connection until the other end closes it, giving \
                      everything that arrived.",
    },
    BuiltinDoc {
        name: "accept",
        signature: "listener.accept() -> result[socket, str]",
        description: "Waits for the next connection to the listener's port and gives a \
                      socket for it.",
    },
    BuiltinDoc {
        name: "port",
        signature: "listener.port() -> int",
        description: "Returns the port the listener waits on, which is the port the system \
                      picked if `net.listen` was given 0.",
    },
    BuiltinDoc {
        name: "close",
        signature: "(socket | listener).close()",
        description: "Closes the connection or stops listening now, rather than once nothing \
                      refers to it any more. Its methods then give the error `socket is \
                      closed` or `listener is closed`.",
    },
    BuiltinDoc {
        name: "bytes",
        signature: "str.bytes() -> bytes",
        description: "Returns the string's UTF-8 bytes, so `\"hi\".bytes()` is `b\"hi\"`.",
    },
    BuiltinDoc {
        name: "text",
        signature: "bytes.text() -> result[str, str]",
        description: "Returns `ok` of the bytes as a string if they are UTF-8 text without a \
                      null byte, which a string cannot hold, or `err(\"not valid UTF-8 text\")`.",
    },
    BuiltinDoc {
        name: "set_timeout",
        signature: "(socket | listener).set_timeout(seconds: float)",
        description: "Makes each later wait, for data to read, room to send, or a connection \
                      to accept, give up after `seconds` with the error `timed out`. Zero, a \
                      negative number, or nan waits forever, as a new socket or listener does.",
    },
    BuiltinDoc {
        name: "send_bytes",
        signature: "socket.send_bytes(data: bytes) -> result[int, str]",
        description: "Sends all of `data` over the connection, as `send` sends text, giving the \
                      number of bytes sent.",
    },
    BuiltinDoc {
        name: "read_bytes",
        signature: "socket.read_bytes(max: int) -> result[bytes, str]",
        description: "Waits for data from the connection and gives what has arrived, up to \
                      `max` bytes, or `b\"\"` once the other end has closed it (or for a `max` \
                      of 0 or less).",
    },
    BuiltinDoc {
        name: "read_exact",
        signature: "socket.read_exact(n: int) -> result[bytes, str]",
        description: "Waits for exactly `n` bytes from the connection, or gives the error \
                      `connection closed early` if the other end closes it first.",
    },
    BuiltinDoc {
        name: "read_all_bytes",
        signature: "socket.read_all_bytes() -> result[bytes, str]",
        description: "Reads from the connection until the other end closes it, giving every \
                      byte that arrived.",
    },
];

/// Returns whether `name` is a builtin function: one of [`BUILTINS`] or the linked name of a
/// function of a builtin module.
///
/// For example, `is_builtin("print")` and `is_builtin("os.env")` are true, but `is_builtin("env")`
/// is false.
pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name) || module_function(name).is_some()
}

/// Returns whether `name` is taken by the language, so no function, variable, or import may use
/// it: a builtin function or a variant of the prelude.
///
/// For example, `is_reserved("print")` and `is_reserved("some")` are true, but
/// `is_reserved("len")` is false, since methods do not reserve their names.
pub fn is_reserved(name: &str) -> bool {
    BUILTINS.contains(&name) || prelude_variant(name).is_some()
}

/// Returns the documentation for the builtin function named `name`, which may be a function of
/// a builtin module by its linked name.
///
/// For example, `builtin_doc("int")` has the signature `int(value: int | float) -> int`, and
/// `builtin_doc("os.pid")` has `os.pid() -> int`.
pub fn builtin_doc(name: &str) -> Option<&'static BuiltinDoc> {
    BUILTIN_DOCS
        .iter()
        .find(|doc| doc.name == name)
        .or_else(|| module_function(name))
}

/// Returns the documentation for the builtin method named `name`.
///
/// For example, `method_doc("len")` has the signature `(str | list[T]).len() -> int`.
pub fn method_doc(name: &str) -> Option<&'static BuiltinDoc> {
    METHOD_DOCS.iter().find(|doc| doc.name == name)
}

/// Returns a stone file that documents every builtin, for editors to open when asked where a
/// builtin is defined.
///
/// Builtins are part of the interpreter and the compiler rather than written in stone, so the
/// file is only comments, which keeps it valid stone. Each builtin function, then each builtin
/// method, then each function of each builtin module, gets a line holding `// ` and its
/// signature, followed by its description.
pub fn builtins_reference() -> String {
    let mut text = String::from(
        "// stone's builtin functions and methods\n\
         //\n\
         // These are part of the interpreter and the compiler rather than written in stone, so\n\
         // this file only documents them. stone-lsp generates it, and editing it changes\n\
         // nothing.\n",
    );
    for doc in &BUILTIN_DOCS {
        text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
    }
    text += "\n// Methods, called on a value as in `xs.len()`\n";
    for doc in &METHOD_DOCS {
        text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
    }
    for module in &MODULES {
        let name = module.name;
        text += &format!("\n// The {name} module, imported with `use {name}`\n");
        for doc in module.functions {
            text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
        }
    }
    text
}

/// Formats a float the way `print` shows it in both backends, which is how Python's `repr` does.
///
/// The digits are the fewest that read back as the same float. Exponents from -4 through 15 are
/// written out in full with at least one digit after the point, and others use `e` notation with a
/// signed exponent of at least two digits. For example, `format_float(1.0)` returns `"1.0"`,
/// `format_float(0.1 + 0.2)` returns `"0.30000000000000004"`, `format_float(1e16)` returns
/// `"1e+16"`, and `format_float(1e-5)` returns `"1e-05"`.
///
/// Compiled code follows the same steps with the bignums of its float runtime: it counts the
/// shortest digits the way Rust's `{:e}` does, then rounds the exact value to that many digits,
/// half to even.
pub fn format_float(value: f64) -> String {
    if value.is_nan() {
        return "nan".into();
    }
    let sign = if value.is_sign_negative() { "-" } else { "" };
    if value.is_infinite() {
        return format!("{sign}inf");
    }
    // `{:e}` finds how many digits round-trip, and formatting again with that precision breaks
    // ties to even, so 1462468587316101.25 shows as ...101.2, not ...101.3
    let shortest = format!("{:e}", value.abs());
    let count = shortest.split_once('e').unwrap().0.replace('.', "").len();
    let scientific = format!("{:.*e}", count - 1, value.abs());
    let (mantissa, exponent) = scientific.split_once('e').unwrap();
    let digits = mantissa.replace('.', "");
    let exponent: i32 = exponent.parse().unwrap();
    if (-4..16).contains(&exponent) {
        let text = if exponent < 0 {
            format!("0.{}{digits}", "0".repeat((-exponent - 1) as usize))
        } else {
            let whole = exponent as usize + 1;
            if digits.len() <= whole {
                format!("{digits}{}.0", "0".repeat(whole - digits.len()))
            } else {
                format!("{}.{}", &digits[..whole], &digits[whole..])
            }
        };
        format!("{sign}{text}")
    } else {
        let exponent_sign = if exponent < 0 { '-' } else { '+' };
        format!("{sign}{mantissa}e{exponent_sign}{:02}", exponent.abs())
    }
}

/// Raises `base` to a non-negative `exp` by squaring and multiplying, wrapping on overflow like
/// `*` does.
///
/// For example, `int_pow(2, 10)` returns `1024` and `int_pow(2, 64)` wraps around to `0`. Unlike
/// `i64::wrapping_pow`, the exponent can be any `i64`, so `int_pow(1, i64::MAX)` is `1`. Compiled
/// code runs the same loop, and since wrapping multiplication is exact modulo 2^64, it gets the
/// same result.
pub fn int_pow(base: i64, exp: i64) -> i64 {
    let (mut result, mut base, mut exp) = (1i64, base, exp as u64);
    while exp != 0 {
        if exp & 1 == 1 {
            result = result.wrapping_mul(base);
        }
        base = base.wrapping_mul(base);
        exp >>= 1;
    }
    result
}

/// Raises `base` to an int `exp` by squaring and multiplying, then takes the reciprocal for a
/// negative `exp`.
///
/// For example, `float_pow(2.5, 3)` returns `15.625` and `float_pow(2.0, -2)` returns `0.25`.
/// Compiled code does the same float operations in the same order, so the two backends round
/// identically, though the result can differ from a correctly rounded power in the last bits.
/// The reciprocal is IEEE division, so `float_pow(0.0, -1)` is infinity rather than an error.
pub fn float_pow(base: f64, exp: i64) -> f64 {
    let (mut result, mut base, mut magnitude) = (1.0, base, exp.unsigned_abs());
    while magnitude != 0 {
        if magnitude & 1 == 1 {
            result *= base;
        }
        base *= base;
        magnitude >>= 1;
    }
    if exp < 0 { 1.0 / result } else { result }
}

/// The error `input` returns once the input is over.
pub const END_OF_INPUT: &str = "end of input";

/// What `unwrap` stops the program with when the option is `none`.
pub const UNWRAP_NONE: &str = "cannot unwrap none";

/// The start of the error `unwrap` stops the program with on an `err`, which goes on with `: `
/// and the error as `print` writes it.
///
/// For example, `"x".parse_int().unwrap()` stops with
/// `cannot unwrap err: invalid literal for int() with base 10: 'x'`.
pub const UNWRAP_ERR: &str = "cannot unwrap err";

/// The start of the error `unwrap_err` stops the program with on an `ok`, which goes on with `: `
/// and the value as `print` writes it, such as `cannot unwrap_err ok: 12`.
pub const UNWRAP_OK: &str = "cannot unwrap_err ok";

/// The most enum values nested inside one another that `print` writes, in both backends, since
/// printing one inside another takes stack.
///
/// For example, a list of 10,000 `Node`s each holding the next prints, but one of 10,001 stops
/// the program with [`PRINT_DEPTH`].
pub const MAX_PRINT_DEPTH: usize = 10_000;

/// What printing an enum value nested more than [`MAX_PRINT_DEPTH`] deep stops the program with.
pub const PRINT_DEPTH: &str = "value is nested too deeply to print (more than 10000 levels)";

/// What comparing enum values nested more than [`MAX_PRINT_DEPTH`] deep with `==` or `!=` stops
/// the program with, since comparing one inside another takes stack too.
pub const COMPARE_DEPTH: &str = "value is nested too deeply to compare (more than 10000 levels)";

/// The runtime error for a slice whose step is 0, such as `xs[::0]`.
pub const SLICE_STEP_ZERO: &str = "slice step cannot be zero";

/// The runtime error for an index past either end of a list, as in `[1][1]`, with a `{}` for the
/// index as written and one for the length, which [`fill`] fills in.
///
/// For example, `fill(INDEX_RANGE, &[1, 1])` is `list index out of range (index 1, length 1)`.
pub const INDEX_RANGE: &str = "list index out of range (index {}, length {})";

/// The runtime error for arithmetic on lists of different lengths, as in `[1, 2] + [3]`, with a
/// `{}` for the first list's length and one for the first length that differs from it.
///
/// For example, `fill(LENGTHS_DIFFER, &[2, 1])` is `lists have different lengths (2 and 1)`.
pub const LENGTHS_DIFFER: &str = "lists have different lengths ({} and {})";

/// The runtime error for `pop` on an empty list.
pub const POP_EMPTY: &str = "pop from empty list";

/// The runtime error for `pop` at an index past either end of a list, as in `[1].pop(1)`.
pub const POP_RANGE: &str = "pop index out of range";

/// The runtime error for an index past either end of a string, as in `"a"[1]`, with the index
/// and the length filled in as for [`INDEX_RANGE`].
pub const STRING_INDEX_RANGE: &str = "string index out of range (index {}, length {})";

/// Fills each `{}` of a runtime error's `template` with the next of `values`, in order. Compiled
/// code prints the same text, writing the values where the holes are.
///
/// For example, `fill(STRING_INDEX_RANGE, &[-4, 3])` is
/// `string index out of range (index -4, length 3)`.
pub fn fill(template: &str, values: &[i64]) -> String {
    let mut pieces = template.split("{}");
    let mut text = pieces.next().unwrap_or_default().to_string();
    for (piece, value) in pieces.zip(values) {
        text += &format!("{value}{piece}");
    }
    text
}

/// The runtime error for indexing or slicing a string where the result would hold only part of a
/// character, since strings index bytes and a character outside ASCII is several bytes.
pub const CUT_CHARACTER: &str = "cannot cut a string inside a character";

/// Returns the one-byte string at byte `position` of `text`, which must be in range, as `s[i]`
/// does. The byte must be ASCII, since any other byte is part of a longer character.
///
/// For example, `str_at("abc", 1)` returns `"b"`, and `str_at("é", 0)` fails with
/// [`CUT_CHARACTER`]. Compiled code runs the same check in `stone.str_at`.
pub fn str_at(text: &str, position: usize) -> Result<&str, &'static str> {
    match text.as_bytes()[position] {
        byte if byte.is_ascii() => Ok(&text[position..=position]),
        _ => Err(CUT_CHARACTER),
    }
}

/// Returns the bytes of `text` that a slice picks, given the first index, step, and count from
/// [`slice_indices`]. The result must not cut a character: with a step of 1, both ends must fall
/// between characters, and with any other step, every byte picked must be ASCII. An empty result
/// is always fine.
///
/// For example, `"aé b"[1:3]` is `"é"`, while `"aé b"[2:]` and `"aé b"[::-1]` fail with
/// [`CUT_CHARACTER`]. Compiled code runs the same checks in `stone.str_slice_of`.
pub fn str_slice(text: &str, start: i64, step: i64, count: usize) -> Result<String, &'static str> {
    if count == 0 {
        return Ok(String::new());
    }
    if step == 1 {
        let start = start as usize;
        return text
            .get(start..start + count)
            .map(str::to_string)
            .ok_or(CUT_CHARACTER);
    }
    let bytes = text.as_bytes();
    (0..count as i64)
        .map(|k| bytes[(start + k * step) as usize])
        .map(|byte| match byte.is_ascii() {
            true => Ok(byte as char),
            false => Err(CUT_CHARACTER),
        })
        .collect()
}

/// Returns `text` as Python's `repr` writes it, which is how a string prints inside a list: in
/// single quotes, or double quotes if it holds a `'` but no `"`, with backslashes, the quote,
/// line breaks, tabs, and other control characters escaped. Characters outside ASCII print as
/// they are.
///
/// For example, `repr_in_list("it's")` returns `"it's"` in double quotes, and
/// `repr_in_list("a\nb")` returns `'a\nb'` with a backslash and an `n`. Compiled code prints
/// the same through `stone.print_str_quoted`.
pub fn repr_in_list(text: &str) -> String {
    let quote = match text.contains('\'') && !text.contains('"') {
        true => '"',
        false => '\'',
    };
    let mut repr = String::with_capacity(text.len() + 2);
    repr.push(quote);
    for ch in text.chars() {
        match ch {
            '\\' => repr.push_str("\\\\"),
            '\n' => repr.push_str("\\n"),
            '\t' => repr.push_str("\\t"),
            '\r' => repr.push_str("\\r"),
            _ if ch == quote => {
                repr.push('\\');
                repr.push(ch);
            }
            '\0'..='\x1f' | '\x7f' => repr.push_str(&format!("\\x{:02x}", ch as u32)),
            _ => repr.push(ch),
        }
    }
    repr.push(quote);
    repr
}

/// Returns how `print` writes bytes, as Python's `repr` does: `b`, then the bytes in quotes, with
/// a backslash before a backslash or the quote and `\n`, `\t`, `\r`, or `\xhh` for other bytes
/// outside printable ASCII. The quote is `'` unless the bytes hold a `'` and no `"`.
///
/// For example, `repr_bytes(b"a\x00")` returns `b'a\x00'`, and `repr_bytes(b"it's")` returns
/// `b"it's"`. Compiled code prints the same through `stone.print_bytes`.
pub fn repr_bytes(bytes: &[u8]) -> String {
    let quote = match bytes.contains(&b'\'') && !bytes.contains(&b'"') {
        true => b'"',
        false => b'\'',
    };
    let mut repr = String::with_capacity(bytes.len() + 3);
    repr.push('b');
    repr.push(quote as char);
    for &b in bytes {
        match b {
            b'\\' => repr.push_str("\\\\"),
            b'\n' => repr.push_str("\\n"),
            b'\t' => repr.push_str("\\t"),
            b'\r' => repr.push_str("\\r"),
            _ if b == quote => {
                repr.push('\\');
                repr.push(b as char);
            }
            0x20..=0x7e => repr.push(b as char),
            _ => repr.push_str(&format!("\\x{b:02x}")),
        }
    }
    repr.push(quote as char);
    repr
}

/// The runtime error for an index past either end of bytes, as in `b"a"[1]`, with the index and
/// the length filled in as for [`INDEX_RANGE`].
pub const BYTES_INDEX_RANGE: &str = "bytes index out of range (index {}, length {})";

/// The runtime error of `bytes(values)` for a value that is not a byte, with the value filled in.
pub const BYTE_RANGE: &str = "byte out of range (value {})";

/// Returns the `count` bytes of `bytes` a slice takes, from position `start` in steps of `step`,
/// as [`slice_indices`] gives them; unlike a string's, any bytes can be picked.
///
/// For example, `bytes_slice(b"abcd", 0, 2, 2)` is `b"ac"`.
pub fn bytes_slice(bytes: &[u8], start: i64, step: i64, count: usize) -> Vec<u8> {
    (0..count as i64)
        .map(|k| bytes[(start + k * step) as usize])
        .collect()
}

/// Picks the elements a slice such as `xs[start:stop:step]` takes from a list of `len` elements,
/// as Python's `slice.indices` does, returning the first index, the step, and how many elements
/// there are. A missing bound is `None`.
///
/// Negative bounds count from the end, bounds past either end are clamped, and missing ones
/// default by the sign of the step, which is 1 if missing. For example, `[1:4:2]` of 5 elements
/// gives `(1, 2, 2)`, which picks indexes 1 and 3, and `[::-1]` gives `(4, -1, 5)`. Compiled code
/// runs the same steps in `stone.list_slice`.
pub fn slice_indices(
    len: usize,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
) -> Result<(i64, i64, usize), &'static str> {
    // as in Python, a step of MIN acts like -MAX, which has a magnitude
    let step = match step.unwrap_or(1) {
        0 => return Err(SLICE_STEP_ZERO),
        i64::MIN => -i64::MAX,
        step => step,
    };
    let len = len as i64;
    let clamp = |bound: i64| {
        if bound < 0 {
            let bound = bound + len;
            if bound >= 0 {
                bound
            } else if step < 0 {
                -1
            } else {
                0
            }
        } else if bound >= len {
            if step < 0 { len - 1 } else { len }
        } else {
            bound
        }
    };
    let (start, stop) = match step < 0 {
        true => (
            clamp(start.unwrap_or(i64::MAX)),
            clamp(stop.unwrap_or(i64::MIN)),
        ),
        false => (clamp(start.unwrap_or(0)), clamp(stop.unwrap_or(i64::MAX))),
    };
    let count = match step < 0 {
        true if stop < start => (start - stop - 1) / -step + 1,
        false if start < stop => (stop - start - 1) / step + 1,
        _ => 0,
    };
    Ok((start, step, count as usize))
}

/// Returns the key `sort` orders a float by, an unsigned integer whose order is the total order
/// `-inf < ... < -0.0 < 0.0 < ... < inf < nan`.
///
/// Every NaN gets the same key, the largest, since a program cannot tell NaNs apart, so a sorted
/// list is the same whatever algorithm sorted it. For example, `float_key(-0.0) < float_key(0.0)`
/// and `float_key(f64::INFINITY) < float_key(f64::NAN)`. Compiled code computes the same key as
/// `bits ^ ((bits >> 63 arithmetic) | 1 << 63)`.
pub fn float_key(value: f64) -> u64 {
    if value.is_nan() {
        return FLOAT_KEY_NAN;
    }
    let bits = value.to_bits();
    bits ^ (((bits as i64 >> 63) as u64) | 1 << 63)
}

/// The key [`float_key`] gives every NaN, which is the key of the quiet NaN `0x7ff8...`, so
/// compiled code writes that NaN back.
pub const FLOAT_KEY_NAN: u64 = 0xfff8_0000_0000_0000;

/// The characters `strip`, `split()`, `int`, and `float` treat as whitespace, which are the ones
/// C's `isspace` accepts: space, tab, newline, vertical tab, form feed, and carriage return.
pub fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\x0b' | '\x0c' | '\r')
}

/// Returns `text` without whitespace at either end.
///
/// For example, `strip(" a b\n")` returns `"a b"`.
pub fn strip(text: &str) -> &str {
    text.trim_matches(is_space)
}

/// Splits `text` at runs of whitespace, dropping empty pieces, like Python's `str.split()`.
///
/// For example, `split_whitespace(" a  b ")` returns `["a", "b"]`.
pub fn split_whitespace(text: &str) -> Vec<&str> {
    text.split(is_space)
        .filter(|piece| !piece.is_empty())
        .collect()
}

/// Splits `text` at each `separator`, keeping empty pieces, like Python's `str.split(sep)`.
///
/// For example, `split("a,,b", ",")` returns `["a", "", "b"]`, and an empty separator is the
/// error `empty separator`.
pub fn split<'a>(text: &'a str, separator: &str) -> Result<Vec<&'a str>, String> {
    if separator.is_empty() {
        return Err("empty separator".into());
    }
    Ok(text.split(separator).collect())
}

/// Splits `text` into lines at each `\n`, `\r\n`, or `\r`, without their endings, like Python's
/// `str.splitlines()` for those three endings: a final ending ends the last line rather than
/// starting an empty one.
///
/// For example, `splitlines("a\r\nb\rc\n")` returns `["a", "b", "c"]`, `splitlines("\n")`
/// returns `[""]`, and `splitlines("")` returns nothing.
pub fn splitlines(text: &str) -> Vec<&str> {
    let mut lines = Vec::new();
    let mut rest = text;
    while !rest.is_empty() {
        let Some(end) = rest.find(['\n', '\r']) else {
            lines.push(rest);
            break;
        };
        lines.push(&rest[..end]);
        let ending = if rest[end..].starts_with("\r\n") {
            2
        } else {
            1
        };
        rest = &rest[end + ending..];
    }
    lines
}

/// Reads a decimal int the way `int` does: whitespace at either end, an optional sign, then one or
/// more ASCII digits.
///
/// For example, `parse_int(" -42\n")` returns `-42`, `parse_int("1.5")` fails with
/// `invalid literal for int() with base 10: '1.5'`, and a number that does not fit in an int
/// fails with `int() argument out of range: '...'`. Compiled code reads the digits the same way,
/// checking each step for overflow.
pub fn parse_int(text: &str) -> Result<i64, String> {
    let trimmed = strip(text);
    let digits = trimmed.strip_prefix(['+', '-']).unwrap_or(trimmed);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(format!("invalid literal for int() with base 10: '{text}'"));
    }
    trimmed
        .parse()
        .map_err(|_| format!("int() argument out of range: '{text}'"))
}

/// Reads a float the way `float` does: whitespace at either end around a decimal number such as
/// `-1.5e3`, `.5`, or `3.`, or `inf`, `infinity`, or `nan` in any case, each with an optional sign.
///
/// For example, `parse_float(" 2.5\n")` returns `2.5`, `parse_float("1e999")` returns infinity,
/// and `parse_float("0x1p3")` fails with `could not convert string to float: '0x1p3'`. Compiled
/// code checks the same grammar, then rounds the number to the nearest float exactly, as Rust
/// does.
pub fn parse_float(text: &str) -> Result<f64, String> {
    let trimmed = strip(text);
    let unsigned = trimmed.strip_prefix(['+', '-']).unwrap_or(trimmed);
    let named = ["inf", "infinity", "nan"]
        .iter()
        .any(|name| unsigned.eq_ignore_ascii_case(name));
    if !named && !is_decimal(unsigned) {
        return Err(format!("could not convert string to float: '{text}'"));
    }
    Ok(trimmed.parse().expect("the grammar is a subset of Rust's"))
}

/// Returns whether `text` is digits with an optional point and exponent, such as `1.5e-3`, `.5`,
/// or `3.`, with at least one digit before the exponent.
fn is_decimal(text: &str) -> bool {
    let bytes = text.as_bytes();
    let digits = |i: &mut usize| {
        let start = *i;
        while *i < bytes.len() && bytes[*i].is_ascii_digit() {
            *i += 1;
        }
        *i - start
    };
    let mut i = 0;
    let mut count = digits(&mut i);
    if bytes.get(i) == Some(&b'.') {
        i += 1;
        count += digits(&mut i);
    }
    if count == 0 {
        return false;
    }
    if matches!(bytes.get(i), Some(b'e' | b'E')) {
        i += 1;
        if matches!(bytes.get(i), Some(b'+' | b'-')) {
            i += 1;
        }
        if digits(&mut i) == 0 {
            return false;
        }
    }
    i == bytes.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_keys_order_floats_totally() {
        let floats = [
            f64::NEG_INFINITY,
            -1.5,
            -f64::MIN_POSITIVE,
            -0.0,
            0.0,
            f64::MIN_POSITIVE,
            1.5,
            f64::MAX,
            f64::INFINITY,
        ];
        for pair in floats.windows(2) {
            assert!(float_key(pair[0]) < float_key(pair[1]), "{pair:?}");
        }
        // every nan is one value, the largest, whatever its sign
        assert_eq!(float_key(f64::NAN), FLOAT_KEY_NAN);
        assert_eq!(float_key(-f64::NAN), FLOAT_KEY_NAN);
        assert_eq!(float_key(f64::INFINITY - f64::INFINITY), FLOAT_KEY_NAN);
        assert!(float_key(f64::INFINITY) < FLOAT_KEY_NAN);
        // the quiet nan's own key, so compiled code can write it back
        assert_eq!(FLOAT_KEY_NAN, f64::NAN.to_bits() ^ (1 << 63));
    }

    #[test]
    fn fill_writes_each_value_into_its_hole() {
        assert_eq!(
            fill(INDEX_RANGE, &[-3, 2]),
            "list index out of range (index -3, length 2)"
        );
        assert_eq!(
            fill(LENGTHS_DIFFER, &[i64::MIN, 0]),
            "lists have different lengths (-9223372036854775808 and 0)"
        );
        assert_eq!(fill(POP_EMPTY, &[]), POP_EMPTY);
    }

    /// Returns the indexes a slice of a list of `len` elements picks, in order.
    fn sliced(len: usize, start: Option<i64>, stop: Option<i64>, step: Option<i64>) -> Vec<i64> {
        let (first, step, count) = slice_indices(len, start, stop, step).unwrap();
        (0..count as i64).map(|k| first + k * step).collect()
    }

    #[test]
    fn slices_pick_what_python_picks() {
        let (min, max) = (Some(i64::MIN), Some(i64::MAX));
        // each expected list is `list(range(5))[start:stop:step]` in Python
        type Case = (Option<i64>, Option<i64>, Option<i64>, &'static [i64]);
        let cases: [Case; 17] = [
            (None, None, None, &[0, 1, 2, 3, 4]),
            (Some(1), None, None, &[1, 2, 3, 4]),
            (None, Some(2), None, &[0, 1]),
            (Some(-2), None, None, &[3, 4]),
            (None, Some(-1), None, &[0, 1, 2, 3]),
            (None, None, Some(-1), &[4, 3, 2, 1, 0]),
            (Some(3), Some(0), Some(-1), &[3, 2, 1]),
            (Some(1), Some(4), Some(2), &[1, 3]),
            (Some(4), None, Some(-2), &[4, 2, 0]),
            (Some(10), Some(20), None, &[]),
            (Some(-10), Some(2), None, &[0, 1]),
            (None, None, Some(3), &[0, 3]),
            (min, max, None, &[0, 1, 2, 3, 4]),
            (max, min, Some(-1), &[4, 3, 2, 1, 0]),
            (None, None, min, &[4]),
            (Some(2), Some(2), None, &[]),
            (Some(3), Some(1), None, &[]),
        ];
        for (start, stop, step, expected) in cases {
            assert_eq!(
                sliced(5, start, stop, step),
                expected,
                "{start:?}:{stop:?}:{step:?}"
            );
        }
        assert_eq!(sliced(0, None, None, Some(-1)), []);
        assert_eq!(sliced(0, Some(3), Some(-3), None), []);
    }

    /// Slices `text` as `text[start:stop:step]` does.
    fn str_sliced(
        text: &str,
        start: Option<i64>,
        stop: Option<i64>,
        step: Option<i64>,
    ) -> Result<String, &'static str> {
        let (first, step, count) = slice_indices(text.len(), start, stop, step)?;
        str_slice(text, first, step, count)
    }

    #[test]
    fn string_indexes_pick_ascii_bytes() {
        assert_eq!(str_at("abc", 1), Ok("b"));
        assert_eq!(str_at("a\tc", 1), Ok("\t"));
        // "é" is the two bytes c3 a9
        assert_eq!(str_at("é!", 0), Err(CUT_CHARACTER));
        assert_eq!(str_at("é!", 1), Err(CUT_CHARACTER));
        assert_eq!(str_at("é!", 2), Ok("!"));
    }

    #[test]
    fn string_slices_never_cut_a_character() {
        assert_eq!(str_sliced("hello", Some(1), Some(3), None), Ok("el".into()));
        assert_eq!(
            str_sliced("hello", None, None, Some(-1)),
            Ok("olleh".into())
        );
        assert_eq!(str_sliced("hello", None, None, Some(2)), Ok("hlo".into()));
        assert_eq!(str_sliced("hello", Some(9), None, None), Ok("".into()));
        // a step of 1 may hold whole characters
        assert_eq!(str_sliced("aé b", Some(1), Some(3), None), Ok("é".into()));
        assert_eq!(str_sliced("aé b", Some(1), None, None), Ok("é b".into()));
        assert_eq!(str_sliced("aé b", Some(2), None, None), Err(CUT_CHARACTER));
        assert_eq!(str_sliced("aé b", None, Some(2), None), Err(CUT_CHARACTER));
        // an empty slice cuts nothing
        assert_eq!(str_sliced("aé b", Some(2), Some(2), None), Ok("".into()));
        // any other step needs every byte it picks to be ascii
        assert_eq!(str_sliced("aé b", None, None, Some(-1)), Err(CUT_CHARACTER));
        assert_eq!(
            str_sliced("aé b", Some(4), Some(2), Some(-1)),
            Ok("b ".into())
        );
        assert_eq!(str_sliced("aé b", None, None, Some(3)), Ok("a ".into()));
        assert_eq!(str_sliced("aé b", None, None, Some(2)), Err(CUT_CHARACTER));
    }

    #[test]
    fn strings_in_lists_print_like_python_repr() {
        assert_eq!(repr_in_list("abc"), "'abc'");
        assert_eq!(repr_in_list(""), "''");
        assert_eq!(repr_in_list("it's"), "\"it's\"");
        assert_eq!(repr_in_list("say \"hi\""), "'say \"hi\"'");
        assert_eq!(repr_in_list("it's \"x\""), "'it\\'s \"x\"'");
        assert_eq!(repr_in_list("a\nb\tc\rd\\e"), "'a\\nb\\tc\\rd\\\\e'");
        assert_eq!(repr_in_list("\x01\x1b\x7f"), "'\\x01\\x1b\\x7f'");
        assert_eq!(repr_in_list("café"), "'café'");
    }

    #[test]
    fn a_slice_step_cannot_be_zero() {
        assert_eq!(
            slice_indices(3, None, None, Some(0)),
            Err("slice step cannot be zero")
        );
    }

    #[test]
    fn the_prelude_defines_option_ordering_result_its_traits_and_sort() {
        use crate::ast::{Mod, StmtKind};
        use crate::lexer::Lexer;
        use crate::parser::Parser;
        use crate::span::FileId;

        let tokens = Lexer::with_file(PRELUDE, FileId::PRELUDE).lex().unwrap();
        let Mod::Module { body } = Parser::new(&tokens).parse().unwrap();
        let mut enums = vec![];
        let mut structs = vec![];
        let mut traits = vec![];
        let mut functions = vec![];
        for stmt in &body {
            match &stmt.kind {
                StmtKind::EnumDef {
                    name,
                    public,
                    type_params,
                    variants,
                    ..
                } => {
                    assert!(public, "{name}");
                    // the runtime builds options and orderings by tag
                    let shape: Vec<(&str, usize)> = variants
                        .iter()
                        .map(|v| (v.name.as_str(), v.fields.len()))
                        .collect();
                    enums.push((name.as_str(), type_params.len(), shape));
                }
                StmtKind::TraitDef { name, methods, .. } => {
                    let names: Vec<String> = methods
                        .iter()
                        .filter_map(|m| match &m.kind {
                            StmtKind::FunctionDef { name, .. } => Some(name.clone()),
                            _ => None,
                        })
                        .collect();
                    traits.push((name.clone(), names));
                }
                StmtKind::StructDef {
                    name,
                    public,
                    fields,
                    ..
                } => {
                    assert!(public, "{name}");
                    // os.run fills an output by field order
                    let names: Vec<&str> = fields.iter().map(|f| f.name.as_str()).collect();
                    structs.push((name.as_str(), names));
                }
                StmtKind::FunctionDef { name, .. } => functions.push(name.clone()),
                other => panic!("the prelude should hold no {other:?}"),
            }
        }
        assert_eq!(
            enums,
            [
                ("option", 1, vec![("none", 0), ("some", 1)]),
                (
                    "ordering",
                    0,
                    vec![("less", 0), ("equal", 0), ("greater", 0)]
                ),
                ("result", 2, vec![("ok", 1), ("err", 1)]),
            ]
        );
        assert_eq!(structs, [(OUTPUT, vec!["status", "stdout", "stderr"])]);
        assert_eq!(
            traits,
            [
                ("eq".to_string(), vec!["eq".to_string()]),
                ("ord".to_string(), vec!["cmp".to_string()]),
                ("display".to_string(), vec!["display".to_string()]),
            ]
        );
        assert_eq!(
            functions,
            [
                "sort",
                "map",
                "filter",
                "any",
                "all",
                "sort_by_key",
                "min_by_key",
                "max_by_key"
            ]
        );
        // each list method that takes a function is a prelude function of the same name
        for (method, function) in PRELUDE_METHODS {
            assert_eq!(function, format!("prelude.{method}"));
            assert!(functions.iter().any(|f| f == method));
        }
        let variants: Vec<(&str, &str)> = enums
            .iter()
            .flat_map(|(name, _, shape)| shape.iter().map(move |(v, _)| (*v, *name)))
            .collect();
        assert_eq!(variants, PRELUDE_VARIANTS);
    }

    #[test]
    fn prelude_variants_are_reserved() {
        assert!(is_reserved("some"));
        assert!(is_reserved("less"));
        assert!(is_reserved("ok"));
        assert!(is_reserved("err"));
        assert!(is_reserved("print"));
        assert!(!is_reserved("len"));
        assert!(!BUILTINS.contains(&"some"));
    }

    #[test]
    fn every_builtin_is_documented() {
        let documented: Vec<&str> = BUILTIN_DOCS.iter().map(|doc| doc.name).collect();
        assert_eq!(documented, BUILTINS);
        for name in BUILTINS {
            let doc = builtin_doc(name).unwrap();
            assert!(doc.signature.starts_with(&format!("{name}(")), "{name}");
        }
        assert!(builtin_doc("nope").is_none());
    }

    #[test]
    fn every_method_is_documented() {
        let documented: Vec<&str> = METHOD_DOCS.iter().map(|doc| doc.name).collect();
        assert_eq!(documented, METHODS);
        for name in METHODS {
            let doc = method_doc(name).unwrap();
            assert!(doc.signature.contains(&format!(".{name}(")), "{name}");
            // `bytes(values)` makes bytes and `s.bytes()` gives a string's, which hover and the
            // reference tell apart by the dot
            assert!(!BUILTINS.contains(&name) || name == "bytes", "{name}");
        }
        assert!(method_doc("print").is_none());
    }

    #[test]
    fn every_module_function_is_documented_under_its_module() {
        for module in &MODULES {
            assert!(!module.functions.is_empty(), "{}", module.name);
            for doc in module.functions {
                let prefix = format!("{}.", module.name);
                assert!(doc.name.starts_with(&prefix), "{}", doc.name);
                assert!(doc.signature.starts_with(&format!("{}(", doc.name)));
                assert_eq!(module_function(doc.name).unwrap().name, doc.name);
                assert_eq!(builtin_doc(doc.name).unwrap().name, doc.name);
                assert!(is_builtin(doc.name), "{}", doc.name);
            }
        }
        assert!(module_function("os.nope").is_none());
        assert!(module_function("env").is_none());
        assert!(!is_builtin("env"));
    }

    #[test]
    fn int_powers_wrap_like_multiplication() {
        assert_eq!(int_pow(2, 10), 1024);
        assert_eq!(int_pow(0, 0), 1);
        assert_eq!(int_pow(-3, 3), -27);
        assert_eq!(int_pow(3, 50), 3i64.wrapping_pow(50));
        assert_eq!(int_pow(2, 64), 0);
        assert_eq!(int_pow(1, i64::MAX), 1);
        assert_eq!(int_pow(-1, i64::MAX), -1);
    }

    #[test]
    fn float_powers_multiply_in_a_fixed_order() {
        assert_eq!(float_pow(2.5, 3), 15.625);
        assert_eq!(float_pow(2.0, -2), 0.25);
        assert_eq!(float_pow(0.0, -1), f64::INFINITY);
        assert_eq!(float_pow(-0.0, -1), f64::NEG_INFINITY);
        assert_eq!(float_pow(1.0, i64::MIN), 1.0);
        assert_eq!(float_pow(f64::NAN, 0), 1.0);
        assert!(float_pow(f64::NAN, 1).is_nan());
        assert_eq!(float_pow(1.5, 4), 5.0625);
        assert_eq!(float_pow(10.0, 400), f64::INFINITY);
    }

    #[test]
    fn floats_format_like_python_repr() {
        let cases = [
            (1.0, "1.0"),
            (0.1, "0.1"),
            (-2.5, "-2.5"),
            (0.1 + 0.2, "0.30000000000000004"),
            (123456.789, "123456.789"),
            // halfway between ...101.2 and ...101.3, so it rounds to even
            (1462468587316101.0 + 0.25, "1462468587316101.2"),
            (100.0, "100.0"),
            (1e14, "100000000000000.0"),
            (1e15, "1000000000000000.0"),
            (1e16, "1e+16"),
            (1.5e16, "1.5e+16"),
            (1e300, "1e+300"),
            (0.0001, "0.0001"),
            (0.00012, "0.00012"),
            (1e-5, "1e-05"),
            (1.5e-7, "1.5e-07"),
            (5e-324, "5e-324"),
            (f64::MAX, "1.7976931348623157e+308"),
            (0.0, "0.0"),
            (-0.0, "-0.0"),
            (f64::INFINITY, "inf"),
            (f64::NEG_INFINITY, "-inf"),
            (f64::NAN, "nan"),
        ];
        for (value, expected) in cases {
            assert_eq!(format_float(value), expected, "{value:e}");
        }
    }

    #[test]
    fn ints_parse_after_stripping_whitespace() {
        assert_eq!(parse_int("42"), Ok(42));
        assert_eq!(parse_int("  -7\n"), Ok(-7));
        assert_eq!(parse_int("+0012"), Ok(12));
        assert_eq!(parse_int("\t\x0b\x0c\r 5 "), Ok(5));
        assert_eq!(parse_int("-9223372036854775808"), Ok(i64::MIN));
        assert_eq!(parse_int("9223372036854775807"), Ok(i64::MAX));
        for bad in [
            "", " ", "+", "-", "1.5", "1e3", "0x10", "1_000", "--1", "+-1", "1 2", "\u{a0}1",
        ] {
            assert_eq!(
                parse_int(bad),
                Err(format!("invalid literal for int() with base 10: '{bad}'")),
                "{bad:?}"
            );
        }
        for big in [
            "9223372036854775808",
            "-9223372036854775809",
            "99999999999999999999999",
        ] {
            assert_eq!(
                parse_int(big),
                Err(format!("int() argument out of range: '{big}'")),
                "{big:?}"
            );
        }
    }

    #[test]
    fn floats_parse_a_decimal_grammar() {
        let cases = [
            ("1", 1.0),
            ("  2.5\n", 2.5),
            ("-.5", -0.5),
            ("+3.", 3.0),
            ("1e3", 1000.0),
            ("1E-2", 0.01),
            ("6.02e+23", 6.02e23),
            ("1e999", f64::INFINITY),
            ("1e-999", 0.0),
            ("inf", f64::INFINITY),
            ("-Infinity", f64::NEG_INFINITY),
            ("0.1", 0.1),
        ];
        for (text, expected) in cases {
            assert_eq!(parse_float(text), Ok(expected), "{text:?}");
        }
        assert!(parse_float(" NaN ").unwrap().is_nan());
        for bad in [
            "", ".", "e5", "1e", "1e+", "0x1p3", "in", "infinit", "nan(1)", "1.2.3", "1_0", "- 1",
        ] {
            assert_eq!(
                parse_float(bad),
                Err(format!("could not convert string to float: '{bad}'")),
                "{bad:?}"
            );
        }
    }

    #[test]
    fn strip_removes_ascii_whitespace() {
        assert_eq!(strip("  a b\t\n"), "a b");
        assert_eq!(strip("\u{a0}x\u{a0}"), "\u{a0}x\u{a0}");
        assert_eq!(strip(" \r\n"), "");
    }

    #[test]
    fn split_without_a_separator_drops_empty_pieces() {
        assert_eq!(split_whitespace("  a  b\tc\n"), ["a", "b", "c"]);
        assert!(split_whitespace("   ").is_empty());
    }

    #[test]
    fn split_with_a_separator_keeps_empty_pieces() {
        assert_eq!(split("a,,b,", ","), Ok(vec!["a", "", "b", ""]));
        assert_eq!(split("", ","), Ok(vec![""]));
        assert_eq!(split("a::b", "::"), Ok(vec!["a", "b"]));
        assert_eq!(split("aaa", "aa"), Ok(vec!["", "a"]));
        assert_eq!(split("a", ""), Err("empty separator".to_string()));
    }

    #[test]
    fn splitlines_breaks_at_each_ending_and_drops_a_final_empty_line() {
        assert_eq!(splitlines("a\nb\r\nc\rd"), ["a", "b", "c", "d"]);
        assert_eq!(splitlines("a\r\rb"), ["a", "", "b"]);
        assert_eq!(splitlines("a\n\n"), ["a", ""]);
        assert_eq!(splitlines("\n"), [""]);
        assert!(splitlines("").is_empty());
        assert_eq!(splitlines("x\r\n"), ["x"]);
    }

    #[test]
    fn the_reference_is_valid_stone() {
        assert_eq!(crate::driver::check(&builtins_reference()), []);
    }

    #[test]
    fn the_reference_has_a_line_for_each_builtin() {
        let reference = builtins_reference();
        let functions = MODULES.iter().flat_map(|module| module.functions);
        for doc in BUILTIN_DOCS.iter().chain(&METHOD_DOCS).chain(functions) {
            let line = format!("// {}", doc.signature);
            assert_eq!(
                reference.lines().filter(|l| *l == line).count(),
                1,
                "{line}"
            );
        }
    }
}
