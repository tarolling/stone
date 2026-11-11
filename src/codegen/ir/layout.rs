//! How compiled code lays out the values of each enum type: boxed, as one counted pointer to an
//! object holding the tag and the variant's values, or flat, as several values kept apart like a
//! tuple's, so a value lives in registers and making one allocates nothing.
//!
//! An enum type is decided once its type arguments are known, so `option[int]` and
//! `option[Node]` are decided apart. It is boxed when any of these holds:
//!
//! - it holds itself directly, through its variants' values, tuples, structs, and other enums,
//!   but not through lists, maps, functions, or handles, since a flat value of it would never
//!   end, as `Tree` holding `Node(Tree, Tree)` or `option[Node]` for a struct `Node` holding
//!   `next: option[Node]`;
//! - it would take more than [`MAX_FLAT_SLOTS`] values flat;
//! - some value the program holds inside another (a list's element, a map entry's value, a boxed
//!   enum's variant, or a closure's copies, [`crate::checker::Analysis::held_types`]) would hold
//!   more than [`MAX_ELEMENT_VALUES`] values with it flat.
//!
//! A flat enum's values are its tag, an int, then slots grouped by how they are counted: values
//! that are not, then strings, then lists, then counted objects. Each group holds as many slots
//! as the variant that needs the most of them, and each variant's values fill the slots of their
//! group in order. Slots a variant does not use hold 0, so a counted one is null.
//!
//! For example, `option[str]` is `[tag, str]`, `result[int, str]` is `[tag, int, str]`, and
//! `ordering` is `[tag]` alone.

use crate::checker::{Enums, Held, Structs, Type, variants};
use crate::codegen::ir::RcKind;
use crate::stdlib::MAX_ELEMENT_VALUES;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// The most values a flat enum takes, its tag included; a wider one is boxed.
pub const MAX_FLAT_SLOTS: usize = 8;

/// How the values of one enum type are laid out.
#[derive(Debug, Clone, PartialEq)]
pub enum EnumLayout {
    /// One counted pointer to an object holding the tag and the variant's values.
    Boxed,
    Flat(FlatLayout),
}

/// The values of a flat enum: a type for each, the tag's `int` first, standing for every type
/// the slot holds (they are all counted alike), and for each variant, by tag, which of them hold
/// its values, in order.
///
/// For example, `result[int, str]` has the leaves `int`, `int`, `str`, and its variants `ok` and
/// `err` hold leaves `[1]` and `[2]`.
#[derive(Debug, Clone, PartialEq)]
pub struct FlatLayout {
    pub leaves: Vec<Type>,
    pub variant_slots: Vec<Vec<usize>>,
}

/// The groups a flat enum's slots fall into, in order.
const GROUPS: [Option<RcKind>; 4] = [
    None,
    Some(RcKind::Str),
    Some(RcKind::List),
    Some(RcKind::Enum),
];

/// The layout of every enum type of a program, decided the first time it is asked for.
pub struct Layouts {
    enums: Enums,
    structs: Structs,
    /// The enum types boxed because a value holding them flat would be too wide, by
    /// [`key`].
    forced: HashSet<String>,
    cache: RefCell<HashMap<String, Rc<EnumLayout>>>,
}

/// Returns the key that tells `ty` apart from every other type.
fn key(ty: &Type) -> String {
    format!("{ty:?}")
}

impl Layouts {
    /// Decides the layouts of a program with the enums `enums` and structs `structs`, which
    /// holds the types `held` inside other values.
    pub fn new(enums: &Enums, structs: &Structs, held: &[(Type, Held)]) -> Layouts {
        let mut layouts = Layouts {
            enums: enums.clone(),
            structs: structs.clone(),
            forced: HashSet::new(),
            cache: RefCell::new(HashMap::new()),
        };
        // boxing an enum only narrows what holds it, so this ends with every width in bounds
        loop {
            let mut boxed = vec![];
            for (ty, how) in held {
                for width in layouts.held_widths(ty, *how) {
                    if width.0 > MAX_ELEMENT_VALUES {
                        layouts.flat_inside(&width.1, &mut boxed);
                    }
                }
            }
            let before = layouts.forced.len();
            layouts.forced.extend(boxed.iter().map(key));
            if layouts.forced.len() == before {
                break;
            }
            layouts.cache.borrow_mut().clear();
        }
        layouts
    }

    /// Returns the number of values each thing `ty`, held as `how`, stores, with the types
    /// stored: an element, an entry's value after its key, each variant's values for a boxed
    /// enum, or the copies.
    fn held_widths(&self, ty: &Type, how: Held) -> Vec<(usize, Vec<Type>)> {
        match how {
            Held::Element | Held::Captures | Held::TraitValue => {
                vec![(self.leaves(ty).len(), vec![ty.clone()])]
            }
            Held::Entry => vec![(1 + self.leaves(ty).len(), vec![ty.clone()])],
            Held::Variants => match *self.of(ty) {
                EnumLayout::Boxed => variants(&self.enums, ty)
                    .into_iter()
                    .map(|(_, fields)| {
                        let width = fields.iter().map(|f| self.leaves(f).len()).sum();
                        (width, fields)
                    })
                    .collect(),
                EnumLayout::Flat(_) => vec![],
            },
        }
    }

    /// Pushes every flat enum type a value of one of `types` holds as values of its own.
    fn flat_inside(&self, types: &[Type], out: &mut Vec<Type>) {
        for ty in types {
            match ty {
                Type::Tuple(fields) => self.flat_inside(fields, out),
                Type::Struct { .. } => {
                    let fields: Vec<Type> = ty
                        .struct_fields(&self.structs)
                        .into_iter()
                        .map(|(_, f)| f)
                        .collect();
                    self.flat_inside(&fields, out);
                }
                Type::Enum { .. } if matches!(*self.of(ty), EnumLayout::Flat(_)) => {
                    out.push(ty.clone());
                    for (_, fields) in variants(&self.enums, ty) {
                        self.flat_inside(&fields, out);
                    }
                }
                _ => {}
            }
        }
    }

    /// Returns the layout of the enum type `ty`, which has no type parameters. Any other type is
    /// boxed, as far as this goes.
    pub fn of(&self, ty: &Type) -> Rc<EnumLayout> {
        let Type::Enum { .. } = ty else {
            return Rc::new(EnumLayout::Boxed);
        };
        let key = key(ty);
        if let Some(layout) = self.cache.borrow().get(&key) {
            return layout.clone();
        }
        let layout = Rc::new(self.decide(ty));
        self.cache.borrow_mut().insert(key, layout.clone());
        layout
    }

    /// Decides the layout of the enum type `ty`.
    fn decide(&self, ty: &Type) -> EnumLayout {
        if self.forced.contains(&key(ty))
            || ty.has_params()
            || self.reaches(ty, &key(ty), &mut HashSet::new())
        {
            return EnumLayout::Boxed;
        }
        // each variant's values, each with how it is counted
        let payloads: Vec<Vec<Type>> = variants(&self.enums, ty)
            .into_iter()
            .map(|(_, fields)| fields.iter().flat_map(|f| self.leaves(f)).collect())
            .collect();
        let mut leaves = vec![Type::Int];
        let mut starts = vec![];
        for group in GROUPS {
            starts.push(leaves.len());
            let count = payloads
                .iter()
                .map(|p| p.iter().filter(|t| RcKind::of(t) == group).count())
                .max()
                .unwrap_or(0);
            // the first variant that holds a slot names its type
            for k in 0..count {
                let found = payloads
                    .iter()
                    .find_map(|p| p.iter().filter(|t| RcKind::of(t) == group).nth(k));
                leaves.push(found.cloned().unwrap_or(Type::Int));
            }
        }
        if leaves.len() > MAX_FLAT_SLOTS {
            return EnumLayout::Boxed;
        }
        let variant_slots = payloads
            .iter()
            .map(|payload| {
                let mut next = starts.clone();
                payload
                    .iter()
                    .map(|t| {
                        let group = GROUPS.iter().position(|g| *g == RcKind::of(t)).unwrap_or(0);
                        next[group] += 1;
                        next[group] - 1
                    })
                    .collect()
            })
            .collect();
        EnumLayout::Flat(FlatLayout {
            leaves,
            variant_slots,
        })
    }

    /// Returns whether a value of type `ty` holds a value of the enum type keyed `target`
    /// directly, through variants, tuples, and structs. `seen` holds the types already looked
    /// through.
    fn reaches(&self, ty: &Type, target: &str, seen: &mut HashSet<String>) -> bool {
        let fields: Vec<Type> = match ty {
            Type::Tuple(fields) => fields.clone(),
            Type::Struct { .. } => ty
                .struct_fields(&self.structs)
                .into_iter()
                .map(|(_, f)| f)
                .collect(),
            Type::Enum { .. } => variants(&self.enums, ty)
                .into_iter()
                .flat_map(|(_, fields)| fields)
                .collect(),
            _ => return false,
        };
        if !seen.insert(key(ty)) {
            return false;
        }
        fields
            .iter()
            .any(|f| key(f) == target || self.reaches(f, target, seen))
    }

    /// Returns the flat layout of `ty`, if it is an enum type laid out flat.
    pub fn flat(&self, ty: &Type) -> Option<FlatLayout> {
        match &*self.of(ty) {
            EnumLayout::Flat(layout) => Some(layout.clone()),
            EnumLayout::Boxed => None,
        }
    }

    /// Returns whether a value of type `ty` is several values kept apart: a tuple, a struct, or
    /// a flat enum.
    pub fn is_flat(&self, ty: &Type) -> bool {
        match ty {
            Type::Tuple(_) | Type::Struct { .. } => true,
            Type::Enum { .. } => self.flat(ty).is_some(),
            _ => false,
        }
    }

    /// Returns the values a value of type `ty` is made of, as [`Type::leaves`] does, with each
    /// flat enum's values in its place.
    ///
    /// For example, `tuple[int, option[str]]` has the leaves `int`, then `int` and `str`.
    pub fn leaves(&self, ty: &Type) -> Vec<Type> {
        match ty {
            Type::Tuple(fields) => fields.iter().flat_map(|f| self.leaves(f)).collect(),
            Type::Struct { .. } => ty
                .struct_fields(&self.structs)
                .iter()
                .flat_map(|(_, f)| self.leaves(f))
                .collect(),
            Type::Enum { .. } => match self.flat(ty) {
                Some(layout) => layout.leaves,
                None => vec![ty.clone()],
            },
            other => vec![other.clone()],
        }
    }

    /// Returns the positions among [`Layouts::leaves`] of field `k` of the tuple type `ty`.
    pub fn field_leaves(&self, ty: &Type, k: usize) -> std::ops::Range<usize> {
        let Type::Tuple(fields) = ty else {
            return 0..1;
        };
        let start: usize = fields[..k].iter().map(|f| self.leaves(f).len()).sum();
        start..start + self.leaves(&fields[k]).len()
    }

    /// Returns the positions among [`Layouts::leaves`] of the field `name` of the struct type
    /// `ty`, and the field's type, or `None` if it has no such field.
    pub fn struct_field(&self, ty: &Type, name: &str) -> Option<(std::ops::Range<usize>, Type)> {
        let mut start = 0;
        for (field, field_ty) in ty.struct_fields(&self.structs) {
            let width = self.leaves(&field_ty).len();
            if field == name {
                return Some((start..start + width, field_ty));
            }
            start += width;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::{EnumDef, StructDef};

    fn option(t: Type) -> Type {
        Type::Enum {
            name: "option".to_string(),
            args: vec![t],
        }
    }

    fn program() -> (Enums, Structs) {
        let mut enums = Enums::new();
        let param = Type::Param("T".to_string());
        enums.insert(
            "option".to_string(),
            EnumDef {
                params: vec!["T".to_string()],
                variants: vec![
                    ("none".to_string(), vec![]),
                    ("some".to_string(), vec![param]),
                ],
                names: vec![None, None],
            },
        );
        let tree = Type::Enum {
            name: "Tree".to_string(),
            args: vec![],
        };
        enums.insert(
            "Tree".to_string(),
            EnumDef {
                params: vec![],
                variants: vec![
                    ("Leaf".to_string(), vec![Type::Int]),
                    ("Node".to_string(), vec![tree.clone(), tree]),
                ],
                names: vec![None, None],
            },
        );
        let mut structs = Structs::new();
        let node = Type::Struct {
            name: "Node".to_string(),
            args: vec![],
        };
        structs.insert(
            "Node".to_string(),
            StructDef {
                params: vec![],
                fields: vec![
                    ("value".to_string(), Type::Int),
                    ("next".to_string(), option(node)),
                ],
            },
        );
        (enums, structs)
    }

    #[test]
    fn an_option_of_a_value_is_flat_with_its_slots_grouped() {
        let (enums, structs) = program();
        let layouts = Layouts::new(&enums, &structs, &[]);
        assert_eq!(
            layouts.leaves(&option(Type::Str)),
            vec![Type::Int, Type::Str]
        );
        let pair = option(Type::Tuple(vec![Type::Str, Type::Float, Type::Str]));
        assert_eq!(
            layouts.flat(&pair).unwrap(),
            FlatLayout {
                leaves: vec![Type::Int, Type::Float, Type::Str, Type::Str],
                variant_slots: vec![vec![], vec![2, 1, 3]],
            }
        );
        // nested options are flat inside each other
        assert_eq!(layouts.leaves(&option(option(Type::Int))).len(), 3);
    }

    #[test]
    fn an_enum_that_holds_itself_is_boxed() {
        let (enums, structs) = program();
        let layouts = Layouts::new(&enums, &structs, &[]);
        let tree = Type::Enum {
            name: "Tree".to_string(),
            args: vec![],
        };
        assert_eq!(*layouts.of(&tree), EnumLayout::Boxed);
        // an option of it holds it, but not itself
        assert_eq!(layouts.leaves(&option(tree.clone())), vec![Type::Int, tree]);
        let node = Type::Struct {
            name: "Node".to_string(),
            args: vec![],
        };
        assert_eq!(*layouts.of(&option(node)), EnumLayout::Boxed);
    }

    #[test]
    fn a_wide_enum_and_one_that_would_overflow_what_holds_it_are_boxed() {
        let (enums, structs) = program();
        let wide = option(Type::Tuple(vec![Type::Int; MAX_FLAT_SLOTS]));
        let layouts = Layouts::new(&enums, &structs, &[]);
        assert_eq!(*layouts.of(&wide), EnumLayout::Boxed);
        // 31 ints and an option fit a list element only with the option boxed
        let mut fields = vec![Type::Int; 31];
        fields.push(option(Type::Int));
        let element = Type::Tuple(fields);
        let held = [(element.clone(), Held::Element)];
        let layouts = Layouts::new(&enums, &structs, &held);
        assert_eq!(*layouts.of(&option(Type::Int)), EnumLayout::Boxed);
        assert_eq!(layouts.leaves(&element).len(), 32);
    }
}
