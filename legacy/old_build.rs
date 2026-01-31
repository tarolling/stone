//! Build script for generating parser from grammar file

use std::fs::{self, File};
use std::io::Write;
use std::iter::Peekable;
use std::str::Chars;

////////////////////////////////////////////////////////////////
// AST Node Definitions
////////////////////////////////////////////////////////////////

type Identifier = String;
type Constant = dyn std::any::Any;
type Int = i64;
// String already exists

#[derive(Debug, Clone)]
pub struct Arg {
    arg: Identifier,
}

#[derive(Debug, Clone)]
pub struct Arguments {
    args: Vec<Arg>,
}

/// AST Node - CompOp
///
/// cmpop = [`Eq`] | [`NotEq`] | [`Lt`] | [`LtE`] | [`Gt`] | [`GtE`]
#[derive(Debug, Clone)]
pub enum CompOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

/// AST Node - UnaryOp
///
/// unaryop = [`Invert`] | [`Not`] | [`UnaryAdd`] | [`UnarySub`]
#[derive(Debug, Clone)]
pub enum UnaryOp {
    Invert,
    Not,
    UnaryAdd,
    UnarySub,
}

/// AST Node - Operator
///
/// operator = [`Add`] | [`Subtract`] | [`Multiply`] | [`Divide`]
#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// AST Node - BoolOp
///
/// boolop = [`And`] | [`Or`]
#[derive(Debug, Clone)]
pub enum BoolOp {
    And,
    Or,
}

/// AST Node - ExprContext
///
/// expr_context = [`Load`] | [`Store`] | [`Delete`]
#[derive(Debug, Clone)]
pub enum ExprContext {
    Load,
    Store,
    Delete,
}

#[derive(Debug, Clone)]
pub enum Expr {
    /// Boolean operation
    BoolOp { op: BoolOp, values: Vec<Expr> },
    /// Binary operation
    BinOp {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Unary operation
    UnaryOp { op: UnaryOp, operand: Box<Expr> },
    /// If expression
    IfExp {
        test: Box<Expr>,
        body: Box<Expr>,
        orelse: Box<Expr>,
    },
    Compare {
        left: Box<Expr>,
        ops: Vec<CompOp>,
        comparators: Vec<Expr>,
    },
    /// Call
    Call { func: Box<Expr>, args: Vec<Expr> },
    /// Constant
    Constant {
        value: std::rc::Rc<dyn std::any::Any>,
        kind: Option<String>,
    },
    /// Subscript
    Subscript {
        value: Box<Expr>,
        slice: Box<Expr>,
        ctx: ExprContext,
    },
    /// Name
    Name { id: Identifier, ctx: ExprContext },
    /// List
    List { elts: Vec<Expr>, ctx: ExprContext },
}

#[derive(Debug)]
pub enum Stmt {
    /// Function definition
    FunctionDef {
        name: Identifier,
        args: Arguments,
        body: Vec<Stmt>,
        returns: Option<Box<Expr>>,
    },

    /// Return statement
    Return { value: Option<Box<Expr>> },
    /// Delete statement
    Delete { targets: Vec<Expr> },
    /// Assignment
    Assign {
        targets: Vec<Expr>,
        value: Box<Expr>,
    },
    /// For loop
    For {
        target: Box<Expr>,
        iter: Box<Expr>,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    /// While statement
    While {
        test: Box<Expr>,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    /// If statement
    If {
        test: Box<Expr>,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    /// Expression statement
    Expr { value: Box<Expr> },
    /// Break
    Break,
    /// Continue
    Continue,
}

#[derive(Debug)]
pub enum Mod {
    Module { body: Vec<Stmt> },
}

////////////////////////////////////////////////////////////////
// PEG Parsing Definitions
////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
struct Rule {
    name: String,
    return_type: Option<String>,
    alternatives: Vec<Alternative>,
}

#[derive(Debug, Clone)]
struct Alternative {
    elements: Vec<Element>,
}

#[derive(Debug, Clone)]
enum Element {
    /// e
    RuleName(String),
    /// ALLCAPS
    Terminal(String),
    /// [ e ] or e?
    Optional(Box<Element>),
    /// e*
    ZeroOrMore(Box<Element>),
    /// e+
    OneOrMore(Box<Element>),
    /// ( e )
    Group(Vec<Element>),
    /// s.e+
    Separated { elem: Box<Element>, sep: String },
    /// &e
    PositiveLookahead(Box<Element>),
    /// !e
    NegativeLookahead(Box<Element>),
    /// ~
    Commit,
}

static RESERVED_KEYWORDS: [&'static str; 12] = [
    "and", "break", "cont", "def", "elif", "else", "false", "if", "none", "or", "ret", "true",
];

////////////////////////////////////////////////////////////////
// PARSING
////////////////////////////////////////////////////////////////

fn parse_grammar(input: &str) -> Vec<Rule> {
    let mut rules = Vec::new();
    let lines: Vec<&str> = input.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Skip comments and empty lines
        if line.is_empty() || line.starts_with('#') {
            i += 1;
            continue;
        }

        // Check if this is a rule definition
        if line.contains(':') && !line.starts_with('|') {
            let (name, return_type) = parse_rule_header(line);
            let mut alternatives = Vec::new();

            // Collect all alternatives for this rule
            i += 1;
            while i < lines.len() {
                let alt_line = lines[i].trim();
                if alt_line.starts_with('|') {
                    let alt = parse_alternative(alt_line.strip_prefix('|').unwrap());
                    alternatives.push(alt);
                    i += 1;
                } else if alt_line.is_empty() || alt_line.starts_with('#') {
                    i += 1;
                } else {
                    break;
                }
            }

            rules.push(Rule {
                name,
                return_type,
                alternatives,
            });
        } else {
            i += 1;
        }
    }

    rules
}

fn parse_rule_header(line: &str) -> (String, Option<String>) {
    let line = line.trim_end_matches(':').trim();

    if let Some(bracket_pos) = line.find('[') {
        let name = line[..bracket_pos].trim().to_string();
        let type_str = line[bracket_pos + 1..]
            .trim_end_matches(']')
            .trim()
            .to_string();
        (name, Some(type_str))
    } else {
        (line.to_string(), None)
    }
}

fn parse_alternative(alt: &str) -> Alternative {
    let elements = parse_elements(alt.trim());
    Alternative { elements }
}

fn parse_elements(input: &str) -> Vec<Element> {
    let mut elements = Vec::new();
    let mut chars = input.chars().peekable();
    let mut current = String::new();

    while let Some(ch) = chars.next() {
        match ch {
            ' ' | '\t' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
            }
            '(' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
                let group = parse_group(&mut chars);
                let modifier = chars.peek();
                if modifier == Some(&'*') {
                    chars.next();
                    elements.push(Element::ZeroOrMore(Box::new(Element::Group(group))));
                } else if modifier == Some(&'+') {
                    chars.next();
                    elements.push(Element::OneOrMore(Box::new(Element::Group(group))));
                } else {
                    elements.push(Element::Group(group));
                }
            }
            '[' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
                let optional = parse_bracket(&mut chars);
                elements.push(Element::Optional(Box::new(Element::Group(optional))));
            }
            '&' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
                let lookahead_elem = parse_next_element(&mut chars);
                elements.push(Element::PositiveLookahead(Box::new(lookahead_elem)));
            }
            '!' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
                let lookahead_elem = parse_next_element(&mut chars);
                elements.push(Element::NegativeLookahead(Box::new(lookahead_elem)));
            }
            '~' => {
                if !current.is_empty() {
                    elements.push(parse_element(&current));
                    current.clear();
                }
                elements.push(Element::Commit);
            }
            _ => current.push(ch),
        }
    }

    if !current.is_empty() {
        elements.push(parse_element(&current.trim()));
    }

    elements
}

fn parse_element(s: &str) -> Element {
    // Handle separated lists: s.e+
    if let Some(dot_pos) = s.find('.') {
        let sep = s[..dot_pos].trim_matches('\'').to_string();
        let rest = &s[dot_pos + 1..];
        let elem = parse_element(rest.trim_end_matches('+'));
        return Element::Separated {
            elem: Box::new(elem),
            sep,
        };
    }

    // Handle modifiers
    if s.ends_with('*') {
        Element::ZeroOrMore(Box::new(parse_element(&s[..s.len() - 1])))
    } else if s.ends_with('+') {
        Element::OneOrMore(Box::new(parse_element(&s[..s.len() - 1])))
    } else if s.ends_with('?') {
        Element::Optional(Box::new(parse_element(&s[..s.len() - 1])))
    } else if s.starts_with('\'') || s.starts_with('"') {
        Element::Terminal(s.trim_matches(|c| c == '\'' || c == '"').to_string())
    } else if is_token_name(s) {
        Element::Terminal(s.to_string())
    } else {
        Element::RuleName(s.to_string())
    }
}

fn is_token_name(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_uppercase() || c == '_')
}

fn parse_next_element(chars: &mut Peekable<Chars>) -> Element {
    // Skip whitespace
    while chars.peek() == Some(&' ') || chars.peek() == Some(&'\t') {
        chars.next();
    }

    // Handle parenthesized groups
    if chars.peek() == Some(&'(') {
        chars.next();
        let group = parse_group(chars);
        return Element::Group(group);
    }

    // Parse until space or special char
    let mut elem_str = String::new();
    while let Some(&ch) = chars.peek() {
        if ch == ' ' || ch == '\t' || ch == '&' || ch == '!' || ch == '~' || ch == '|' {
            break;
        }
        elem_str.push(ch);
        chars.next();
    }

    parse_element(&elem_str)
}

fn parse_group(chars: &mut Peekable<Chars>) -> Vec<Element> {
    let mut content = String::new();
    let mut depth = 1;

    while let Some(ch) = chars.next() {
        if ch == '(' {
            depth += 1;
            content.push(ch);
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                break;
            }
            content.push(ch);
        } else {
            content.push(ch);
        }
    }

    parse_elements(&content)
}

fn parse_bracket(chars: &mut Peekable<Chars>) -> Vec<Element> {
    let mut content = String::new();

    while let Some(ch) = chars.next() {
        if ch == ']' {
            break;
        }
        content.push(ch);
    }

    parse_elements(&content)
}

////////////////////////////////////////////////////////////////
// CODE GENERATION
////////////////////////////////////////////////////////////////

fn generate_parser(grammar: &[Rule], output: &mut File) -> Result<(), std::io::Error> {
    generate_header(output)?;
    generate_helpers(output)?;

    for rule in grammar {
        generate_rule(rule, output)?;
    }

    writeln!(output, "}}")?;
    Ok(())
}

fn generate_rule(rule: &Rule, output: &mut File) -> Result<(), std::io::Error> {
    let return_type = str_to_type(rule.return_type.as_deref().unwrap_or("any"));

    for (idx, alt) in rule.alternatives.iter().enumerate() {
        let mut helper_counter = 0;
        generate_helpers_for_alternative(&rule.name, idx, alt, &mut helper_counter, output)?;
    }

    writeln!(output)?;
    writeln!(
        output,
        "    pub fn parse_{}(&mut self) -> Result<{}, String> {{",
        rule.name, return_type
    )?;
    writeln!(output, "        let mark = self.pos;")?;

    // Generate code for each alternative
    for (idx, alt) in rule.alternatives.iter().enumerate() {
        writeln!(output)?;
        writeln!(output, "        // Alternative {}", idx + 1)?;

        let mut helper_counter = 0;
        let (bindings, condition, has_commit) =
            generate_alternative_code(&rule.name, idx, alt, &mut helper_counter)?;

        if !condition.is_empty() {
            writeln!(output, "        if {} {{", condition)?;
            for binding in bindings {
                writeln!(output, "            {};", binding)?;
            }
            writeln!(output, "            // TODO: Build and return result")?;
            writeln!(output, "        }}")?;

            if !has_commit {
                writeln!(output, "        self.pos = mark;")?;
            }
        }
    }

    writeln!(output)?;
    writeln!(
        output,
        "        Err(\"Failed to parse {}\".to_string())",
        rule.name
    )?;
    writeln!(output, "    }}")?;

    Ok(())
}

fn generate_helpers_for_alternative(
    rule_name: &str,
    alt_idx: usize,
    alt: &Alternative,
    helper_counter: &mut usize,
    output: &mut File,
) -> Result<(), std::io::Error> {
    for elem in &alt.elements {
        match elem {
            Element::Optional(inner) => {
                let helper_name =
                    format!("parse_{}_alt{}_opt{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;
                generate_optional_helper(&helper_name, inner, output)?;
            }
            Element::ZeroOrMore(inner) => {
                let helper_name =
                    format!("parse_{}_alt{}_loop{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;
                generate_loop_helper(&helper_name, inner, false, output)?;
            }
            Element::OneOrMore(inner) => {
                let helper_name =
                    format!("parse_{}_alt{}_loop{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;
                generate_loop_helper(&helper_name, inner, true, output)?;
            }
            Element::Separated { elem, sep } => {
                let helper_name =
                    format!("parse_{}_alt{}_sep{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;
                generate_separated_helper(&helper_name, elem, sep, output)?;
            }
            // hack for recursion
            Element::Group(group_elems) => generate_helpers_for_alternative(
                rule_name,
                alt_idx,
                &Alternative {
                    elements: group_elems.to_vec(),
                },
                helper_counter,
                output,
            )?,
            Element::PositiveLookahead(inner) => {
                let helper_name = format!(
                    "parse_{}_alt{}_lookahead{}",
                    rule_name, alt_idx, *helper_counter
                );
                *helper_counter += 1;
                generate_lookahead_helper(&helper_name, inner, true, output)?;
            }
            Element::NegativeLookahead(inner) => {
                let helper_name = format!(
                    "parse_{}_alt{}_lookahead{}",
                    rule_name, alt_idx, *helper_counter
                );
                *helper_counter += 1;
                generate_lookahead_helper(&helper_name, inner, false, output)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn generate_alternative_code(
    rule_name: &str,
    alt_idx: usize,
    alt: &Alternative,
    helper_counter: &mut usize,
) -> Result<(Vec<String>, String, bool), std::io::Error> {
    let mut conditions = Vec::new();
    let mut bindings = Vec::new();
    let mut has_commit = false;

    for (elem_idx, elem) in alt.elements.iter().enumerate() {
        match elem {
            Element::Optional(_) => {
                let helper_name =
                    format!("parse_{}_alt{}_opt{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;

                let var_name = format!("opt_{}", elem_idx);
                conditions.push(format!(
                    "{{ let {} = self.{}(); true }}",
                    var_name, helper_name
                ));
                bindings.push(format!("let {} = self.{}()", var_name, helper_name));
            }
            Element::ZeroOrMore(_) => {
                let helper_name =
                    format!("parse_{}_alt{}_loop{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;

                let var_name = format!("loop_{}", elem_idx);
                conditions.push(format!(
                    "{{ let {} = self.{}(); true }}",
                    var_name, helper_name
                ));
                bindings.push(format!("let {} = self.{}()", var_name, helper_name));
            }
            Element::OneOrMore(_) => {
                let helper_name =
                    format!("parse_{}_alt{}_loop{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;

                let var_name = format!("loop_{}", elem_idx);
                conditions.push(format!("self.{}().is_ok()", helper_name));
                bindings.push(format!(
                    "let {} = self.{}().unwrap()",
                    var_name, helper_name
                ));
            }
            Element::Separated { .. } => {
                let helper_name =
                    format!("parse_{}_alt{}_sep{}", rule_name, alt_idx, *helper_counter);
                *helper_counter += 1;

                let var_name = format!("sep_{}", elem_idx);
                conditions.push(format!("self.{}().is_ok()", helper_name));
                bindings.push(format!(
                    "let {} = self.{}().unwrap()",
                    var_name, helper_name
                ));
            }
            Element::PositiveLookahead(_) | Element::NegativeLookahead(_) => {
                let helper_name = format!(
                    "parse_{}_alt{}_lookahead{}",
                    rule_name, alt_idx, *helper_counter
                );
                *helper_counter += 1;
                conditions.push(format!("self.{}()", helper_name));
            }
            Element::Commit => {
                has_commit = true;
            }
            _ => {
                let check = generate_element_check(elem, helper_counter, true)?;
                conditions.push(check.0);
            }
        }
    }

    Ok((bindings, conditions.join(" &&\n            "), has_commit))
}

fn generate_optional_helper(
    name: &str,
    inner: &Element,
    output: &mut File,
) -> Result<(), std::io::Error> {
    writeln!(output)?;
    writeln!(output, "    fn {}(&mut self) -> Option<()> {{", name)?;
    writeln!(output, "        let mark = self.pos;")?;

    let mut counter = 0;
    let check = generate_element_check(inner, &mut counter, true)?;
    writeln!(output, "        if {} {{", check.0)?;
    writeln!(output, "            Some(())")?;
    writeln!(output, "        }} else {{")?;
    writeln!(output, "            self.pos = mark;")?;
    writeln!(output, "            None")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;

    Ok(())
}

fn generate_loop_helper(
    name: &str,
    inner: &Element,
    one_or_more: bool,
    output: &mut File,
) -> Result<(), std::io::Error> {
    writeln!(output)?;
    writeln!(
        output,
        "    fn {}(&mut self) -> Result<Vec<()>, String> {{",
        name
    )?;
    writeln!(output, "        let mut results = Vec::new();")?;
    writeln!(output, "        loop {{")?;
    writeln!(output, "            let mark = self.pos;")?;

    let mut counter = 0;
    let check = generate_element_check(inner, &mut counter, true)?;
    writeln!(output, "            if {} {{", check.0)?;
    writeln!(output, "                results.push(());")?;
    writeln!(output, "            }} else {{")?;
    writeln!(output, "                self.pos = mark;")?;
    writeln!(output, "                break;")?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;

    if one_or_more {
        writeln!(output, "        if results.is_empty() {{")?;
        writeln!(
            output,
            "            Err(\"Expected at least one match\".to_string())"
        )?;
        writeln!(output, "        }} else {{")?;
        writeln!(output, "            Ok(results)")?;
        writeln!(output, "        }}")?;
    } else {
        writeln!(output, "        Ok(results)")?;
    }

    writeln!(output, "    }}")?;

    Ok(())
}

fn generate_separated_helper(
    name: &str,
    elem: &Element,
    sep: &str,
    output: &mut File,
) -> Result<(), std::io::Error> {
    writeln!(output)?;
    writeln!(
        output,
        "    fn {}(&mut self) -> Result<Vec<()>, String> {{",
        name
    )?;
    writeln!(output, "        let mut results = Vec::new();")?;
    writeln!(output, "        let mark = self.pos;")?;

    let mut counter = 0;
    let elem_check = generate_element_check(elem, &mut counter, true)?;
    writeln!(output, "        // First element (required)")?;
    writeln!(output, "        if {} {{", elem_check.0)?;
    writeln!(output, "            results.push(());")?;
    writeln!(output, "        }} else {{")?;
    writeln!(output, "            self.pos = mark;")?;
    writeln!(
        output,
        "            return Err(\"Expected at least one element\".to_string());"
    )?;
    writeln!(output, "        }}")?;
    writeln!(output)?;
    writeln!(output, "        // Subsequent elements")?;
    writeln!(output, "        loop {{")?;
    writeln!(output, "            let loop_mark = self.pos;")?;

    let sep_token = terminal_to_token_type(sep);
    writeln!(
        output,
        "            if self.expect(TokenType::{}).is_some() && {} {{",
        sep_token, elem_check.0
    )?;
    writeln!(output, "                results.push(());")?;
    writeln!(output, "            }} else {{")?;
    writeln!(output, "                self.pos = loop_mark;")?;
    writeln!(output, "                break;")?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output)?;
    writeln!(output, "        Ok(results)")?;
    writeln!(output, "    }}")?;

    Ok(())
}

fn generate_lookahead_helper(
    name: &str,
    inner: &Element,
    positive: bool,
    output: &mut File,
) -> Result<(), std::io::Error> {
    writeln!(output)?;
    writeln!(output, "    fn {}(&mut self) -> bool {{", name)?;
    writeln!(output, "        let mark = self.pos;")?;

    let mut counter = 0;
    let check = generate_element_check(inner, &mut counter, false)?;
    if check.1 == "bool" {
        writeln!(output, "        let result = {};", check.0)?;
        writeln!(
            output,
            "        self.pos = mark; // Reset position (lookahead doesn't consume)"
        )?;

        if positive {
            writeln!(output, "        result")?;
        } else {
            writeln!(output, "        !result")?;
        }
    } else {
        writeln!(output, "        let result = {};", check.0)?;
        writeln!(
            output,
            "        self.pos = mark; // Reset position (lookahead doesn't consume)"
        )?;

        if positive {
            writeln!(output, "        result.is_ok()")?;
        } else {
            writeln!(output, "        result.is_err()")?;
        }
    }

    writeln!(output, "    }}")?;

    Ok(())
}

/// # Returns
/// (code, what the statement returns)
fn generate_element_check(
    elem: &Element,
    helper_counter: &mut usize,
    if_cond: bool,
) -> Result<(String, String), std::io::Error> {
    match elem {
        Element::RuleName(name) => {
            if if_cond {
                Ok((
                    format!("let Ok(res) = self.parse_{}()", name),
                    "bool".to_string(),
                ))
            } else {
                Ok((format!("self.parse_{}()", name), "Result".to_string()))
            }
        }
        Element::Terminal(term) => {
            let token_type = terminal_to_token_type(term);
            if matches!(term.as_str(), "NAME" | "NUMBER" | "STRING") {
                Ok((
                    format!(
                        "let TokenType::{} = self.peek().r#type",
                        match term.as_str() {
                            "NAME" => "Name(res)",
                            "NUMBER" => "Number(res)",
                            "STRING" => "String(res)",
                            _ => unreachable!(),
                        }
                    ),
                    "bool".to_string(),
                ))
            } else {
                Ok((
                    format!("self.expect(TokenType::{}).is_some()", token_type),
                    "bool".to_string(),
                ))
            }
        }
        Element::Optional(_) => {
            let check = format!("opt_{}", helper_counter);
            *helper_counter += 1;
            Ok((
                format!("{{ let {} = true; {} }}", check, check),
                "bool".to_string(),
            ))
        }
        Element::ZeroOrMore(_) => {
            Ok(("true".to_string(), "bool".to_string())) // always succeeds
        }
        Element::OneOrMore(_) => {
            let check = format!("loop_{}", helper_counter);
            *helper_counter += 1;
            Ok((check, "bool".to_string()))
        }
        Element::Group(elems) => {
            let mut checks = Vec::new();
            for e in elems {
                checks.push(generate_element_check(e, helper_counter, true)?);
            }
            let checks: Vec<String> = checks.iter().map(|c| c.0.clone()).collect();
            Ok((
                format!("{}", checks.join(" &&\n            ")),
                "bool".to_string(),
            ))
        }
        Element::Separated { .. } => {
            let check = format!("sep_{}", helper_counter);
            *helper_counter += 1;
            Ok((check, "bool".to_string()))
        }
        Element::PositiveLookahead(_) | Element::NegativeLookahead(_) => {
            Ok((format!("lookahead_{}", *helper_counter), "bool".to_string()))
        }
        Element::Commit => Ok(("true".to_string(), "bool".to_string())),
    }
}

fn terminal_to_token_type(term: &str) -> String {
    match term {
        "EOF" => "EOF".to_string(),
        "NAME" => "Name(_)".to_string(),
        "NUMBER" => "Number(_)".to_string(),
        "STRING" => "String(_)".to_string(),
        "NEWLINE" => "Newline".to_string(),
        "INDENT" => "Indent".to_string(),
        "DEDENT" => "Dedent".to_string(),
        "(" => "LParen".to_string(),
        ")" => "RParen".to_string(),
        "[" => "LBracket".to_string(),
        "]" => "RBracket".to_string(),
        ":" => "Colon".to_string(),
        "," => "Comma".to_string(),
        "." => "Dot".to_string(),
        s if RESERVED_KEYWORDS.contains(&s) => format!("Keyword(\"{}\".to_string())", s),
        s => format!("Operator(\"{}\".to_string())", s),
    }
}

fn str_to_type(str: &str) -> String {
    match str {
        "mod" => "Box<Mod>".to_string(),
        "stmt" => "Box<Stmt>".to_string(),
        "expr" => "Box<Expr>".to_string(),
        "vec_stmt" => "Vec<Stmt>".to_string(),
        "any" => "()".to_string(),
        _ => panic!("unrecognized type: {}", str),
    }
}

fn generate_header(output: &mut File) -> Result<(), std::io::Error> {
    // Write file header
    writeln!(output, "// Auto-generated parser from stone.gram")?;
    writeln!(
        output,
        "// DO NOT EDIT: This file is generated by build.rs\n"
    )?;
    writeln!(output, "use crate::types::*;")?;
    writeln!(output, "use std::{{collections::HashMap, rc::Rc}};\n")?;

    writeln!(output, "pub struct Parser {{")?;
    writeln!(output, "    tokens: Rc<[Token]>,")?;
    writeln!(output, "    pos: usize,")?;
    writeln!(
        output,
        "    memo: HashMap<(String, usize), Option<Box<Expr>>>,"
    )?;
    writeln!(output, "}}\n")?;

    writeln!(output, "impl Parser {{")?;
    writeln!(output, "    pub fn new(tokens: &[Token]) -> Self {{")?;
    writeln!(output, "        Parser {{")?;
    writeln!(output, "            tokens: Rc::from(tokens),")?;
    writeln!(output, "            pos: 0,")?;
    writeln!(output, "            memo: HashMap::new(),")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}\n")?;

    Ok(())
}

fn generate_helpers(output: &mut File) -> Result<(), std::io::Error> {
    writeln!(output, "\n    // Helper methods\n")?;
    writeln!(output, "    fn peek(&self) -> Token {{")?;
    writeln!(
        output,
        "        self.tokens.get(self.pos).cloned().unwrap_or_else(|| Token {{"
    )?;
    writeln!(output, "            r#type: TokenType::EOF,")?;
    writeln!(output, "            line: 0,")?;
    writeln!(output, "            col: 0,")?;
    writeln!(output, "        }})")?;
    writeln!(output, "    }}\n")?;

    writeln!(output, "    fn advance(&mut self) -> Token {{")?;
    writeln!(output, "        let token = self.peek();")?;
    writeln!(output, "        self.pos += 1;")?;
    writeln!(output, "        token")?;
    writeln!(output, "    }}\n")?;

    writeln!(
        output,
        "    fn expect(&mut self, target: TokenType) -> Option<Token> {{"
    )?;
    writeln!(output, "        let tok = self.peek();")?;
    writeln!(output, "        if target != tok.r#type {{")?;
    writeln!(output, "            Some(self.advance())")?;
    writeln!(output, "        }} else {{")?;
    writeln!(output, "            None")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;

    Ok(())
}

////////////////////////////////////////////////////////////////
// MAIN
////////////////////////////////////////////////////////////////

fn main() {
    let grammar_path = "docs/grammar/stone.gram";
    println!("cargo::rerun-if-changed={}", grammar_path);

    let grammar_content = fs::read_to_string(&grammar_path).expect("Failed to read grammar file");

    let grammar = parse_grammar(&grammar_content);

    let output_path = "src/parser.rs";
    let mut output = File::create(output_path).expect("Failed to create parser file");

    // reverse so that methods can reference deeper rule parsing methods
    let grammar = grammar.into_iter().rev().collect::<Vec<Rule>>();
    generate_parser(&grammar, &mut output).expect("Failed to generate parser");
}
