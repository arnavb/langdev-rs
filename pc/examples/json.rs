/// Build a representation of a JSON document that can parse a document into an abstract syntax
/// tree and then recreate the source exactly.
use std::collections::HashMap;

use pc::trait_based::{character, Parser};

struct Json {
    root: Value,
}

enum Value {
    Object(Object),
    Array(Array),
    String(String),
    Number(Number),
    /// Covers true/false in the value McKeeman Form
    Boolean(bool),
    Null,
}

struct Object {
    members: HashMap<String, Value>,
}

struct Array {
    elements: Vec<Value>,
}

enum Number {}

enum Whitespace {}

fn main() {
    println!("Hello, World\n");
}
