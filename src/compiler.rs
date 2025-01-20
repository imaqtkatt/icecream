use std::collections::HashMap;

use crate::{
  loc::{Name, Text},
  parse_tree::Op,
  syntax::ExpandedTree,
};

#[derive(Clone)]
pub enum Value<'c> {
  Unit,
  Number(i32),
  Bool(bool),
  String(Text),
  Primitive(fn(Value<'c>) -> Value<'c>),
  Closure(Name, Compiled<'c>, HashMap<Name, Value<'c>>),
}

impl<'c> std::fmt::Debug for Value<'c> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Unit => write!(f, "Unit"),
      Value::Number(num) => write!(f, "Number({num})"),
      Value::Bool(bool) => write!(f, "Bool({bool})"),
      Value::String(text) => write!(f, "String({text})"),
      Value::Primitive(_) => write!(f, "Primitive(<rust>)"),
      Value::Closure(param, _, _) => write!(f, "Closure({param}, <body>, <env>)"),
    }
  }
}

pub fn new_env<'c>() -> HashMap<Name, Value<'c>> {
  let mut env = HashMap::new();

  env.insert(
    "print".to_string().into(),
    Value::Primitive(|v| {
      match v {
        Value::Unit => println!("unit"),
        Value::Number(num) => println!("{num}"),
        Value::Bool(bool) => println!("{bool}"),
        Value::String(text) => println!("{text}"),
        Value::Primitive(_) => println!("<primitive>"),
        Value::Closure(_, _, _) => println!("<closure>"),
      };
      Value::Unit
    }),
  );

  env
}

#[derive(Clone)]
pub struct Compiled<'c>(std::rc::Rc<dyn 'c + Fn(&mut HashMap<Name, Value<'c>>) -> Value<'c>>);

impl<'c> Compiled<'c> {
  pub fn new(closure: impl 'c + Fn(&mut HashMap<Name, Value<'c>>) -> Value<'c>) -> Self {
    Self(std::rc::Rc::new(closure))
  }

  pub fn run(&self, env: &mut HashMap<Name, Value<'c>>) -> Value<'c> {
    self.0(env)
  }
}

// just to be able to run early

pub fn compile<'c>(tree: &'c ExpandedTree) -> Compiled<'c> {
  match tree {
    crate::syntax::Syntax::Variable(name) => Compiled::new(move |env| {
      dbg!(&env);
      env[dbg!(name)].clone()
    }),

    crate::syntax::Syntax::Literal(literal) => Compiled::new(move |_| match &literal {
      crate::parse_tree::Literal::Number(num) => Value::Number(*num),
      crate::parse_tree::Literal::Boolean(bool) => Value::Bool(*bool),
      crate::parse_tree::Literal::String(text) => Value::String(text.clone()),
    }),

    crate::syntax::Syntax::Call(callee, argument) => {
      let callee = compile(&*callee);
      let argument = compile(&*argument);

      Compiled::new(move |env| match callee.run(env) {
        Value::Primitive(f) => f(argument.run(env)),
        Value::Closure(param, body, env) => {
          let mut env = env.clone();
          let argument = argument.run(&mut env);
          env.insert(param.clone(), argument);
          body.run(&mut env)
        }
        _ => panic!("skill issue"),
      })
    }

    crate::syntax::Syntax::Binary(op, lhs, rhs) => {
      let lhs = compile(&*lhs);
      let rhs = compile(&*rhs);

      Compiled::new(move |env| {
        let lhs = lhs.run(env);
        let rhs = rhs.run(env);
        match (op, lhs, rhs) {
          (Op::Equal, _x, _y) => todo!(),
          (Op::NotEqual, _x, _y) => todo!(),
          (Op::And, Value::Bool(x), Value::Bool(y)) => Value::Bool(x && y),
          (Op::Or, Value::Bool(x), Value::Bool(y)) => Value::Bool(x || y),
          (Op::Add, Value::Number(x), Value::Number(y)) => Value::Number(x + y),
          (Op::Sub, Value::Number(x), Value::Number(y)) => Value::Number(x - y),
          (Op::Mul, Value::Number(x), Value::Number(y)) => Value::Number(x * y),
          (Op::Div, Value::Number(x), Value::Number(y)) => Value::Number(x / y),
          (Op::Rem, Value::Number(x), Value::Number(y)) => Value::Number(x % y),
          _ => todo!(),
        }
      })
    }

    crate::syntax::Syntax::Lambda(bind, value) => {
      let value = compile(&*value);
      Compiled::new(move |env| Value::Closure(bind.clone(), value.clone(), env.clone()))
    }

    crate::syntax::Syntax::Let(bind, value) => {
      let value = compile(&*value);

      Compiled::new(move |env| {
        let value = value.run(env);
        env.insert(bind.clone(), value);
        Value::Unit
      })
    }

    crate::syntax::Syntax::Block(block) => {
      let block = block.into_iter().map(compile).collect::<Vec<_>>();
      Compiled::new(move |env| {
        let mut last = Value::Unit;
        for value in block.iter() {
          last = value.run(env);
        }
        last
      })
    }

    crate::syntax::Syntax::Ext(()) => unreachable!(),
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::parse_from_source, loc::Source, syntax};

  #[test]
  fn compiler_test() {
    let source = Source::from(
      r#"
macro fn name params body =
  let name = params => body

fn snd (x y) { y }

fn fst (x y) { x }

let z = 1 + 2

print (fst 1 3)
"#,
    );
    let program = parse_from_source(source).unwrap();
    let expanded = syntax::program_to_expanded_tree(program).unwrap();
    let compiled = super::compile(&expanded);

    let mut env = super::new_env();
    compiled.run(&mut env);
  }
}
