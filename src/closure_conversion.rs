use std::collections::HashSet;

use crate::{
  loc::Name,
  syntax::{ExpandedTree, Syntax},
};

pub type ClosureConverted = Syntax<Name, Lambda>;

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[diagnostic()]
#[error("unbound variable '{name}'")]
pub struct UnboundVariable {
  name: Name,
}

#[derive(Debug)]
pub struct Lambda {
  pub name: Name,
  pub captures: Vec<Name>,
  pub body: Box<ClosureConverted>,
}

#[derive(Clone, Debug, Default)]
pub struct Scope {
  outer: Option<Box<Scope>>,
  locals: HashSet<Name>,
  captures: HashSet<Name>,
}

impl Scope {
  pub fn empty() -> Self {
    let mut scope = Scope::default();
    // TODO: find a better way to do this
    scope.define(&Name::from("print".to_string()));

    scope
  }

  fn captures(self) -> HashSet<Name> {
    self.captures
  }

  fn new(outer: &Self) -> Self {
    Self {
      outer: Some(outer.clone().into()),
      locals: Default::default(),
      captures: Default::default(),
    }
  }

  fn define(&mut self, name: &Name) {
    self.locals.insert(name.clone());
  }

  fn resolve(&mut self, name: &Name) -> Option<Name> {
    if let Some(name) = self.locals.get(name).cloned() {
      return Some(name);
    }

    if let Some(name) = self.captures.get(name).cloned() {
      return Some(name);
    }

    if let Some(outer) = &mut self.outer {
      if let Some(index) = outer.resolve(name) {
        self.captures.insert(name.clone());
        return Some(index);
      }
    }

    None
  }
}

pub fn closure_convert(tree: ExpandedTree, scope: &mut Scope) -> miette::Result<ClosureConverted> {
  match tree {
    Syntax::Variable(name) => match scope.resolve(&name) {
      Some(name) => Ok(Syntax::Variable(name)),
      None => Err(UnboundVariable { name })?,
    },
    Syntax::Literal(literal) => Ok(Syntax::Literal(literal)),
    Syntax::Call(callee, argument) => Ok(Syntax::Call(
      closure_convert(*callee, scope)?.into(),
      closure_convert(*argument, scope)?.into(),
    )),
    Syntax::Binary(op, lhs, rhs) => Ok(Syntax::Binary(
      op,
      closure_convert(*lhs, scope)?.into(),
      closure_convert(*rhs, scope)?.into(),
    )),
    Syntax::Lambda(name, body) => {
      let mut scope = Scope::new(scope);
      scope.define(&name);
      let body = closure_convert(*body, &mut scope)?;
      let captures = scope.captures();

      Ok(Syntax::Ext(Lambda {
        name,
        captures: captures.into_iter().collect(),
        body: body.into(),
      }))
    }
    Syntax::Let(bind, value) => {
      // define after value?
      scope.define(&bind);
      let value = closure_convert(*value, scope)?;
      Ok(Syntax::Let(bind, value.into()))
    }
    Syntax::Block(block) => {
      let block = block
        .into_iter()
        .map(|tree| closure_convert(tree, scope))
        .collect::<Result<_, _>>()?;
      Ok(Syntax::Block(block))
    }
    Syntax::Ext(()) => unreachable!(),
  }
}

#[cfg(test)]
mod test {
  use crate::{
    closure_conversion::Scope, lexer::parse_from_source, loc::Source,
    syntax::program_to_expanded_tree,
  };

  #[test]
  fn test_scope() {
    let src = r#"
let x = 1
let x = 2

(f => g => g (f x x))

(x => x)
"#;
    let source = Source::from(src);
    let program = parse_from_source(source).unwrap();
    let tree = program_to_expanded_tree(program).unwrap();
    let mut scope = Scope::empty();
    let closure_converted = super::closure_convert(tree, &mut scope);
    println!("closure_converted = {closure_converted:?}");
    println!("scope = {scope:?}");
  }
}
