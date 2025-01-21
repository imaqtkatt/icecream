use std::collections::HashMap;

use crate::{loc::Name, parse_tree};

#[derive(Debug)]
pub enum Syntax<Id = crate::loc::Name, Ext = ()> {
  Variable(Id),
  Literal(parse_tree::Literal),
  Call(Box<Syntax<Id, Ext>>, Box<Syntax<Id, Ext>>),
  Binary(parse_tree::Op, Box<Syntax<Id, Ext>>, Box<Syntax<Id, Ext>>),
  Lambda(Id, Box<Syntax<Id, Ext>>),
  Let(Id, Box<Syntax<Id, Ext>>),
  Block(Vec<Syntax<Id, Ext>>),
  Ext(Ext),
}

pub type ExpandedTree = Syntax<crate::loc::Name, ()>;

pub fn program_to_expanded_tree(program: parse_tree::Program) -> miette::Result<ExpandedTree> {
  let mut macros = HashMap::new();

  let mut syntaxes = vec![];

  for top_level in program.top_levels.into_iter() {
    match top_level {
      parse_tree::TopLevel::Macro(name, parameters, body) => {
        macros.insert(name, Macro::new(parameters, body));
      }
      parse_tree::TopLevel::Tree(tree) => {
        syntaxes.push(tree.to_syntax(&mut macros)?);
      }
    }
  }

  Ok(Syntax::Block(syntaxes))
}

struct Macro {
  parameters: Vec<Name>,
  tree: parse_tree::Tree,
}

impl Macro {
  fn new(parameters: Vec<Name>, tree: parse_tree::Tree) -> Self {
    Self { parameters, tree }
  }
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[diagnostic()]
#[error("macros inside macros are not allowed")]
pub struct NestedMacroError {
  #[label("here")]
  loc: crate::loc::Loc,

  #[source_code]
  src: crate::loc::Source,
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[diagnostic()]
#[error("macro call expects {parameters} parameters")]
pub struct MacroArityError {
  parameters: usize,

  #[label("here")]
  loc: crate::loc::Loc,

  #[source_code]
  src: crate::loc::Source,
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[diagnostic()]
#[error("bind is not a valid name")]
pub struct BindNotValid {
  #[label("here")]
  loc: crate::loc::Loc,

  #[source_code]
  src: crate::loc::Source,
}

fn mangle(name: &Name, bindings: &HashMap<Name, parse_tree::Tree>) -> Name {
  let mut counter = 0;
  let mut base = format!("{name}_{counter}");

  while bindings.contains_key(&base) {
    counter += 1;
    base = format!("{name}_{counter}");
  }

  Name::from(base)
}

impl Macro {
  fn expand(
    tree: parse_tree::Tree,
    bindings: &mut HashMap<Name, parse_tree::Tree>,
  ) -> miette::Result<parse_tree::Tree> {
    match tree.tree_kind {
      parse_tree::TreeKind::Variable(ref name) => match bindings.get(name) {
        Some(tree) => Ok(tree.clone()),
        None => {
          let mangled = mangle(name, bindings);
          let tree_kind = parse_tree::TreeKind::Variable(mangled);
          let tree = parse_tree::Tree::new(tree_kind, tree.loc);
          bindings.insert(name.clone(), tree.clone());
          Ok(tree)
        }
      },
      parse_tree::TreeKind::Literal(_) => Ok(tree),
      parse_tree::TreeKind::Spine(callee, spine) => {
        let callee = Macro::expand(*callee, bindings)?;
        let spine = spine
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Result<_, _>>()?;
        let tree_kind = parse_tree::TreeKind::Spine(callee.into(), spine);
        Ok(parse_tree::Tree::new(tree_kind, tree.loc))
      }
      parse_tree::TreeKind::Binary(op, lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings)?;
        let rhs = Macro::expand(*rhs, bindings)?;
        let tree_kind = parse_tree::TreeKind::Binary(op, lhs.into(), rhs.into());
        Ok(parse_tree::Tree::new(tree_kind, tree.loc))
      }
      parse_tree::TreeKind::Lambda(lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings)?;
        let rhs = Macro::expand(*rhs, bindings)?;
        let tree_kind = parse_tree::TreeKind::Lambda(lhs.into(), rhs.into());
        Ok(parse_tree::Tree::new(tree_kind, tree.loc))
      }
      parse_tree::TreeKind::Let(bind, value) => {
        let bind = Macro::expand(*bind, bindings)?;
        let value = Macro::expand(*value, bindings)?;
        let tree_kind = parse_tree::TreeKind::Let(bind.into(), value.into());
        Ok(parse_tree::Tree::new(tree_kind, tree.loc))
      }
      parse_tree::TreeKind::Block(block) => {
        let block = block
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Result<_, _>>()?;
        let tree_kind = parse_tree::TreeKind::Block(block);
        Ok(parse_tree::Tree::new(tree_kind, tree.loc))
      }
    }
  }
}

trait ToSyntax {
  fn to_syntax(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<ExpandedTree>;
}

impl ToSyntax for parse_tree::Tree {
  fn to_syntax(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<ExpandedTree> {
    use crate::parse_tree::TreeKind;

    match self.tree_kind {
      TreeKind::Variable(name) => match macros.get(&name) {
        Some(r#macro) if r#macro.parameters.len() == 0 => r#macro.tree.clone().to_syntax(macros),
        Some(r#macro) => Err(MacroArityError {
          parameters: r#macro.parameters.len(),
          loc: self.loc.clone(),
          src: self.loc.source.clone(),
        })?,
        None => Ok(Syntax::Variable(name)),
      },

      TreeKind::Spine(callee, arguments) => match callee.tree_kind {
        TreeKind::Variable(ref name) => match macros.get(name) {
          Some(r#macro) if r#macro.parameters.len() == arguments.len() => {
            // TODO: bind the defined macros to their names
            let mut bindings = HashMap::new();
            for (bind, tree) in r#macro.parameters.iter().zip(arguments) {
              bindings.insert(bind.clone(), tree);
            }
            Macro::expand(r#macro.tree.clone(), &mut bindings)?.to_syntax(macros)
          }
          Some(r#macro) => Err(MacroArityError {
            parameters: r#macro.parameters.len(),
            loc: self.loc.clone(),
            src: self.loc.source,
          })?,
          None => {
            let mut callee = callee.to_syntax(macros)?;
            for tree in arguments {
              callee = Syntax::Call(callee.into(), tree.to_syntax(macros)?.into());
            }
            Ok(callee)
          }
        },
        _ => {
          let mut callee = callee.to_syntax(macros)?;
          for tree in arguments {
            callee = Syntax::Call(callee.into(), tree.to_syntax(macros)?.into());
          }
          Ok(callee)
        }
      },
      TreeKind::Binary(op, lhs, rhs) => Ok(Syntax::Binary(
        op,
        lhs.to_syntax(macros)?.into(),
        rhs.to_syntax(macros)?.into(),
      )),
      TreeKind::Lambda(lhs, rhs) => {
        let pat = lhs.to_lambda_params(macros)?;
        let lambda = rhs.to_syntax(macros)?;

        let lambda = pat
          .into_iter()
          .rfold(lambda, |x, y| Syntax::Lambda(y, x.into()));
        Ok(lambda)
      }
      TreeKind::Let(bind, value) => match bind.to_syntax(macros)? {
        Syntax::Variable(name) => Ok(Syntax::Let(name, value.to_syntax(macros)?.into())),
        _ => Err(BindNotValid {
          loc: self.loc.clone(),
          src: self.loc.source,
        })?,
      },
      TreeKind::Block(block) => {
        let block = block
          .into_iter()
          .map(|tree| tree.to_syntax(macros))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Syntax::Block(block))
      }
      TreeKind::Literal(literal) => Ok(Syntax::Literal(literal)),
    }
  }
}

trait ToLambdaParams {
  fn to_lambda_params(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<Vec<Name>>;
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[diagnostic()]
#[error("is not a valid pattern")]
pub struct PatternSyntaxError {
  #[label("this expression")]
  loc: crate::loc::Loc,

  #[source_code]
  src: crate::loc::Source,
}

impl ToLambdaParams for parse_tree::Tree {
  fn to_lambda_params(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<Vec<Name>> {
    match self.tree_kind {
      parse_tree::TreeKind::Variable(name) => match macros.get(&name) {
        Some(r#macro) if r#macro.parameters.len() == 0 => {
          r#macro.tree.clone().to_lambda_params(macros)
        }
        Some(r#macro) => Err(MacroArityError {
          parameters: r#macro.parameters.len(),
          loc: self.loc.clone(),
          src: self.loc.source,
        })?,
        None => Ok(vec![name]),
      },
      parse_tree::TreeKind::Spine(x1, xn) => {
        let trees = std::iter::once(*x1)
          .chain(xn)
          .map(|tree| tree.to_lambda_params(macros))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(trees.concat())
      }
      _ => Err(PatternSyntaxError {
        loc: self.loc.clone(),
        src: self.loc.source,
      })?,
    }
  }
}

#[cfg(test)]
mod test {

  use crate::{lexer, loc::Source, syntax::program_to_expanded_tree};

  #[test]
  fn syntax_test() {
    let src = r#"
macro letfn name params body =
  let name = params => body

letfn snd (x y) { y }

(x => x) 2
"#;
    let source = Source::from(src);
    match lexer::parse_from_source(source).map(program_to_expanded_tree) {
      Ok(program) => println!("{program:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }
}
