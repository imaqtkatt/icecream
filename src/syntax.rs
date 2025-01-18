use std::collections::HashMap;

use crate::loc::Name;

#[derive(Debug)]
pub enum Syntax<Ext = ()> {
  Variable(crate::loc::Name),
  Literal(crate::parse_tree::Literal),
  Call(Box<Syntax<Ext>>, Box<Syntax<Ext>>),
  Binary(crate::parse_tree::Op, Box<Syntax<Ext>>, Box<Syntax<Ext>>),
  Lambda(LambdaPattern, Box<Syntax<Ext>>),
  Let(crate::loc::Name, Box<Syntax<Ext>>),
  Block(Vec<Syntax<Ext>>),
  Ext(Ext),
}

#[derive(Debug)]
pub enum LambdaPattern {
  Variable(crate::loc::Name),
  Many(Vec<LambdaPattern>),
}

type ExpandedTree = Syntax<MacroDefined>;

pub fn program_to_expanded_tree(
  program: crate::parse_tree::Program,
) -> miette::Result<ExpandedTree> {
  let mut macros = HashMap::new();

  let syntaxes = (program.trees)
    .into_iter()
    .map(|tree| tree.to_syntax(&mut macros))
    .collect::<Result<_, _>>()?;

  Ok(Syntax::Block(syntaxes))
}

#[derive(Debug)]
pub struct MacroDefined;

struct Macro {
  parameters: Vec<Name>,
  tree: crate::parse_tree::Tree,
}

impl Macro {
  fn new(parameters: Vec<Name>, tree: crate::parse_tree::Tree) -> Self {
    Self { parameters, tree }
  }
}

#[derive(Default)]
pub struct Environment {
  bindings: HashMap<Name, crate::parse_tree::Tree>,
  namegen: usize,
}

impl Environment {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert(&mut self, name: Name, tree: crate::parse_tree::Tree) {
    self.bindings.insert(name, tree);
  }

  pub fn get(&mut self, name: &Name) -> Option<&crate::parse_tree::Tree> {
    self.bindings.get(name)
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

impl Macro {
  fn expand(
    tree: crate::parse_tree::Tree,
    bindings: &HashMap<Name, crate::parse_tree::Tree>,
  ) -> miette::Result<crate::parse_tree::Tree> {
    match tree.tree_kind {
      crate::parse_tree::TreeKind::Variable(ref name) => match bindings.get(name) {
        Some(tree) => Ok(tree.clone()),
        None => Ok(tree),
      },
      crate::parse_tree::TreeKind::Literal(_) => Ok(tree),
      crate::parse_tree::TreeKind::Spine(callee, spine) => {
        let callee = Macro::expand(*callee, bindings)?;
        let spine = spine
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Result<_, _>>()?;
        let tree_kind = crate::parse_tree::TreeKind::Spine(callee.into(), spine);
        Ok(crate::parse_tree::Tree::new(tree_kind, tree.loc))
      }
      crate::parse_tree::TreeKind::Binary(op, lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings)?;
        let rhs = Macro::expand(*rhs, bindings)?;
        let tree_kind = crate::parse_tree::TreeKind::Binary(op, lhs.into(), rhs.into());
        Ok(crate::parse_tree::Tree::new(tree_kind, tree.loc))
      }
      crate::parse_tree::TreeKind::Lambda(lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings)?;
        let rhs = Macro::expand(*rhs, bindings)?;
        let tree_kind = crate::parse_tree::TreeKind::Lambda(lhs.into(), rhs.into());
        Ok(crate::parse_tree::Tree::new(tree_kind, tree.loc))
      }
      crate::parse_tree::TreeKind::Let(bind, value) => {
        let bind = Macro::expand(*bind, bindings)?;
        let value = Macro::expand(*value, bindings)?;
        let tree_kind = crate::parse_tree::TreeKind::Let(bind.into(), value.into());
        Ok(crate::parse_tree::Tree::new(tree_kind, tree.loc))
      }
      crate::parse_tree::TreeKind::Block(block) => {
        let block = block
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Result<_, _>>()?;
        let tree_kind = crate::parse_tree::TreeKind::Block(block);
        Ok(crate::parse_tree::Tree::new(tree_kind, tree.loc))
      }
      crate::parse_tree::TreeKind::Macro(_, _, _) => Err(NestedMacroError {
        loc: tree.loc.clone(),
        src: tree.loc.source.clone(),
      })?,
    }
  }
}

trait ToSyntax {
  fn to_syntax(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<ExpandedTree>;
}

impl ToSyntax for crate::parse_tree::Tree {
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
            let mut bindings = HashMap::new();
            for (bind, tree) in r#macro.parameters.iter().zip(arguments) {
              bindings.insert(bind.clone(), tree);
            }
            Macro::expand(r#macro.tree.clone(), &bindings)?.to_syntax(macros)
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
      TreeKind::Lambda(lhs, rhs) => Ok(Syntax::Lambda(
        lhs.to_lambda_pattern(macros)?,
        rhs.to_syntax(macros)?.into(),
      )),
      TreeKind::Let(bind, value) => match bind.to_syntax(macros)? {
        Syntax::Variable(name) => Ok(Syntax::Let(name, value.to_syntax(macros)?.into())),
        _ => Err(BindNotValid {
          loc: self.loc.clone(),
          src: self.loc.source,
        })?,
      },
      TreeKind::Macro(name, params, tree) => {
        let mut parameters = vec![];
        for param in params {
          match param.tree_kind {
            TreeKind::Variable(name) => parameters.push(name),
            _ => Err(BindNotValid {
              loc: param.loc.clone(),
              src: param.loc.source,
            })?,
          }
        }
        macros.insert(name, Macro::new(parameters, *tree));
        Ok(Syntax::Ext(MacroDefined))
      }
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

trait ToLambdaPattern {
  fn to_lambda_pattern(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<LambdaPattern>;
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

impl ToLambdaPattern for crate::parse_tree::Tree {
  fn to_lambda_pattern(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<LambdaPattern> {
    match self.tree_kind {
      crate::parse_tree::TreeKind::Variable(name) => match macros.get(&name) {
        Some(r#macro) if r#macro.parameters.len() == 0 => {
          r#macro.tree.clone().to_lambda_pattern(macros)
        }
        Some(r#macro) => Err(MacroArityError {
          parameters: r#macro.parameters.len(),
          loc: self.loc.clone(),
          src: self.loc.source,
        })?,
        None => Ok(LambdaPattern::Variable(name)),
      },
      crate::parse_tree::TreeKind::Spine(x1, xn) => {
        let trees = std::iter::once(*x1)
          .chain(xn)
          .map(|tree| tree.to_lambda_pattern(macros))
          .collect::<Result<_, _>>()?;
        Ok(LambdaPattern::Many(trees))
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
