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

#[derive(Debug)]
pub struct MacroDefined;

struct Macro {
  parameters: Vec<Name>,
  tree: crate::parse_tree::Tree,
}

impl Macro {
  fn expand(
    tree: crate::parse_tree::Tree,
    bindings: &HashMap<Name, crate::parse_tree::Tree>,
  ) -> crate::parse_tree::Tree {
    match tree.tree_kind {
      crate::parse_tree::TreeKind::Variable(ref name) => match bindings.get(name) {
        Some(tree) => tree.clone(),
        None => tree,
      },
      crate::parse_tree::TreeKind::Literal(_) => tree,
      crate::parse_tree::TreeKind::Spine(callee, spine) => {
        let callee = Macro::expand(*callee, bindings);
        let spine = spine
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Vec<_>>();
        let tree_kind = crate::parse_tree::TreeKind::Spine(callee.into(), spine);
        crate::parse_tree::Tree {
          tree_kind,
          loc: tree.loc,
        }
      }
      crate::parse_tree::TreeKind::Binary(op, lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings);
        let rhs = Macro::expand(*rhs, bindings);
        let tree_kind = crate::parse_tree::TreeKind::Binary(op, lhs.into(), rhs.into());
        crate::parse_tree::Tree {
          tree_kind,
          loc: tree.loc,
        }
      }
      crate::parse_tree::TreeKind::Lambda(lhs, rhs) => {
        let lhs = Macro::expand(*lhs, bindings);
        let rhs = Macro::expand(*rhs, bindings);
        let tree_kind = crate::parse_tree::TreeKind::Lambda(lhs.into(), rhs.into());
        crate::parse_tree::Tree {
          tree_kind,
          loc: tree.loc,
        }
      }
      crate::parse_tree::TreeKind::Let(bind, value) => {
        let bind = Macro::expand(*bind, bindings);
        let value = Macro::expand(*value, bindings);
        let tree_kind = crate::parse_tree::TreeKind::Let(bind.into(), value.into());
        crate::parse_tree::Tree {
          tree_kind,
          loc: tree.loc,
        }
      }
      crate::parse_tree::TreeKind::Block(block) => {
        let block = block
          .into_iter()
          .map(|tree| Macro::expand(tree, bindings))
          .collect::<Vec<_>>();
        let tree_kind = crate::parse_tree::TreeKind::Block(block);
        crate::parse_tree::Tree {
          tree_kind,
          loc: tree.loc,
        }
      }
      crate::parse_tree::TreeKind::Macro(_, _, _) => unreachable!(),
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
        Some(_) => panic!(),
        None => Ok(Syntax::Variable(name)),
      },

      TreeKind::Spine(callee, arguments) => match callee.tree_kind {
        TreeKind::Variable(ref name) => match macros.get(name) {
          Some(r#macro) if r#macro.parameters.len() == arguments.len() => {
            let mut bindings = HashMap::new();
            for (bind, tree) in r#macro.parameters.iter().zip(arguments) {
              bindings.insert(bind.clone(), tree);
            }
            Macro::expand(r#macro.tree.clone(), &bindings).to_syntax(macros)
          }
          Some(_) => panic!(),
          None => todo!(),
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
        _ => panic!(),
      },
      TreeKind::Macro(name, params, tree) => {
        let mut parameters = vec![];
        for param in params {
          match param.tree_kind {
            TreeKind::Variable(name) => parameters.push(name),
            e => panic!("{e:?}"),
          }
        }
        macros.insert(
          name,
          Macro {
            parameters,
            tree: *tree,
          },
        );
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

impl ToLambdaPattern for crate::parse_tree::Tree {
  fn to_lambda_pattern(self, macros: &mut HashMap<Name, Macro>) -> miette::Result<LambdaPattern> {
    match self.tree_kind {
      crate::parse_tree::TreeKind::Variable(name) => match macros.get(&name) {
        Some(r#macro) if r#macro.parameters.len() == 0 => {
          r#macro.tree.clone().to_lambda_pattern(macros)
        }
        Some(_) => panic!(),
        None => Ok(LambdaPattern::Variable(name)),
      },
      crate::parse_tree::TreeKind::Spine(x1, xn) => {
        let trees = std::iter::once(*x1)
          .chain(xn)
          .map(|tree| tree.to_lambda_pattern(macros))
          .collect::<Result<_, _>>()?;
        Ok(LambdaPattern::Many(trees))
      }
      _ => panic!(),
      // crate::parse_tree::TreeKind::Literal(_) => todo!(),
      // crate::parse_tree::TreeKind::Binary(_, _, _) => todo!(),
      // crate::parse_tree::TreeKind::Lambda(_, _) => todo!(),
      // crate::parse_tree::TreeKind::Let(_, _) => todo!(),
      // crate::parse_tree::TreeKind::Macro(_, _, _) => todo!(),
      // crate::parse_tree::TreeKind::Block(_) => todo!(),
    }
  }
}
