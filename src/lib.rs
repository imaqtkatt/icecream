pub mod closure_conversion;
pub mod compiler;
pub mod lexer;
pub mod loc;
pub mod parse_tree;
pub mod parser;
pub mod syntax;
pub mod token;

pub fn source_passes(source: loc::Source) -> miette::Result<closure_conversion::ClosureConverted> {
  let program = lexer::parse_from_source(source)?;
  let expanded = syntax::program_to_expanded_tree(program)?;

  let closure_converted = {
    let mut scope = closure_conversion::Scope::empty();
    closure_conversion::closure_convert(expanded, &mut scope)?
  };

  Ok(closure_converted)
}
