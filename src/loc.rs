use miette::IntoDiagnostic;

#[derive(Clone, Debug)]
pub struct Loc {
  pub start: usize,
  pub end: usize,
  pub source: Source,
}

impl Loc {
  pub fn merge(self, other: Self) -> Self {
    // assert!(self.source == other.source);
    Self {
      start: std::cmp::min(self.start, other.start),
      end: std::cmp::max(self.end, other.end),
      source: self.source,
    }
  }
}

#[derive(Clone, Debug)]
pub struct Source {
  pub(crate) source: miette::NamedSource<std::sync::Arc<String>>,
}

impl TryFrom<std::path::PathBuf> for Source {
  type Error = miette::Report;

  fn try_from(path: std::path::PathBuf) -> Result<Self, Self::Error> {
    let text = std::fs::read_to_string(&path).into_diagnostic()?;
    let source = miette::NamedSource::new(path.to_str().unwrap_or(""), std::sync::Arc::new(text))
      .with_language("icecream");
    Ok(Source { source })
  }
}

impl From<String> for Source {
  fn from(text: String) -> Self {
    let source = miette::NamedSource::new("", std::sync::Arc::new(text));
    Source { source }
  }
}

impl From<&str> for Source {
  fn from(value: &str) -> Self {
    Self::from(value.to_owned())
  }
}

impl Source {
  pub fn text(&self) -> &str {
    self.source.inner()
  }

  pub fn get_text(&self) -> String {
    self.source.inner().to_string()
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(std::rc::Rc<String>);

impl std::borrow::Borrow<str> for Name {
  fn borrow(&self) -> &str {
    &self.0
  }
}

impl std::borrow::Borrow<String> for Name {
  fn borrow(&self) -> &String {
    &self.0
  }
}

impl From<String> for Name {
  fn from(value: String) -> Self {
    Self(std::rc::Rc::new(value))
  }
}

impl AsRef<String> for Name {
  fn as_ref(&self) -> &String {
    &self.0
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Text(std::rc::Rc<String>);

impl std::fmt::Display for Text {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl From<String> for Text {
  fn from(value: String) -> Self {
    Self(std::rc::Rc::new(value))
  }
}

impl AsRef<String> for Text {
  fn as_ref(&self) -> &String {
    &self.0
  }
}

impl miette::SourceCode for Source {
  fn read_span<'a>(
    &'a self,
    span: &miette::SourceSpan,
    context_lines_before: usize,
    context_lines_after: usize,
  ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
    self
      .source
      .read_span(span, context_lines_before, context_lines_after)
  }
}

impl From<Loc> for miette::SourceSpan {
  fn from(loc: Loc) -> Self {
    let length = loc.end - loc.start;
    miette::SourceSpan::new(miette::SourceOffset::from(loc.start), length)
  }
}

impl std::fmt::Display for Name {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}
