use std::{iter::Peekable, str::Chars};

pub struct Lexer<'src> {
  pub source: crate::loc::Source,
  pub peekable: Peekable<Chars<'src>>,
  pub index: usize,
  pub start: usize,
}

pub fn parse_from_source(source: crate::loc::Source) -> miette::Result<crate::parse_tree::Program> {
  let str = &source.get_text();
  let lexer = Lexer {
    source,
    peekable: str.chars().peekable(),
    index: 0,
    start: 0,
  };
  let mut parser = crate::parser::Parser::new(lexer);
  parser.parse_program()
}

impl<'src> Lexer<'src> {
  fn advance(&mut self) -> Option<char> {
    let char = self.peekable.next()?;
    self.index += char.len_utf8();
    Some(char)
  }

  fn save(&mut self) {
    self.start = self.index;
  }

  #[inline(always)]
  fn peek(&mut self) -> Option<&char> {
    self.peekable.peek()
  }

  fn advance_while(&mut self, predicate: impl Fn(&char) -> bool) {
    while let Some(char) = self.peek() {
      if predicate(char) {
        self.advance().unwrap();
      } else {
        break;
      }
    }
  }

  fn loc(&self) -> crate::loc::Loc {
    crate::loc::Loc {
      start: self.start,
      end: self.index,
      source: self.source.clone(),
    }
  }

  fn skip_whitespace(&mut self) {
    self.advance_while(|c| matches!(*c, '\t' | '\x0C' | '\r' | ' '));
  }

  fn skip(&mut self) {
    loop {
      match self.peek() {
        Some('#') => self.advance_while(|c| *c != '\n'),
        Some(c) if c.is_ascii_whitespace() && *c != '\n' => self.skip_whitespace(),
        Some(_) | None => break,
      }
    }
  }

  fn lexeme(&self) -> &str {
    &self.source.text()[self.start..self.index]
  }

  fn consume(&mut self, expected: char) -> bool {
    match self.peek() {
      Some(char) if *char == expected => {
        self.advance().unwrap();
        true
      }
      _ => false,
    }
  }

  fn string(&mut self) -> Option<String> {
    let mut buf = String::with_capacity(16);

    while let Some(char) = self.peek() {
      if *char == '"' {
        break;
      }
      buf.push(self.advance().unwrap());
    }

    if self.consume('"') {
      Some(buf)
    } else {
      None
    }
  }

  pub fn next_token(&mut self) -> crate::token::Token {
    self.skip();
    self.save();

    let token_kind = if let Some(char) = self.advance() {
      match char {
        '\n' => crate::token::TokenKind::Separator,
        ';' => crate::token::TokenKind::Separator,
        '(' => crate::token::TokenKind::LParens,
        ')' => crate::token::TokenKind::RParens,
        '{' => crate::token::TokenKind::LBrace,
        '}' => crate::token::TokenKind::RBrace,
        '+' => crate::token::TokenKind::Plus,
        '-' => crate::token::TokenKind::Minus,
        '*' => crate::token::TokenKind::Star,
        '/' => crate::token::TokenKind::Slash,
        '=' if self.consume('=') => crate::token::TokenKind::EqualEqual,
        '=' if self.consume('>') => crate::token::TokenKind::FatArrow,
        '=' => crate::token::TokenKind::Equal,
        '!' if self.consume('=') => crate::token::TokenKind::NotEqual,
        '0'..='9' => {
          self.advance_while(|c| c.is_ascii_digit());
          if self.consume('.') {
            self.advance_while(|c| c.is_ascii_digit());
          }
          crate::token::TokenKind::Number
        }
        'a'..='z' | 'A'..='Z' => {
          self.advance_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '-');
          match self.lexeme() {
            "true" => crate::token::TokenKind::True,
            "false" => crate::token::TokenKind::False,
            "let" => crate::token::TokenKind::Let,
            "macro" => crate::token::TokenKind::Macro,
            "and" => crate::token::TokenKind::And,
            "or" => crate::token::TokenKind::Or,
            _ => crate::token::TokenKind::Identifier,
          }
        }
        '"' => {
          if let Some(string) = self.string() {
            return crate::token::Token {
              token_kind: crate::token::TokenKind::String,
              lexeme: string,
              loc: self.loc(),
            };
          } else {
            crate::token::TokenKind::Error
          }
        }
        _ => crate::token::TokenKind::Error,
      }
    } else {
      crate::token::TokenKind::Eof
    };

    crate::token::Token {
      token_kind,
      lexeme: self.lexeme().to_owned(),
      loc: self.loc(),
    }
  }
}

impl<'src> Iterator for Lexer<'src> {
  type Item = crate::token::Token;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index >= self.source.text().len() {
      None
    } else {
      Some(self.next_token())
    }
  }
}

#[cfg(test)]
mod test {
  use super::Lexer;

  #[test]
  fn lexer_test() {
    let source = r#"
      ##bruh
"biologic"

x = x => x

x 2
"#;
    let source = crate::loc::Source::from(source);
    let src = &source.get_text();
    let lexer = Lexer {
      source,
      peekable: src.chars().peekable(),
      index: 0,
      start: 0,
    };

    println!("{:?}", lexer.into_iter().collect::<Vec<_>>());
  }
}
