#[derive(PartialEq, Debug)]
pub enum Token {
  LeftParen, RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  Identifier(String),
  String(String),
  Number(String),
 
  // Keywords
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

impl Token {
  pub fn is_equality(&self) -> bool {
    match self {
      Self::BangEqual => true,
      Self::EqualEqual => true,
      _ => false
    }
  }
  pub fn is_comparison(&self) -> bool {
    match self {
      Self::Greater => true,
      Self::GreaterEqual => true,
      Self::Less => true,
      Self::LessEqual => true,
      _ => false
    }
  }
  pub fn is_add_or_sub(&self) -> bool {
    match self {
      Self::Plus => true,
      Self::Minus => true,
      _ => false
    }
  }
  pub fn is_div_or_mul(&self) -> bool {
    match self {
      Self::Slash => true,
      Self::Star => true,
      _ => false
    }
  }
  pub fn is_unary(&self) -> bool {
    match self {
      Self::Bang => true,
      Self::Minus => true,
      _ => false
    }
  }
}

#[derive(Debug)]
pub enum SyntaxError {
  UnexpectedCharacter { character: char },
  ReadError { error: Box<dyn std::error::Error> }
}

pub struct Tokenizer<T: Iterator<Item = Result<u8, std::io::Error>>> {
  stream: std::iter::Peekable<T>,
  line: usize,
  col: usize,
  current: char,
  eof: bool
}

impl<T: Iterator<Item = Result<u8, std::io::Error>>> Tokenizer<T> {
  pub fn new(stream: T) -> Self {
    Tokenizer {
      stream: stream.peekable(),
      line: 1,
      col: 0,
      current: '\0',
      eof: false
    }
  }
  pub fn try_read_token(&mut self) -> Result<Token, SyntaxError> {
    match self.consume() {
      None => {
        self.eof = true;
        return Ok(Token::Eof)
      },
      Some(Ok(_)) => {}
      Some(Err(err)) => return Err(err)
    }

    match self.current {
      '(' => Ok(Token::LeftParen),
      ')' => Ok(Token::RightParen),
      '{' => Ok(Token::LeftBrace),
      '}' => Ok(Token::RightBrace),
      ',' => Ok(Token::Comma),
      '.' => Ok(Token::Dot),
      '-' => Ok(Token::Minus),
      '+' => Ok(Token::Plus),
      ';' => Ok(Token::Semicolon),
      '/' => Ok(Token::Slash),
      '*' => Ok(Token::Star),
      '!' => Ok(if self.is_next('=') { 
        self.stream.next();
        Token::BangEqual
      } else { Token::Bang }),
      '=' => Ok(if self.is_next('=') { 
        self.stream.next();
        Token::EqualEqual
      } else { Token::Equal }),
      '>' => Ok(if self.is_next('=') { 
        self.stream.next();
        Token::GreaterEqual
      } else { Token::Greater }),
      '<' => Ok(if self.is_next('=') { 
        self.stream.next();
        Token::LessEqual
      } else { Token::Less }),
      '"' => {
        let mut s = "".to_string();
        while let Some(Ok(char)) = self.consume() {
          if char == '"' {
            return Ok(Token::String(s.to_string()))
          } else {
            s.push(char);
          }
        }
        Err(SyntaxError::UnexpectedCharacter { character: self.current })
      },
      c if c.is_digit(10) => Ok(self.scan_digit()),
      c if c.is_alphanumeric() => Ok(self.scan_identifier()),
      c if c.is_whitespace() => {
        return self.try_read_token() // skip whitespace
      },
      _ => { 
        Err(SyntaxError::UnexpectedCharacter { character: self.current })
      }
    }
  }
  fn consume(&mut self) -> Option<Result<char, SyntaxError>>{
    let char = self.stream.next()?;
    if let Err(err) = char {
      return Some(Err(SyntaxError::ReadError { error: Box::new(err) }))
    }
    self.col += 1;
    if self.current == '\n' {
      self.line += 1;
      self.col = 1;
    }
    self.current = char.unwrap() as char;
    Some(Ok(self.current))
  }
  fn is_next(&mut self, c: char) -> bool {
    let next = self.stream.peek();
    if let Some(Ok(next)) = next {
      return *next as char == c;
    }
    return false
  }
  fn scan_digit(&mut self) -> Token {
    let mut s = "".to_string();
    s.push(self.current);
    while let Some(Ok(char)) = self.consume() {
      if char.is_digit(10) || char == '.' {
        s.push(char);
      } else {
        break;
      };
    }
    Token::Number(s)
  }
  fn scan_identifier(&mut self) -> Token {
    let mut s = "".to_string();
    s.push(self.current);
    while let Some(Ok(char)) = self.consume() {
      if char.is_alphanumeric() {
        s.push(char);
      } else {
        break;
      }
    }
    match s.as_str() {
      "and" => Token::And,
      "class" => Token::Class,
      "else" => Token::Else,
      "false" => Token::False,
      "fun" => Token::Fun,
      "for" => Token::For,
      "if" => Token::If, 
      "nil" => Token::Nil,
      "or" => Token::Or,
      "print" => Token::Print,
      "return" => Token::Return,
      "super" => Token::Super,
      "this" => Token::This,
      "true" => Token::True,
      "var" => Token::Var,
      "while" => Token::While,
      _ => Token::Identifier(s)
    }
  }
}

impl<T: Iterator<Item = Result<u8, std::io::Error>>> Iterator for Tokenizer<T> {
  type Item = Result<Token, SyntaxError>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.eof {
      return None
    }
    Some(self.try_read_token())
  }
}