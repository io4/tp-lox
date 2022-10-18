use crate::tokenizer::{Token, LexingError};

#[derive(Debug)]
pub enum ASTNode {
  BinaryOp { lhs: Box<ASTNode>, operation: Token, rhs: Box<ASTNode> },
  UnaryOp { operation: Token, node: Box<ASTNode> },
  StringLiteral(String),
  NumericLiteral(f64),
  BooleanLiteral(bool),
  NilLiteral,
  Error(ParseError),
  Grouping(Box<ASTNode>),
  Statement(Box<ASTNode>),
  PrintStatement(Box<ASTNode>),
  VariableDeclaration(String, Option<Box<ASTNode>>),
  VariableAccess(String)
}

#[derive(Debug)]
pub enum ParseError {
  InvalidTokenFound(LexingError),
  ExpectedButFound { expected: Token, found: Token },
  UnexpectedToken(Token),
  ExpectedExpressionBut(Box<ParseError>),
  ExpectedIdentifierButFound(Token)
}

pub struct Parser<T: Iterator<Item = Result<Token, LexingError>>> {
  stream: std::iter::Peekable<T>,
  ended: bool
}

impl<T: Iterator<Item = Result<Token, LexingError>>> Parser<T> {
  pub fn new(stream: T) -> Self {
    Parser {
      stream: stream.peekable(),
      ended: false
    }
  }
  pub fn parse(&mut self) -> ASTNode {
    match self.parse_declaration() {
      Ok(node) => node,
      Err(err) => {
        self.synchronize();
        return ASTNode::Error(err)
      }
    }
  }
  pub fn parse_statement(&mut self) -> Result<ASTNode, ParseError> {
    let node = if self.is_next(Token::Print) {
      self.consume()?;
      ASTNode::PrintStatement(Box::new(self.parse_expresion()?))
    } else {
      ASTNode::Statement(Box::new(self.parse_expresion()?))
    };
    self.consume_expecting(Token::Semicolon)?;
    Ok(node)
  }
  pub fn parse_declaration(&mut self) -> Result<ASTNode, ParseError> {
    if self.is_next(Token::Var) {
      self.consume()?;

      let name = self.consume_identifier()?;
      let value = if self.is_next(Token::Equal) {
        self.consume()?;
        let expr = self.parse_expresion()?;
        self.consume_expecting(Token::Semicolon)?;
        Some(Box::new(expr))
      } else {
        None
      };
      Ok(ASTNode::VariableDeclaration(name, value))
    } else {
      self.parse_statement()
    }
  }
  pub fn parse_expresion(&mut self) -> Result<ASTNode, ParseError> {
    if self.is_next(Token::Semicolon) {
      self.consume()?;
    }
    self.parse_equality().map_err(|err| ParseError::ExpectedExpressionBut(Box::new(err)))
  }
  pub fn parse_equality(&mut self) -> Result<ASTNode, ParseError> {
    self.parse_binary(&mut |p| p.parse_comparison(), &|tok| tok.is_equality())
  }
  pub fn parse_comparison(&mut self) -> Result<ASTNode, ParseError> {
    self.parse_binary(&mut |p| p.parse_term(), &|tok| tok.is_comparison())
  }
  pub fn parse_term(&mut self) -> Result<ASTNode, ParseError> {
    self.parse_binary(&mut |p| p.parse_factor(), &|tok| tok.is_add_or_sub())
  }
  pub fn parse_factor(&mut self) -> Result<ASTNode, ParseError> {
    self.parse_binary(&mut |p| p.parse_unary(), &|tok| tok.is_div_or_mul())
  }
  pub fn parse_unary(&mut self) -> Result<ASTNode, ParseError> {
    if self.next_matches(&|tok| tok.is_unary()) {
      return Ok(ASTNode::UnaryOp {
        operation: self.consume()?,
        node: Box::new(self.parse_primary()?)
      })
    }
    self.parse_primary()
  }
  pub fn parse_primary(&mut self) -> Result<ASTNode, ParseError> {
    let tok = self.consume()?;
    Ok(match tok {
      Token::Number(str) => ASTNode::NumericLiteral(str.parse::<f64>().unwrap()),
      Token::String(str) => ASTNode::StringLiteral(str.to_owned()),
      Token::True => ASTNode::BooleanLiteral(true),
      Token::False => ASTNode::BooleanLiteral(false),
      Token::Nil => ASTNode::NilLiteral,
      Token::LeftParen => {
        let node = self.parse_expresion()?;
        self.consume_expecting(Token::RightParen)?;
        ASTNode::Grouping(Box::new(node))
      }
      Token::Identifier(name) => ASTNode::VariableAccess(name),
      _ => return Err(ParseError::UnexpectedToken(tok))
    })
  }
  pub fn synchronize(&mut self) {
    while self.next_matches(&|tok| tok.is_synchronization_point()) {
      let _ = self.consume();
    }
  }
  pub fn parse_binary(&mut self, parse_branch: &mut dyn FnMut(&mut Self) -> Result<ASTNode, ParseError>, is_needed_op: &dyn Fn(&Token) -> bool)
    -> Result<ASTNode, ParseError>
  {
    let mut node = parse_branch(self)?;
    while self.next_matches(is_needed_op) {
      node = ASTNode::BinaryOp { lhs: Box::new(node), operation: self.consume()?, rhs: Box::new(parse_branch(self)?) }
    }
    Ok(node)
  }
  pub fn consume(&mut self) -> Result<Token, ParseError> {
    let tok = self.stream.next().unwrap_or(Ok(Token::Eof));
    if let Err(err) = tok {
      return Err(ParseError::InvalidTokenFound(err))
    }
    let tok = tok.unwrap();
    if tok == Token::Eof {
      self.ended = true
    }
    Ok(tok)
  }
  pub fn consume_expecting(&mut self, expected: Token) -> Result<Token, ParseError> {
    let tok = self.consume()?;
    if tok != expected {
      return Err(ParseError::ExpectedButFound { expected, found: tok })
    }
    Ok(tok)
  }
  pub fn consume_identifier(&mut self) -> Result<String, ParseError> {
    match self.consume()? {
      Token::Identifier(name) => Ok(name),
      found => Err(ParseError::ExpectedIdentifierButFound(found))
    }
  }
  pub fn peek(&mut self) -> Option<&Token> {
    match self.stream.peek() {
      Some(Ok(tok)) => Some(tok),
      _ => None
    }
  }
  pub fn next_matches(&mut self, condition: &dyn Fn(&Token) -> bool) -> bool {
    match self.peek() {
      Some(next) => condition(next),
      _ => false
    }
  }
  pub fn is_next(&mut self, tok: Token) -> bool {
    self.next_matches(&|next| std::mem::discriminant(&tok) == std::mem::discriminant(next))
  }
}

impl<T: Iterator<Item = Result<Token, LexingError>>> Iterator for Parser<T> {
  type Item = ASTNode;

  fn next(&mut self) -> Option<Self::Item> {
    if self.ended {
      return None
    }
    Some(self.parse())
  }
}