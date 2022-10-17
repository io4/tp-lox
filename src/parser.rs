use crate::tokenizer::Token;

#[derive(PartialEq, Debug)]
pub enum ASTNode {
  BinaryOp { lhs: Box<ASTNode>, operation: Token, rhs: Box<ASTNode> },
  UnaryOp { operation: Token, node: Box<ASTNode> },
  StringLiteral(String),
  NumericLiteral(f64),
  BooleanLiteral(bool),
  NilLiteral,
  Error,
  Grouping(Box<ASTNode>)
}

pub struct Parser<T: Iterator<Item = Token>> {
  stream: std::iter::Peekable<T>,
  ended: bool
}

impl<T: Iterator<Item = Token>> Parser<T> {
  pub fn new(stream: T) -> Self {
    Parser {
      stream: stream.peekable(),
      ended: false
    }
  }
  pub fn parse_expresion(&mut self) -> ASTNode {
    let node = self.parse_equality();
    if self.is_next(Token::Eof) {
      self.ended = true;
    }
    node
  }
  pub fn parse_equality(&mut self) -> ASTNode {
    self.parse_binary(&mut |p| p.parse_comparison(), &|tok| tok.is_equality())
  }
  pub fn parse_comparison(&mut self) -> ASTNode {
    self.parse_binary(&mut |p| p.parse_term(), &|tok| tok.is_comparison())
  }
  pub fn parse_term(&mut self) -> ASTNode {
    self.parse_binary(&mut |p| p.parse_factor(), &|tok| tok.is_add_or_sub())
  }
  pub fn parse_factor(&mut self) -> ASTNode {
    self.parse_binary(&mut |p| p.parse_unary(), &|tok| tok.is_div_or_mul())
  }
  pub fn parse_unary(&mut self) -> ASTNode {
    if self.peek().is_unary() {
      return ASTNode::UnaryOp {
        operation: self.consume(),
        node: Box::new(self.parse_primary())
      }
    }
    self.parse_primary()
  }
  pub fn parse_primary(&mut self) -> ASTNode {
    let tok = self.consume();
    match tok {
      Token::Number(str) => ASTNode::NumericLiteral(str.parse::<f64>().unwrap()),
      Token::String(str) => ASTNode::StringLiteral(str.to_owned()),
      Token::True => ASTNode::BooleanLiteral(true),
      Token::False => ASTNode::BooleanLiteral(false),
      Token::Nil => ASTNode::NilLiteral,
      Token::LeftParen => {
        let node = self.parse_expresion();
        if self.consume() != Token::RightParen {
          return ASTNode::Error
        }
        return ASTNode::Grouping(Box::new(node))
      }
      _ => ASTNode::Error
    }
  }
  pub fn parse_binary(&mut self, parse_branch: &mut dyn FnMut(&mut Self) -> ASTNode, is_needed_op: &dyn Fn(&Token) -> bool) -> ASTNode {
    let mut node = parse_branch(self);
    while is_needed_op(self.peek()) {
      node = ASTNode::BinaryOp { lhs: Box::new(node), operation: self.consume(), rhs: Box::new(parse_branch(self)) }
    }
    node
  }
  pub fn consume(&mut self) -> Token {
    let tok = self.stream.next().unwrap_or(Token::Eof);
    if tok == Token::Eof {
      self.ended = true
    }
    tok
  }
  pub fn peek(&mut self) -> &Token {
    self.stream.peek().unwrap_or(&Token::Eof)
  }
  pub fn is_next(&mut self, tok: Token) -> bool {
    self.peek() == &tok
  }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
  type Item = ASTNode;

  fn next(&mut self) -> Option<Self::Item> {
    if self.ended {
      return None
    }
    Some(self.parse_expresion())
  }
}