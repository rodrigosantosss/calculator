use std::fmt::Display;

use crate::tokenizer::Token;
use rust_decimal_macros::dec;

#[derive(Debug)]
pub struct ParseTree {
    pub token: Token,
    pub left: Option<Box<ParseTree>>,
    pub right: Option<Box<ParseTree>>,
}

impl ParseTree {
    fn new(token: Token) -> Self {
        Self {
            token,
            left: None,
            right: None,
        }
    }
}

#[derive(Debug)]
pub enum ParsingError {
    InvalidParenthesis,
    InvalidComma,
    InvalidArgs,
    ExpectedExpression,
    ExpectedOperation,
    BlankInput,
    Unknown,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidParenthesis => write!(f, "Invalid Parenthesis"),
            Self::InvalidComma => write!(f, "Invalid Comma"),
            Self::InvalidArgs => write!(f, "Invalid Arguments to function"),
            Self::ExpectedExpression => write!(f, "Expected Expression"),
            Self::ExpectedOperation => write!(f, "Expected Operation"),
            Self::BlankInput => write!(f, "Missing Input"),
            Self::Unknown => write!(f, "Unknown"),
        }
    }
}

#[derive(Debug)]
enum Expression {
    Token(Token),
    Tree(ParseTree),
}

impl Expression {
    fn unwrap_token_unchecked(self) -> Token {
        match self {
            Self::Token(x) => x,
            _ => unreachable!(),
        }
    }
}

macro_rules! next_expression {
    ($iter:ident) => {
        match $iter.next() {
            Some(Expression::Token(Token::OpenParenthesis)) => {
                let mut expressions: Vec<Vec<Expression>> = vec![ Vec::new() ];
                let mut curr: usize = 0;
                let mut open_count = 1usize;
                let mut close_count = 0usize;
                while let Some(next) = $iter.next() {
                    match next {
                        Expression::Token(Token::OpenParenthesis) => {
                            open_count += 1;
                            expressions[curr].push(next);
                        }
                        Expression::Token(Token::Comma) => {
                            curr += 1;
                            expressions.push(Vec::new());
                        }
                        Expression::Token(Token::CloseParenthesis) => {
                            close_count += 1;
                            if close_count == open_count {
                                break;
                            }
                            expressions[curr].push(next);
                        }
                        _ => expressions[curr].push(next),
                    }
                }
                if close_count != open_count {
                    return Err(ParsingError::InvalidParenthesis);
                }
                expressions.into_iter().map(|expression| parse_expressions(expression).map(|tree| Expression::Tree(tree))).collect::<Result<Vec<_>, _>>()?
            }
            Some(Expression::Token(Token::CloseParenthesis)) => {
                return Err(ParsingError::InvalidParenthesis);
            }
            Some(expr) => vec![expr],
            None => Vec::new(),
        }
    };
}

macro_rules! parse_factorial {
    ($expressions:ident, $buffer:ident) => {
        loop {
            let expr = next_expression!($expressions);
            let expr_len = expr.len();
            let expr = match expr.into_iter().next() {
                Some(e) if expr_len == 1 => e,
                Some(_) => return Err(ParsingError::InvalidComma),
                None => break
            };
            match expr {
                Expression::Token(Token::Factorial) => {
                    let arg1 = $buffer.pop().ok_or(ParsingError::ExpectedExpression)?;
                    $buffer.push(Expression::Tree(ParseTree {
                        token: Token::Factorial,
                        left: match arg1 {
                            Expression::Token(lit) => Some(Box::new(ParseTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                        right: None,
                    }));
                }
                _ => $buffer.push(expr)
            }
        }
    };
}

macro_rules! parse_function {
    ($expressions:ident, $buffer:ident, $fn:ident $(, $other_fn:ident)*) => {
        loop {
            let expr = next_expression!($expressions);
            let expr_len = expr.len();
            let expr = match expr.into_iter().next() {
                Some(e) if expr_len == 1 => e,
                Some(_) => return Err(ParsingError::InvalidComma),
                None => break
            };
            match expr {
                Expression::Token(Token::$fn $(| Token::$other_fn)*) => {
                    let function = expr.unwrap_token_unchecked();
                    let mut exprs = next_expression!($expressions).into_iter();
                    let arg1 = match exprs.next() {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            }
                            else {
                                return Err(ParsingError::ExpectedExpression);
                            }
                        },
                        Some(expr) => expr,
                        None => return Err(ParsingError::ExpectedExpression)
                    };
                    let mut arg2: Option<Expression> = None;
                    if let Token::Log = function {
                        arg2 = exprs.next();
                        if exprs.next().is_some() {
                            return Err(ParsingError::InvalidArgs);
                        }
                    }
                    else if exprs.len() > 1 {
                        return Err(ParsingError::InvalidArgs);
                    }
                    $buffer.push(Expression::Tree(ParseTree {
                        token: function,
                        left: match arg1 {
                            Expression::Token(lit) => Some(Box::new(ParseTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                        right: arg2.map(|expr| match expr {
                            Expression::Token(lit) => Box::new(ParseTree::new(lit)),
                            Expression::Tree(tree) => Box::new(tree),
                        }),
                    }));
                },
                _ => $buffer.push(expr),
            }
        }
    };
}

macro_rules! parse_operation {
    ($expressions:ident, $buffer:ident, $op:ident $(, $other_op:ident)*) => {
        loop {
            let expr = next_expression!($expressions);
            let expr_len = expr.len();
            let expr = match expr.into_iter().next() {
                Some(e) if expr_len == 1 => e,
                Some(_) => return Err(ParsingError::InvalidComma),
                None => break
            };
            match expr {
                Expression::Token(Token::$op $(| Token::$other_op)*) => {
                    let operation = expr.unwrap_token_unchecked();
                    let last = match $buffer.pop() {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            }
                            else {
                                return Err(ParsingError::ExpectedExpression);
                            }
                        }
                        Some(expr) => expr,
                        None => {
                            if let Token::Add | Token::Sub = operation {
                                Expression::Token(Token::Literal(dec!(0)))
                            } else {
                                return Err(ParsingError::ExpectedExpression);
                            }
                        }
                    };
                    let mut exprs = next_expression!($expressions).into_iter();
                    let next = match exprs.next() {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            }
                            else {
                                return Err(ParsingError::ExpectedExpression);
                            }
                        }
                        Some(expr) => expr,
                        None => return Err(ParsingError::ExpectedExpression)
                    };
                    if exprs.next().is_some() {
                        return Err(ParsingError::InvalidComma);
                    }
                    $buffer.push(Expression::Tree(ParseTree {
                        token: operation,
                        left: match last {
                            Expression::Token(lit) => Some(Box::new(ParseTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                        right: match next {
                            Expression::Token(lit) => Some(Box::new(ParseTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                    }));
                }
                _ => $buffer.push(expr),
            }
        }
    };
}

fn parse_expressions(expressions: Vec<Expression>) -> Result<ParseTree, ParsingError> {
    let mut expressions = expressions.into_iter();

    let mut buffer = Vec::new();

    parse_function!(
        expressions,
        buffer,
        Sin,
        Cos,
        Tan,
        Exp,
        Ln,
        Log,
        Sqrt
    );

    expressions = buffer.into_iter();
    buffer = Vec::new();

    parse_factorial!(expressions, buffer);

    expressions = buffer.into_iter();
    buffer = Vec::new();

    parse_operation!(expressions, buffer, Pow);

    expressions = buffer.into_iter();
    buffer = Vec::new();

    parse_operation!(expressions, buffer, ImplMul);

    expressions = buffer.into_iter();
    buffer = Vec::new();

    parse_operation!(expressions, buffer, Mul, Div);

    expressions = buffer.into_iter();
    buffer = Vec::new();

    parse_operation!(expressions, buffer, Add, Sub);

    let buffer_len = buffer.len();
    match buffer.into_iter().next() {
        Some(Expression::Token(token)) if buffer_len == 1 => {
            if token.is_value() {
                Ok(ParseTree::new(token))
            } else {
                Err(ParsingError::Unknown)
            }
        }
        Some(Expression::Tree(tree)) if buffer_len == 1 => Ok(tree),
        Some(_) => Err(ParsingError::ExpectedOperation),
        None => Err(ParsingError::BlankInput),
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<ParseTree, ParsingError> {
    parse_expressions(
        tokens
            .into_iter()
            .map(|token| Expression::Token(token))
            .collect(),
    )
}
