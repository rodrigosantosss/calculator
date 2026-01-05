use rust_decimal::prelude::*;

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Add,
    Sub,
    ImplMul,
    Mul,
    Div,
    Pow,
    Sin,
    Cos,
    Tan,
    Exp,
    Ln,
    Log,
    Sqrt,
    Factorial,
    OpenParenthesis,
    CloseParenthesis,
    Comma,
    Literal(Decimal),
    Ans,
    PI,
    E,
}

impl TryFrom<char> for Token {
    type Error = ();

    fn try_from(value: char) -> Result<Self, ()> {
        Ok(match value {
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            ':' | '/' => Self::Div,
            '^' => Self::Pow,
            '√' => Self::Sqrt,
            '!' => Self::Factorial,
            '(' => Self::OpenParenthesis,
            ')' => Self::CloseParenthesis,
            ',' => Self::Comma,
            _ => return Err(()),
        })
    }
}

impl TryFrom<String> for Token {
    type Error = ();

    fn try_from(mut value: String) -> Result<Self, ()> {
        if value.chars().next().ok_or(())?.is_digit(10) {
            Ok(Self::Literal(value.parse::<Decimal>().map_err(|_| ())?))
        } else {
            value.make_ascii_lowercase();
            Ok(match value.as_str() {
                "ans" => Self::Ans,
                "pi" | "π" => Self::PI,
                "e" => Self::E,
                "sin" => Self::Sin,
                "cos" => Self::Cos,
                "tan" => Self::Tan,
                "exp" => Self::Exp,
                "ln" => Self::Ln,
                "log" => Self::Log,
                "sqrt" => Self::Sqrt,
                _ => return Err(())
            })
        }
    }
}

impl Token {
    pub fn is_value(&self) -> bool {
        match self {
            Token::Literal(_) | Token::PI | Token::E | Token::Ans => true,
            _ => false,
        }
    }
}

pub fn tokenize(source: String) -> Option<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iterator = source.chars().peekable();

    while let Some(c) = iterator.next() {
        if c.is_whitespace() {
            continue;
        } else if c.is_alphabetic() {
            let mut literal = String::with_capacity(3);
            literal.push(c);
            while let Some(&d) = iterator.peek() {
                if d.is_alphabetic() {
                    literal.push(iterator.next().unwrap_or_default());
                } else {
                    break;
                }
            }
            let token = literal.try_into().ok()?;
            if let Some(prev) = tokens.last() {
                if prev.is_value() {
                    tokens.push(Token::ImplMul);
                }
            }
            tokens.push(token);
        } else if c.is_digit(10) {
            let mut literal = String::new();
            literal.push(c);
            let mut dot_appeared = false;
            while let Some(&d) = iterator.peek() {
                if d == '.' {
                    if dot_appeared {
                        return None;
                    } else {
                        dot_appeared = true;
                    }
                    literal.push(iterator.next().unwrap_or_default());
                    continue;
                }
                if !d.is_digit(10) {
                    break;
                }
                literal.push(iterator.next().unwrap_or_default());
            }
            let token = literal.try_into().ok()?;
            tokens.push(token);
        } else {
            let token = c.try_into().ok()?;
            if let Token::OpenParenthesis | Token::Sqrt = token {
                if let Some(prev) = tokens.last() {
                    if prev.is_value() {
                        tokens.push(Token::ImplMul);
                    }
                }
            }
            tokens.push(token);
        }
    }

    Some(tokens)
}
