use std::{
    convert::TryFrom,
    error::Error,
    fmt,
    io::prelude::*,
    iter::Peekable,
    slice::Iter,
};

// derive attribute macro is used to automatically generate implementations of the Debug, PartialEq, Eq, Clone, and Copy traits for a struct or an enum.
// Define a enum token for Input Token : +, -, *, /, (, )
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Token {
    Plus, // `+`
    Dash, // `-`
    Star, // `*`
    Slash, // `/`
    RightParen, // `)`
    LeftParen, // `(`
    End, // The end of token
    Number(i64), // Numbers among token
}

// Define a method called `is_binary`that returns a boolean value indicating whether the token is a binary operator such as plus, dash, star, slash or not
impl Token {
    fn is_binary(&self) -> bool {
        match self {
            Token::Plus => true,
            Token::Dash => true,
            Token::Star => true,
            Token::Slash => true,
            _ => false,
        }
    }
}

// derive attribute macro of Debug, PartialEp, Eq
// Define a enum for Operator : +, *, /, -, 
#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Negative,
}

impl Operator {
    // Define a method called `cmp_val` that allocate precedence to operator
    fn cmp_val(&self) -> usize {
        match self {
            Operator::Negative => 4,
            Operator::Multiply => 3,
            Operator::Divide => 3,
            Operator::Add => 3,
            Operator::Subtract => 3,
        }
    }
}

impl TryFrom<Token> for Operator {
    type Error = &'static str;

    // Define a method called `try_from()` that converts token to operator
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Operator::Add),  // Convert a plus token to the add operator
            Token::Star => Ok(Operator::Multiply), // Convert a star token to the multiply operator
            Token::Dash => Ok(Operator::Subtract), // Convert a dash token to the subtract operator
            Token::Slash => Ok(Operator::Divide), // Convert a slash token to the divide operator
            _ => Err("Can only convert operators"),  // Return an error for any other token
        }
    }
}

// Define an enum called `Expression`
#[derive(Debug, PartialEq, Eq)]
enum Expression {
    // Define a variant called `Binary` that represents a binary expression
    Binary(Operator, Box<Expression>, Box<Expression>),
    // Define a variant called `Unary` that represents a unary expression
    Unary(Operator, Box<Expression>),
    // Define a variant called `Number` that represents a numeric value
    Number(i64),
}

// Implement methods for the `Expression` enum
impl Expression {
    // Define a recursive method called `eval()` that evaluates the expression and returns the result
    fn eval(&mut self) -> i64 {
        match self {
             // If the expression is a `Number`, return previous value * n
            Expression::Number(n) => *n,
            // If the expression is a `Unary` operation, negate the evaluated result of the inner expression
            Expression::Unary(_negative, expr) => -1 * expr.eval(),
            // If the expression is a `Binary` operation, evaluate both operands and perform the corresponding operation
            // Operations include Add, Multiply, Substract, Divide
            Expression::Binary(Operator::Add, expr1, expr2) => expr1.eval() + expr2.eval(),
            Expression::Binary(Operator::Multiply, expr1, expr2) => expr1.eval() * expr2.eval(),
            Expression::Binary(Operator::Subtract, expr1, expr2) => expr1.eval() - expr2.eval(),
            Expression::Binary(Operator::Divide, expr1, expr2) => expr1.eval() / expr2.eval(),
            _ => {
                panic!("Unreachable code: for expr {:?}", self);
            }
        }
    }
}

// Define a custom error struct called `SyntaxError`
#[derive(Debug)]
struct SyntaxError {
     // Define a `message` field to store the error message
    message: String,
    // Define a `level` field to store the error level (e.g. "Lex", "Parse", etc.)
    level: String,
}

// Define a custom error struct called `SyntaxError`
impl SyntaxError {
    // Define a constructor method for creating new `SyntaxError` objects with a "Lex" level
    fn new_lex_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Lex".to_string(),
        }
    }

    // Define a constructor method for creating new `SyntaxError` objects with a "Parse" level
    fn new_parse_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Parse".to_string(),
        }
    }
}

// Implement the `fmt::Display` trait for the `SyntaxError` struct
impl fmt::Display for SyntaxError {
    // Define the `fmt()` method for the `SyntaxError` struct
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Use the `write!()` macro to write a formatted string to the formatter object
        // The formatted string includes the `level` and `message` fields of the `SyntaxError` object
        write!(f, "{} Error {}", self.level, self.message)
    }
}

// Implement the `Error` trait for the `SyntaxError` struct
impl Error for SyntaxError {}

// define ClimbingParser struct
struct ClimbingParser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

// Implementation of a parser for an expression grammar using the Climbing Parsing algorithm
impl<'a> ClimbingParser<'a> {
    // Create a new ClimbingParser with an iterator over tokens
    fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        ClimbingParser { iter }
    }

    // Define a method called `assert_next` that asserts if the next token in the iterator is the specified token
    fn assert_next(&mut self, token: Token) -> Result<(), SyntaxError> {
        let next = self.iter.next();
        // checks whether there are any tokens left in the iterator self.iter
        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        // checks whether the next token in the iterator is equal to a given token
        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                token,
                next.unwrap(),
            )));
        }

        Ok(())
    }

    // Define a method called `primary` that parses a primary expression
    fn primary(&mut self) -> Result<Expression, SyntaxError> {
        let temp =  self.iter.next().unwrap();
        match temp {
             // If the token is a dash, create a new negative operator and recursively parse token to expression that follows it
            Token::Dash => {
                let op = Operator::Negative;
                let expr = self.expression(op.cmp_val())?;
                Ok(Expression::Unary(op, Box::new(expr)))
            }
            // If the token is rightparen, recursively parse token to expression
            Token::RightParen => {
                let expr: Expression = self.expression(0)?;
                self.assert_next(Token::LeftParen)?;
                Ok(expr)
            }
            // If the token is number, return it as OK
            Token::Number(n) => {
                Ok(Expression::Number(*n))
            },
            // Else Unexpected token error
            tok => Err(SyntaxError::new_parse_error(format!(
                "Unexpected token {:?}",
                tok
            ))),
        }
    }

     // Define a recursive method called `expression` to parse token to expression using climbing parse algorigthm
     // e.g.) Binary(Multiply, Binary(Add, Number(3), Number(2)), Number(4))
    fn expression(&mut self, precedence: usize) -> Result<Expression, SyntaxError> {
        let mut expr = self.primary()?;
        while let Some(tok) = self.iter.peek() {
            if !tok.is_binary() {
                break;
            }
            let operator = Operator::try_from(**tok).unwrap();
            if operator.cmp_val() < precedence {
                break;
            }
            self.iter.next();
            let inner_precedence = 1 + operator.cmp_val();
            let rhs = self.expression(inner_precedence)?;
            expr = Expression::Binary(operator, Box::new(expr), Box::new(rhs));
        }

        Ok(expr)
    }

    // Trigger function to parse token to expression
    fn parse(&mut self) -> Result<Expression, SyntaxError> {
        let ast = self.expression(0)?;
        self.assert_next(Token::End)?;
        Ok(ast)
    }
}

// Tokenize input string
fn lex(code: String) -> Result<Vec<Token>, SyntaxError> {
    // convert input string to iterator
    let mut iter = code.chars().peekable();
    // create new vector for tokens
    let mut tokens: Vec<Token> = Vec::new();
    let mut leftover: Option<char> = None;

    loop {
        let ch = match leftover {
            Some(ch) => ch,
            None => match iter.next() {
                None => break,
                Some(ch) => ch,
            },
        };
        leftover = None;
        match ch {
            ' ' => continue,
            'a' => tokens.push(Token::Plus),
            'c' => tokens.push(Token::Star),
            'd' => tokens.push(Token::Slash),
            'f' => tokens.push(Token::LeftParen),
            'e' => tokens.push(Token::RightParen),
            'b' => tokens.push(Token::Dash),
            ch if ch.is_ascii_digit() => {
                let number_stream: String = iter
                    .by_ref()
                    .take_while(|c| match c.is_ascii_digit() {
                        true => true,
                        false => {
                            leftover = Some(*c);
                            false
                        }
                    })
                    .collect();
                let number: i64 = format!("{}{}", ch, number_stream).parse().unwrap();
                tokens.push(Token::Number(number));
            }
            _ => {
                return Err(SyntaxError::new_lex_error(format!(
                    "Unrecognized character {}",
                    ch
                )))
            }
        }
    }

    tokens.push(Token::End);

    Ok(tokens)
}

// Method to get input string from command line
fn get_line() -> String {
    print!("> ");
    std::io::stdout().flush().unwrap();
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_s) => {}
        Err(_e) => {}
    };
    input.trim().to_string()
}

// This function evaluates a climbing parser on the provided code string.
// It tokenizes the code, parses it, and evaluates the resulting abstract syntax tree (AST).
// If successful, it prints the result of the evaluation.
// If an error occurs during tokenization, parsing, or evaluation, it returns an error message.
fn eval_climbing(code: String) -> Result<(), Box<dyn Error>> {
    // Tokenize the code string into a vector of tokens
    let tokens = lex(code)?;
    // Create a mutable iterator over the tokens, allowing peeking at the next element
    let mut token_iter: Peekable<Iter<'_, Token>> = tokens.iter().peekable();
    // Create new ClimbingParser from token_iter
    let mut parser = ClimbingParser::new(&mut token_iter);
    // Perform calculation and handling error in the parser
    let result = parser.parse();
    match result {
        Ok(mut ast) => println!("{}", ast.eval()),
        Err(e) => return Err(Box::new(e)),
    }

    Ok(())
}

// Get Input from command line and handle input string by climbing parse algorithm
fn run() -> Result<(), Box<dyn Error>> {
    loop {
        let line = get_line();
        if line == "quit" {
            break ();
        }
        if let Err(e) = eval_climbing(line) {
            println!("Error: {}", e);
        }
    }
    Ok(())
}

// Entry Point of main script
fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
    }
}