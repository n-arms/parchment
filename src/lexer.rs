use super::token::Token;

pub fn scan(prog: &str) -> Vec<Token> {
    let raw_chars = prog.chars().collect::<Vec<_>>();
    let mut text = &raw_chars[..];
    let mut out: Vec<Token> = vec![];
    while !text.is_empty() {
        match text[0] {
            '>' if text.len() > 1 && text[1] == '=' => {
                out.push(Token::LessThanEquals);
                text = &text[2..];
            }
            '>' => {
                out.push(Token::LessThan);
                text = &text[1..];
            }
            '<' if text.len() > 1 && text[1] == '=' => {
                out.push(Token::GreaterThanEquals);
                text = &text[2..];
            }
            '<' => {
                out.push(Token::GreaterThan);
                text = &text[1..];
            }
            ';' => {
                out.push(Token::Semicolon);
                text = &text[1..];
            }
            '(' => {
                out.push(Token::Lpar);
                text = &text[1..];
            }
            ',' => {
                out.push(Token::Comma);
                text = &text[1..];
            }
            ')' => {
                out.push(Token::Rpar);
                text = &text[1..];
            }
            ' ' | '\n' | '\t' => text = &text[1..],
            '-' if text.len() > 1 && text[1] == '>' => {
                out.push(Token::Rarrow);
                text = &text[2..];
            }
            '-' => {
                out.push(Token::Minus);
                text = &text[1..];
            }
            '*' => {
                out.push(Token::Times);
                text = &text[1..];
            }
            '+' => {
                out.push(Token::Plus);
                text = &text[1..];
            }
            '|' => {
                out.push(Token::Pipe);
                text = &text[1..];
            }
            'f' if text.len() > 2 && text[1] == 'n' && text[2] == ' ' => {
                out.push(Token::Fn);
                text = &text[3..];
            }
            'i' if text.len() > 2 && text[1] == 'f' && text[2] == ' ' => {
                out.push(Token::If);
                text = &text[3..];
            }
            'l' if text.len() > 3 && text[1] == 'e' && text[2] == 't' && text[3] == ' ' => {
                out.push(Token::Let);
                text = &text[4..];
            }
            't' if text.len() > 4
                && text[1] == 'h'
                && text[2] == 'e'
                && text[3] == 'n'
                && text[4] == ' ' =>
            {
                out.push(Token::Then);
                text = &text[5..];
            }
            't' if text.len() > 3 
                && text[1] == 'y'
                && text[2] == 'p'
                && text[3] == 'e' =>
            {
                out.push(Token::Type);
                text = &text[4..];
            }
            'e' if text.len() > 4
                && text[1] == 'l'
                && text[2] == 's'
                && text[3] == 'e'
                && text[4] == ' ' =>
            {
                out.push(Token::Else);
                text = &text[5..];
            }
            't' if text.len() > 3 && text[1] == 'r' && text[2] == 'u' && text[3] == 'e' => {
                out.push(Token::True);
                text = &text[4..];
            }
            'f' if text.len() > 4
                && text[1] == 'a'
                && text[2] == 'l'
                && text[3] == 's'
                && text[4] == 'e' =>
            {
                out.push(Token::False);
                text = &text[5..];
            }
            'm' if text.len() > 4
                && text[1] == 'a'
                && text[2] == 't'
                && text[3] == 'c'
                && text[4] == 'h' =>
            {
                out.push(Token::Match);
                text = &text[5..];
            }
            'i' if text.len() > 3 && text[1] == 'n' && text[2] == ' ' => {
                out.push(Token::In);
                text = &text[3..];
            }
            '=' if text.len() > 2 && text[1] == '=' => {
                out.push(Token::DoubleEqs);
                text = &text[2..];
            }
            '=' => {
                out.push(Token::Eqs);
                text = &text[1..];
            }
            '{' => {
                out.push(Token::LBrace);
                text = &text[1..];
            }
            '}' => {
                out.push(Token::RBrace);
                text = &text[1..];
            }
            ':' => {
                out.push(Token::Colon);
                text = &text[1..];
            }
            c => {
                if c.is_alphabetic() {
                    let mut buf = String::new();
                    while !text.is_empty()
                        && text[0] != ' '
                        && text[0] != '\n'
                        && text[0] != '\t'
                        && text[0] != ')'
                        && text[0] != '('
                        && text[0] != '}'
                        && text[0] != '{'
                        && text[0] != ':'
                        && text[0] != ','
                        && text[0] != '='
                        && text[0] != ';'
                    {
                        buf.push(text[0]);
                        text = &text[1..];
                    }
                    out.push(if buf.chars().next().unwrap().is_ascii_uppercase() {
                        Token::Constructor(buf)
                    } else {
                        Token::Identifier(buf)
                    });
                } else if c.is_numeric() {
                    let mut buf = String::new();
                    while !text.is_empty() && (text[0].is_numeric() || text[0] == '.') {
                        buf.push(text[0]);
                        text = &text[1..];
                    }
                    out.push(Token::Number(buf.parse().unwrap()));
                } else {
                    panic!("found illegal char")
                }
            }
        }
    }
    out
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Token::*;

    #[test]
    fn scan_lpar() {
        assert_eq!(scan("("), vec![Lpar]);
    }
    #[test]
    fn scan_rpar() {
        assert_eq!(scan(")"), vec![Rpar]);
    }
    #[test]
    fn scan_rarrow() {
        assert_eq!(scan("->"), vec![Rarrow]);
    }
    #[test]
    fn scan_fn() {
        assert_eq!(scan("fn "), vec![Fn]);
    }
    #[test]
    fn scan_num() {
        assert_eq!(scan("3.14"), vec![Number(3.14)]);
    }
    #[test]
    fn scan_id() {
        assert_eq!(scan("abc123"), vec![Identifier(String::from("abc123"))]);
    }
    #[test]
    fn scan_prog() {
        assert_eq!(
            scan("(fn a -> 5.0) 1"),
            vec![
                Lpar,
                Fn,
                Identifier(String::from("a")),
                Rarrow,
                Number(5.),
                Rpar,
                Number(1.)
            ]
        );
    }
}
