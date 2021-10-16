use super::token::Token;

pub fn scan(prog: &str) -> Vec<Token> {
    let raw_chars = prog
        .chars().
        collect::<Vec<_>>();
    let mut text = &raw_chars[..];
    let mut out : Vec<Token> = vec![];
    while text.len() > 0 {
        match text[0] {
            '(' => {
                out.push(Token::Lpar);
                text = &text[1..];
            },
            ',' => {
                out.push(Token::Comma);
                text = &text[1..];
            },
            ')' => {
                out.push(Token::Rpar);
                text = &text[1..];
            },
            ' ' | '\n' => text = &text[1..],
            '-' if text.len() > 1 && text[1] == '>' => {
                out.push(Token::Rarrow);
                text = &text[2..];
            },
            'f' if text.len() > 2 && text[1] == 'n' && text[2] == ' ' => {
                out.push(Token::Fn);
                text = &text[3..];
            },
            'l' if text.len() > 3 && text[1] == 'e' && text[2] == 't' && text[3] == ' ' => {
                out.push(Token::Let);
                text = &text[4..];
            },
            'i' if text.len() > 3 && text[1] == 'n' && text[2] == ' ' => {
                out.push(Token::In);
                text = &text[3..];
            },
            '=' => {
                out.push(Token::Eqs);
                text = &text[1..];
            },
            '{' => {
                out.push(Token::LBrace);
                text = &text[1..];
            },
            '}' => {
                out.push(Token::RBrace);
                text = &text[1..];
            },
            ':' => {
                out.push(Token::Colon);
                text = &text[1..];
            },
            c => {
                if c.is_alphabetic() {
                    let mut buf = String::new();
                    while text.len() > 0 
                            && text[0] != ' ' 
                            && text[0] != ')' 
                            && text[0] != '(' 
                            && text[0] != '}' 
                            && text[0] != '{' 
                            && text[0] != ':' 
                            && text[0] != '=' {
                        buf.push(text[0]);
                        text = &text[1..];
                    }
                    out.push(Token::Identifier(buf));
                } else if c.is_numeric() {
                    let mut buf = String::new();
                    while text.len() > 0 && (text[0].is_numeric() || text[0] == '.') {
                        buf.push(text[0]);
                        text = &text[1..];
                    }
                    out.push(Token::Number(buf.parse().unwrap()));
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
        assert_eq!(scan("(fn a -> 5.0) 1"), vec![
                   Lpar, 
                   Fn, 
                   Identifier(String::from("a")), 
                   Rarrow, 
                   Number(5.),
                   Rpar,
                   Number(1.)
        ]);
    }
}
