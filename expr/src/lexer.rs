use super::token::Token;
use chumsky::Span;
use core::ops::Range;

#[allow(clippy::too_many_lines, clippy::expect_used)]
pub fn scan(prog: &str) -> Vec<(Token, Range<usize>)> {
    let raw_chars = prog.chars().collect::<Vec<_>>();
    let mut text = &raw_chars[..];
    let mut out: Vec<(Token, Range<usize>)> = vec![];
    let mut start = 0;
    while !text.is_empty() {
        match text[0] {
            '>' if text.len() > 1 && text[1] == '=' => {
                out.push((Token::LessThanEquals, start..start + 2));
                start += 2;
                text = &text[2..];
            }
            '>' => {
                out.push((Token::LessThan, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '<' if text.len() > 1 && text[1] == '=' => {
                out.push((Token::GreaterThanEquals, start..start + 2));
                start += 2;
                text = &text[2..];
            }
            '<' => {
                out.push((Token::GreaterThan, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            ';' => {
                out.push((Token::Semicolon, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '(' => {
                out.push((Token::Lpar, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            ',' => {
                out.push((Token::Comma, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            ')' => {
                out.push((Token::Rpar, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            ' ' | '\n' | '\t' => {
                start += 1;
                text = &text[1..];
            }
            '-' if text.len() > 1 && text[1] == '>' => {
                out.push((Token::Rarrow, start..start + 2));
                start += 2;
                text = &text[2..];
            }
            '-' => {
                out.push((Token::Minus, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '*' => {
                out.push((Token::Times, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '+' => {
                out.push((Token::Plus, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '|' => {
                out.push((Token::Pipe, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            'f' if text.len() > 2 && text[1] == 'n' && text[2] == ' ' => {
                out.push((Token::Fn, start..start + 2));
                start += 3;
                text = &text[3..];
            }
            'i' if text.len() > 2 && text[1] == 'f' && text[2] == ' ' => {
                out.push((Token::If, start..start + 2));
                start += 3;
                text = &text[3..];
            }
            'l' if text.len() > 3 && text[1] == 'e' && text[2] == 't' && text[3] == ' ' => {
                out.push((Token::Let, start..start + 3));
                start += 4;
                text = &text[4..];
            }
            't' if text.len() > 4
                && text[1] == 'h'
                && text[2] == 'e'
                && text[3] == 'n'
                && text[4] == ' ' =>
            {
                out.push((Token::Then, start..start + 4));
                start += 5;
                text = &text[5..];
            }
            't' if text.len() > 3 && text[1] == 'y' && text[2] == 'p' && text[3] == 'e' => {
                out.push((Token::Type, start..start + 4));
                start += 4;
                text = &text[4..];
            }
            'e' if text.len() > 4
                && text[1] == 'l'
                && text[2] == 's'
                && text[3] == 'e'
                && text[4] == ' ' =>
            {
                out.push((Token::Else, start..start + 4));
                start += 5;
                text = &text[5..];
            }
            't' if text.len() > 3 && text[1] == 'r' && text[2] == 'u' && text[3] == 'e' => {
                out.push((Token::True, start..start + 4));
                start += 4;
                text = &text[4..];
            }
            'f' if text.len() > 4
                && text[1] == 'a'
                && text[2] == 'l'
                && text[3] == 's'
                && text[4] == 'e' =>
            {
                out.push((Token::False, start..start + 5));
                start += 5;
                text = &text[5..];
            }
            'm' if text.len() > 4
                && text[1] == 'a'
                && text[2] == 't'
                && text[3] == 'c'
                && text[4] == 'h' =>
            {
                out.push((Token::Match, start..start + 5));
                start += 5;
                text = &text[5..];
            }
            'i' if text.len() > 2 && text[1] == 'n' && text[2] == ' ' => {
                out.push((Token::In, start..start + 2));
                start += 3;
                text = &text[3..];
            }
            '=' if text.len() > 2 && text[1] == '=' => {
                out.push((Token::DoubleEqs, start..start + 2));
                start += 2;
                text = &text[2..];
            }
            '=' => {
                out.push((Token::Eqs, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '{' => {
                out.push((Token::LBrace, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            '}' => {
                out.push((Token::RBrace, start..start + 1));
                start += 1;
                text = &text[1..];
            }
            ':' => {
                out.push((Token::Colon, start..start + 1));
                start += 1;
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
                    let span = start..start + buf.len();
                    start += buf.len();
                    out.push(
                        if buf
                            .chars()
                            .next()
                            .expect("the buffer can't be empty")
                            .is_ascii_uppercase()
                        {
                            (Token::Constructor(buf), span)
                        } else {
                            (Token::Identifier(buf), span)
                        },
                    );
                } else if c.is_numeric() {
                    let mut buf = String::new();
                    while !text.is_empty() && (text[0].is_numeric() || text[0] == '.') {
                        buf.push(text[0]);
                        text = &text[1..];
                    }
                    let span = start..start + buf.len();
                    start += buf.len();
                    out.push((Token::Number(
                        buf.parse()
                            .expect("constructed `buf` out of only numbers and '.'s"),
                    ), span));
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
