// TODO: Use arena allocator to avoid heap allocation
// TODO: Add CharSequence for more efficient string matching
// TODO: Add AnyChar for any character matching
// TODO: Write a parser for regular expressions

use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum RegExp {
    EmptySet,
    EmptyString,
    Char(char),
    Concat(Box<RegExp>, Box<RegExp>),
    Star(Box<RegExp>),
    Alternation(Box<RegExp>, Box<RegExp>),
    And(Box<RegExp>, Box<RegExp>),
    Complement(Box<RegExp>),
}

impl PartialEq for RegExp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RegExp::EmptySet, RegExp::EmptySet) => true,
            (RegExp::EmptyString, RegExp::EmptyString) => true,
            (RegExp::Char(a), RegExp::Char(b)) => a == b,
            (RegExp::Concat(a1, b1), RegExp::Concat(a2, b2)) => a1 == a2 && b1 == b2,
            (RegExp::Star(a), RegExp::Star(b)) => a == b,
            (RegExp::Alternation(a1, b1), RegExp::Alternation(a2, b2)) => a1 == a2 && b1 == b2,
            (RegExp::And(a1, b1), RegExp::And(a2, b2)) => a1 == a2 && b1 == b2,
            (RegExp::Complement(a), RegExp::Complement(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for RegExp {}

impl Display for RegExp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            RegExp::EmptySet => write!(f, "∅"),
            RegExp::EmptyString => write!(f, "ε"),
            RegExp::Char(c) => write!(f, "{}", c),
            RegExp::Concat(a, b) => {
                if a.is_empty_set() || b.is_empty_set() {
                    write!(f, "∅")
                } else if a.is_empty_string() {
                    write!(f, "{}", b)
                } else if b.is_empty_string() {
                    write!(f, "{}", a)
                } else {
                    write!(f, "{}{}", a, b)
                }
            }
            RegExp::Star(r) => {
                if r.is_empty_set() {
                    write!(f, "ε")
                } else {
                    write!(f, "({})*", r)
                }
            }
            RegExp::Alternation(a, b) => {
                if a.is_empty_set() {
                    write!(f, "{}", b)
                } else if b.is_empty_set() {
                    write!(f, "{}", a)
                } else {
                    write!(f, "({}|{})", a, b)
                }
            }
            RegExp::And(a, b) => {
                if a.is_empty_set() || b.is_empty_set() {
                    write!(f, "∅")
                } else {
                    write!(f, "({}&{})", a, b)
                }
            }
            RegExp::Complement(a) => write!(f, "!({})", a),
        }
    }
}

impl RegExp {
    /// Returns true if the regular expression matches the given string.
    pub fn matches(&self, s: &str) -> bool {
        match &self {
            RegExp::EmptySet => false,
            RegExp::EmptyString => s.len() == 0,
            RegExp::Char(c) => s.len() == 1 && s.chars().next().unwrap() == *c,
            RegExp::Concat(a, b) => {
                for i in 0..s.len() {
                    if a.matches(&s[..i]) && b.matches(&s[i..]) {
                        return true;
                    }
                }
                false
            }
            RegExp::Star(r) => {
                if s.len() == 0 {
                    return true;
                }
                for i in 1..s.len() {
                    if r.matches(&s[..i]) && self.matches(&s[i..]) {
                        return true;
                    }
                }
                r.matches(s)
            }
            RegExp::Alternation(a, b) => a.matches(s) || b.matches(s),
            RegExp::And(a, b) => a.matches(s) && b.matches(s),
            RegExp::Complement(a) => !a.matches(s),
        }
    }

    /// Returns true if the regular expression matches the empty string.
    pub fn is_nullable(&self) -> bool {
        match &self {
            RegExp::EmptySet => false,
            RegExp::EmptyString => true,
            RegExp::Char(_) => false,
            RegExp::Concat(a, b) => a.is_nullable() && b.is_nullable(),
            RegExp::Star(_) => true,
            RegExp::Alternation(a, b) => a.is_nullable() || b.is_nullable(),
            RegExp::And(a, b) => a.is_nullable() && b.is_nullable(),
            RegExp::Complement(a) => !a.is_nullable(),
        }
    }

    /// Returns true if the regular expression does not match any strings.
    pub fn is_empty_set(&self) -> bool {
        match &self {
            RegExp::EmptySet => true,
            RegExp::EmptyString => false,
            RegExp::Char(_) => false,
            RegExp::Concat(a, b) => a.is_empty_set() || b.is_empty_set(),
            RegExp::Star(_) => false,
            RegExp::Alternation(a, b) => a.is_empty_set() && b.is_empty_set(),
            RegExp::And(a, b) => a.is_empty_set() || b.is_empty_set(),
            RegExp::Complement(a) => a.is_empty_set(),
        }
    }

    /// Returns true if the empty string is the only string that the regular expression matches.
    pub fn is_empty_string(&self) -> bool {
        match &self {
            RegExp::EmptySet => false,
            RegExp::EmptyString => true,
            RegExp::Char(_) => false,
            RegExp::Concat(a, b) => a.is_empty_string() && b.is_empty_string(),
            RegExp::Star(a) => a.is_empty_set(),
            RegExp::Alternation(a, b) => {
                (a.is_empty_string() && b.is_empty_set())
                    || (a.is_empty_set() && b.is_empty_string())
                    || (a.is_empty_string() && b.is_empty_string())
            }
            RegExp::And(a, b) => {
                (a.is_empty_string() && b.is_nullable()) || (a.is_nullable() && b.is_empty_string())
            }
            RegExp::Complement(_) => false, // TODO: this should be true only when a is ΣΣ*
        }
    }

    /// Compute the derivative of the regular expression with respect to the given character.
    pub fn derivative(&self, c: char) -> RegExp {
        match &self {
            RegExp::EmptySet => RegExp::EmptySet,
            RegExp::EmptyString => RegExp::EmptySet,
            RegExp::Char(c2) => {
                if c == *c2 {
                    RegExp::EmptyString
                } else {
                    RegExp::EmptySet
                }
            }
            RegExp::Concat(a, b) => {
                let ad = a.derivative(c);
                let bd = b.derivative(c);
                let v = if a.is_nullable() {
                    RegExp::EmptyString
                } else {
                    RegExp::EmptySet
                };
                RegExp::Alternation(
                    Box::new(RegExp::Concat(Box::new(ad), b.clone())),
                    Box::new(RegExp::Concat(Box::new(v), Box::new(bd))),
                )
            }
            RegExp::Star(r) => {
                let rd = r.derivative(c);
                RegExp::Concat(Box::new(rd), Box::new(self.clone()))
            }
            RegExp::Alternation(a, b) => {
                let ad = a.derivative(c);
                let bd = b.derivative(c);
                RegExp::Alternation(Box::new(ad), Box::new(bd))
            }
            RegExp::And(a, b) => {
                let ad = a.derivative(c);
                let bd = b.derivative(c);
                RegExp::And(Box::new(ad), Box::new(bd))
            }
            RegExp::Complement(a) => {
                let ad = a.derivative(c);
                RegExp::Complement(Box::new(ad))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! check_matches {
        ($re:expr, { $($input:expr => $expected:expr),* $(,)? }) => {
            let re = $re;
            $(
                assert_eq!(re.matches($input), $expected);
            )*
        };
    }

    #[test]
    fn test_matches_empty_set() {
        check_matches!(RegExp::EmptySet, {
            "" => false,
            "a" => false,
        });
    }

    #[test]
    fn test_matches_empty_string() {
        check_matches!(RegExp::EmptyString, {
            "" => true,
            "a" => false,
        });
    }

    #[test]
    fn test_matches_char() {
        check_matches!(RegExp::Char('a'), {
            "" => false,
            "a" => true,
            "b" => false,
            "aa" => false,
        });
    }

    #[test]
    fn test_matches_concat_chars() {
        check_matches!(RegExp::Concat(Box::new(RegExp::Char('a')), Box::new(RegExp::Char('b'))), {
            "" => false,
            "a" => false,
            "b" => false,
            "ab" => true,
            "ba" => false,
            "abc" => false,
        });
    }

    #[test]
    fn test_matches_concat_empty_set() {
        check_matches!(RegExp::Concat(Box::new(RegExp::EmptySet), Box::new(RegExp::Char('b')),), {
            "" => false,
            "a" => false,
            "b" => false,
            "ab" => false,
            "ba" => false,
            "bb" => false,
        });
    }

    #[test]
    fn test_matches_concat_empty_string() {
        check_matches!(RegExp::Concat(Box::new(RegExp::EmptyString), Box::new(RegExp::Char('b')),), {
            "" => false,
            "a" => false,
            "b" => true,
            "ab" => false,
            "ba" => false,
            "bb" => false,
        });
    }

    #[test]
    fn test_matches_star_char() {
        check_matches!(RegExp::Star(Box::new(RegExp::Char('a'))), {
            "" => true,
            "a" => true,
            "b" => false,
            "aa" => true,
            "ab" => false,
            "ba" => false,
            "abc" => false,
        });
    }

    #[test]
    fn test_matches_star_concat() {
        check_matches!(RegExp::Star(Box::new(RegExp::Concat(Box::new(RegExp::Char('a')), Box::new(RegExp::Char('b'))))), {
            "" => true,
            "a" => false,
            "b" => false,
            "ab" => true,
            "ba" => false,
            "abc" => false,
            "abab" => true,
            "abba" => false,
            "ababab" => true,
        });
    }

    #[test]
    fn test_derivative_char() {
        let re = RegExp::Char('a');
        assert_eq!(re.derivative('a'), RegExp::EmptyString);
        assert_eq!(re.derivative('b'), RegExp::EmptySet);
    }
}
