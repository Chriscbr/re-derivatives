use re_derivatives::RegExp;

pub fn main() {
    let re = RegExp::Star(Box::new(RegExp::Concat(
        Box::new(RegExp::Concat(
            Box::new(RegExp::Char('a')),
            Box::new(RegExp::Char('b')),
        )),
        Box::new(RegExp::Char('c')),
    )));
    println!("r: {}", re);

    let re2 = re.derivative('a');
    println!("d(r, 'a'): {}", re2);

    let re3 = re.derivative('b');
    println!("d(r, 'b'): {}", re3);

    let re4 = re2.derivative('b');
    println!("d(d(r, 'a'), 'b'): {}", re4);
}
