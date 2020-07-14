//! A Brainfuck intepreter
//! 
//! Author --- DMorgan  
//! Last Modified --- 2020-06-03

use bfi::parsing;
use parser::*;
use std::io;

fn main() -> io::Result<()> {
  parsing::error_parser(parsing::parse_program(),).parse(b"+[,.]",).output?
  .execute(
    &mut bfi::CompState::default(),
    &mut io::stdin(), &mut io::stdout(),
  )
}
