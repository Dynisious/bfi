//! A Brainfuck intepreter
//! 
//! Author --- DMorgan  
//! Last Modified --- 2020-06-03

use std::io;

fn main() -> io::Result<()> {
  bfi::parsing::parse(b"+[,.]",)?
  .execute(
    &mut bfi::CompState::default(),
    &mut io::stdin(), &mut io::stdout(),
  )
}
