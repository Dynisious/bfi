//! A Brainfuck intepreter
//! 
//! Author --- DMorgan  
//! Last Modified --- 2020-06-03

#![deny(missing_docs,)]

use nummap::NumMap;
use std::io;

pub mod parsing;

/// The state of a Brainfuck programs memory.
/// 
/// The tape of cell values and the pointer to the active cell.
pub type CompState = (NumMap<usize, u8,>, usize,);

/// Interperats and executes a Brainfuck program.
/// 
/// The state of the computation is returned.
/// 
/// # Params
/// 
/// program --- The program to enterperate.
/// input --- The input to read from.  
/// output --- The output to write too.  
pub fn execute<I, O,>(program: &[u8], mut input: I, mut output: O,) -> io::Result<CompState>
  where I: io::Read,
    O: io::Write, {
  use parsing::Brainfuck;

  //Initialise the program state.
  let mut state = (NumMap::new(), 0,);
  //The program yet to be interperated.
  let mut to_interp = program;
  //Process each instruction.
  loop {
    //Parse an instruction.
    let (instruction, remainder,) = match parsing::parse_one(to_interp,) {
      Ok(v) => v,
      Err(e) => {
        let index = program.len() - e.0.len();

        return Err(io::Error::new(
          io::ErrorKind::InvalidData,
          //Return the index of the byte that failed.
          format!("interperatation failed at byte {} `{}`", index, (program[index] as char).escape_default(),),
        ))
      },
    };
    //If there is no instruction halt.
    let instruction = if let Some(v) = instruction { v }
      else { break };
    //Store the remaining uninterperated program.
    to_interp = remainder;

    //Execute the instruction.
    Brainfuck::execute(&instruction, &mut state, &mut input, &mut output,)?;
  }
  
  Ok(state)
}

#[cfg(test)]
mod tests {
  #[test]
  fn basic_instructions() {
    let mut buffer = [0u8; 1];
    let mut output = buffer.as_mut();
    let program = b"+-><[],.";

    crate::execute(program.as_ref(), b"a".as_ref(), &mut output,).expect("execution failed");
    assert_eq!(buffer.as_ref(), b"a",);
  }
  #[test]
  fn complex_instructions() {
    let mut buffer = [0u8; 1];
    let mut output = buffer.as_mut();
    let program = b"+[-><],.";

    crate::execute(program.as_ref(), b"a".as_ref(), &mut output,).expect("execution failed");
    assert_eq!(buffer.as_ref(), b"a",);
  }
  #[test]
  fn nested_loops() {
    let mut buffer = [0u8; 1];
    let mut output = buffer.as_mut();
    let program = b"+[-+[-><]+-],.";

    crate::execute(program.as_ref(), b"a".as_ref(), &mut output,).expect("execution failed");
    assert_eq!(buffer.as_ref(), b"a",);
  }
  #[test]
  fn hello_world() {
    let mut buffer = [0u8; 13];
    let mut output = buffer.as_mut();
    let program = b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

    crate::execute(program.as_ref(), std::io::stdin(), &mut output,).expect("execution failed");
    assert_eq!(buffer.as_ref(), b"Hello World!\n",);
  }
  #[test]
  fn dirty_hello_world() {
    let mut buffer = [0u8; 13];
    let mut output = buffer.as_mut();
    let program = b"++++h+++e+[l>++++l[o>++>+++> +++W>+<o<<<-]>+>+>->>+r[<]<-l]>>.>---.++d+++++.!.+++.>>.<\n-.<.++t+.h--is\tcode----.--works------.>>+!.>++.";

    crate::execute(program.as_ref(), std::io::stdin(), &mut output,).expect("execution failed");
    assert_eq!(buffer.as_ref(), b"Hello World!\n",);
  }
}
