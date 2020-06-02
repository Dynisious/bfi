//! The parsing for brainfuck instructions.
//! 
//! Author --- DMorgan  
//! Last Modified --- 2020-06-03

use crate::CompState;
use std::io::{self, Read, Write,};

/// The control characters for Brainfuck.
pub static CONTROL: &[u8] = b"+-<>[].,";

/// The individual instructions making up a brainfuck program.
#[derive(PartialEq, Eq, Clone, Debug,)]
pub enum Brainfuck {
  /// Moves the pointer. (wrapping)
  Move(isize),
  /// Updates the current cell. (wrapping)
  Update(i8),
  /// Outputs the current character.
  Output,
  /// Reads a character from the input.
  Input,
  /// A looped sequence of instructions.
  Loop(Vec<Self>),
  /// A sequence of instructions.
  Sequence(Vec<Self>),
}

impl Brainfuck {
  /// Executes the instruction.
  /// 
  /// # Params
  /// 
  /// state --- The computation state.  
  /// input --- The input to read from.  
  /// output --- The output to write too.  
  pub fn execute<I, O,>(&self, state: &mut CompState, input: &mut I, output: &mut O,) -> io::Result<()>
    where I: Read,
      O: Write, {
    use Brainfuck::*;

    match self {
      //Shift the pointer.
      Move(shift) => state.1 = state.1.wrapping_add(*shift as usize,),
      //Update the cell.
      Update(change) => { state.0.update(state.1, |cell,| *cell = cell.wrapping_add(*change as u8,),); },
      //Write the byte to the output.
      Output => output.write_all(&[state.0.get(&state.1,)],)?,
      //Read the byte from the input.
      Input => {
        let byte = input.bytes().next().unwrap_or(Err(io::ErrorKind::UnexpectedEof.into()),)?;

        state.0.set(state.1, byte,);
      },
      //Execute the sequence of instructions as long as the current cell is non zero.
      Loop(seq) => while state.0.get(&state.1,) != 0 {
        execute_sequence(seq, state, input, output,)?
      },
      //Execute the sequence of instructions.
      Sequence(seq) => { return execute_sequence(seq, state, input, output,) },
    }

    Ok(())
  }
  /// Decompiles the instruction.
  /// 
  /// # Params
  /// 
  /// output --- The output to write too.  
  pub fn decompile<O,>(&self, output: &mut O,) -> io::Result<()>
    where O: Write, {
    use Brainfuck::*;

    match self {
      Move(shift) => {
        let shifts = std::iter::repeat(if *shift < 0 { &[b'<'] } else { &[b'>'] },)
          .take(*shift as usize,);

        for c in shifts { output.write_all(c,)? }
      },
      Update(change) => {
        let changes = std::iter::repeat(if *change < 0 { &[b'-'] } else { &[b'+'] },)
          .take(*change as isize as usize,);
        
        for c in changes { output.write_all(c,)? }
      },
      Output => output.write_all(&[b'.'],)?,
      Input => output.write_all(&[b','],)?,
      Loop(seq) => {
        output.write_all(&[b'['],)?;
        for instruction in seq { instruction.decompile(output,)? }
        output.write_all(&[b']'],)?;
      },
      Sequence(seq) => for instruction in seq { instruction.decompile(output,)? },
    }

    Ok(())
  }
}

/// Executes a sequence of instructions.
/// 
/// # Params
/// 
/// seq --- The instructions sequence.  
/// state --- The computation state.  
/// input --- The input to read from.  
/// output --- The output to write too.  
fn execute_sequence<I, O,>(seq: &[Brainfuck], state: &mut CompState, input: &mut I, output: &mut O,) -> io::Result<()>
  where I: Read,
    O: Write, {
  //Execute the sequence of instructions.
  for instruction in seq {
    instruction.execute(state, input, output,)?;
  }

  Ok(())
}

/// Attempts to parse a single simple instruction from the input.
/// 
/// # Params
/// 
/// program --- The program to interperate.  
fn parse_simple(program: &[u8],) -> Result<(Option<Brainfuck>, &[u8],), ParseError> {
  use Brainfuck::*;
  //Get the next character to interperate.
  let (ch, mut remainder,) =  match program.split_first() {
    Some(v) => v,
    None => return Ok((None, program,)),
  };
  //Convert the character into an instruction.
  let instruction = match ch {
    b'>' => Move(1),
    b'<' => Move(-1),
    b'+' => Update(1),
    b'-' => Update(-1),
    b'.' => Output,
    b',' => Input,
    //Start interperating a loop.
    b'[' => {
      let mut seq = Vec::new();

      //Interperate instructions in a loop until the matching close bracket is encountered.
      loop { match parse_one(remainder,) {
        //We read an instruction.
        Ok((Some(instruction), rem,)) => {
          seq.push(instruction);
          remainder = rem;
        },
        //There are no more instructions and no closing brace was encountered.
        Ok((None, _,)) => return Err(ParseError(program)),
        //We encountered the closing brace.
        Err(e) if e.0.first() == Some(&b']') => { remainder = &e.0[1..]; break },
        //We encountered an error while parsing.
        Err(e) => return Err(e),
      } }

      //If the loop is empty, ignore it and get the next instruction.
      if seq.is_empty() { return parse_simple(remainder,) }

      Loop(seq)
    },
    //A closing brace should never be the next instruction. (unless we are in the middle of interperating a loop)
    b']' => return Err(ParseError(program)),
    //Any other character is ignored.
    _ => return parse_one(remainder,),
  };

  Ok((Some(instruction), remainder,))
}

/// Attempts to parse a single instruction from the input.
/// 
/// Optimisations are applied to the interperatation such as merging sequences of moves
/// or increments into single instructions.
/// 
/// # Params
/// 
/// program --- The program to interperate.  
pub fn parse_one(program: &[u8],) -> Result<(Option<Brainfuck>, &[u8],), ParseError> {
  use Brainfuck::*;

  //Read a simple instruction.
  let (mut instruction, mut program,) = match parse_simple(program,)? {
    (None, rem,) => return Ok((None, rem,)),
    (Some(v), rem,) => (v, rem,),
  };

  //Optimise the instruction.
  match &mut instruction {
    //Combine shifts into a single shift instruction.
    Move(shift) => {
      //Iterate over the remaining bytes ignoring all non control bytes.
      let mut remaining = program.iter().cloned().enumerate().filter(|(_, c,),| CONTROL.contains(c,),).peekable();

      //Accumulate all shifts.
      while let Some(v) = remaining.peek()
        //Accept only shifts.
        .and_then(|&(_, c,),| if c == b'>' { Some(1) } else if c == b'<' { Some(-1) } else { None },) {
        *shift = shift.wrapping_add(v,);
        //Consume the used byte.
        remaining.next();
      }

      //Consume all of the used bytes.
      program = &program[remaining.peek().map(|e,| e.0,).unwrap_or(program.len(),)..];

      //If the shifts have canceled out, skip this instruction.
      if *shift == 0 { return parse_one(program,) }
    },
    //Combine cell updates into a single update instruction.
    Update(change) => {
      //Iterate over the remaining bytes ignoring all non control bytes.
      let mut remaining = program.iter().cloned().enumerate().filter(|(_, c,),| CONTROL.contains(c,),).peekable();

      //Accumulate all updates.
      while let Some(v) = remaining.peek()
        //Accept only updates.
        .and_then(|&(_, c,),| if c == b'+' { Some(1) } else if c == b'-' { Some(-1) } else { None },) {
        *change = change.wrapping_add(v,);
        //Consume the used byte.
        remaining.next();
      }

      //Consume all of the used bytes.
      program = &program[remaining.peek().map(|e,| e.0,).unwrap_or(program.len(),)..];

      //If the shifts have canceled out, skip this instruction.
      if *change == 0 { return parse_one(program,) }
    },
    _ => (),
  }

  Ok((Some(instruction), program,))
}

/// Attempts to parse a sequence of instructions from the input.
/// 
/// Optimisations are applied to the interperatation such as merging sequences of moves
/// or increments into single instructions.
/// 
/// # Params
/// 
/// program --- The program to interperate.  
pub fn parse(program: &[u8],) -> Result<Brainfuck, ParseError> {
  //The uninterperated program.
  let mut to_interp = program;
  let mut seq = Vec::new();

  //Read all instructions.
  while let (Some(instruction), rem,) = parse_one(to_interp,)? {
    seq.push(instruction);
    to_interp = rem;
  }

  //Only bother with a sequence if there is more than one instruction.
  if seq.len() <= 1 { seq.pop().ok_or(ParseError(program)) }
  else { Ok(Brainfuck::Sequence(seq)) }
}

/// The program state which caused the error.
#[derive(PartialEq, Eq, Clone, Copy, Debug,)]
pub struct ParseError<'a,>(pub &'a [u8],);

impl From<ParseError<'_,>> for io::Error {
  fn from(from: ParseError,) -> Self {
    io::Error::new(
      io::ErrorKind::InvalidData,
      format!("failed to parse instruction, unused input: {:?}", from.0,),
    )
  }
}
