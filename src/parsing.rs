//! The parsing for brainfuck instructions.
//! 
//! Author --- DMorgan  
//! Last Modified --- 2020-07-14

use crate::CompState;
use parser::*;
use std::{
  io::{self, Read, Write,},
  ops::Try,
};

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

//Skip many non control characters.
static NON_CONTROL: parsers::Skip<&u8, &[u8], ErrorKind, Parser<AsParser<fn(&[u8]) -> ParseResult<Result<&u8, ErrorKind>, &[u8]>>>>
  = parsers::skip(Parser::from_fn(move |program,| {
    //Parse a non control character.
    static NON_CONTROL: Parser<parsers::NextIs<&u8, fn(&&u8) -> bool>>
      = Parser::next_is(|&&c,| CONTROL.iter().all(|&a,| a != c,),);

    NON_CONTROL.parse_slice(program,)
  },),);

/// The parser function for a simple instruction i.e. not a loop.
pub type Simple<'a,> = impl ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]> + Copy;

/// A `Parser` which attempts to parse a single simple instruction from the input.
/// 
/// Any leading non control characters are consumed and ignored. 
pub fn parse_simple<'a,>() -> Parser<Simple<'a,>> {
  static VALUE: &[u8] = b"+-";
  static POINTER: &[u8] = b"<>";
  static IO: &[u8] = b".,";

  //Convert value characters into value increments.
  let value = |&c: &u8,| -> i8 { if c == b'+' { 1 } else { -1 } };
  //Get value increments from input.
  let value = Parser::next().filter_map(|&c,| VALUE.iter().find(|&&a,| c == a,),).map_ok(value,);
  //Sum sequences of value increments.
  let value = Parser::from_fn(move |program,| try {
    //The initial increment.
    let (mut program, mut shift,) = value.parse_slice(program,).map_err(|_,| program,)?;

    //Sum all increments.
    while let (remain, Ok(v),) = value.parse_slice(program,).into() {
      program = remain;
      shift = shift.wrapping_add(v,);
    }

    (program, Brainfuck::Update(shift,),)
  },);

  //Convert pointer characters into pointer shifts.
  let pointer = |&c: &u8,| -> isize { if c == b'>' { 1 } else { -1 } };
  //Get pointer shifts from input.
  let pointer = Parser::next().filter_map(|&c,| POINTER.iter().find(|&&a,| c == a,),).map_ok(pointer,);
  //Sum sequences of pointer shifts.
  let pointer = Parser::from_fn(move |program,| try {
    //Get the initial shift.
    let (mut program, mut shift,) = pointer.parse_slice(program,).map_err(|_,| program,)?;

    //Sum all shifts.
    while let (remain, Ok(v),) = pointer.parse_slice(program,).into() {
      program = remain;
      shift = shift.wrapping_add(v,);
    }

    (program, Brainfuck::Move(shift,),)
  },);

  //Convert IO characters into instructions.
  let output = |&c: &u8,| -> Brainfuck { if c == b'.' { Brainfuck::Output } else { Brainfuck::Input } };
  //Get IO instructions from input.
  let io = Parser::next().filter_map(|&c,| IO.iter().find(|&&a,| c == a,),).map_ok(output,);
  //Coerce the type interface to match.
  let io = Parser::from_fn(move |program,| io.parse_slice(program,).map_err(|_,| program,),);

  //Skip any leading non control characters and merge the parsers.
  Parser(&NON_CONTROL).then(value.or(pointer,).or(io,),)
}

/// Appends the zero or more instances of `value` from the front of `program` to `sequence`.
/// 
/// The remainder of `program` is returned.
/// 
/// # Params
/// 
/// sequence --- The sequence of values to add too.  
/// value --- The value parser to apply.  
/// program --- The input to parse.  
fn parse_sequence<'a, 'b, Value,>(sequence: &'b mut Vec<Brainfuck>, value: &Parser<Value>, mut program: &'a [u8],) -> &'a [u8]
  where Value: ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]>, {
  //Apply `value` as many times as possible.
  while let (remain, Ok(v),) = value.parse(program,).into() {
    program = remain; sequence.push(v,);
  }

  program
}

/// Parses a loop structure from `program` containing instances of `value` or nested loops.
/// 
/// * Any non control characters immediately preceding the open or close brackets are
/// consumed and ignored.
/// * Empty loops are replaced with a `None` result.
/// 
/// # Params
/// 
/// value --- The value parser to apply.  
/// program --- The input to parse.  
fn parse_loop<'a, Value,>(value: &Parser<Value>, program: &'a [u8],) -> ParseResult<Result<Option<Brainfuck>, &'a [u8]>, &'a [u8]>
  where Value: ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]>, {
  //Parses the start of a loop.
  static START_LOOP: Parser<parsers::NextIs<&u8, fn(&&u8) -> bool>> = Parser::next_is(|&&c: &&u8,| c == b'[',);
  //Parses the end of a loop.
  static END_LOOP: Parser<parsers::NextIs<&u8, fn(&&u8) -> bool>> = Parser::next_is(|&&c: &&u8,| c == b']',);

  //Consume any leading non control characters.
  let program = NON_CONTROL.parse(program,).remaining;
  //Parse the start of the loop.
  let program = START_LOOP.parse_slice(program,).map_err(|_,| program,)?.0;

  let mut seq = Vec::new();
  //Parse any leading sequence of instructions.
  let mut program = parse_sequence(&mut seq, value, program,);
  //Attempt to parse any nested loops and trailing instructions.
  let program = loop { match parse_loop(value, program,).into() {
    (remain, Ok(instruction),) => {
      //If the loop was not empty append it.
      if let Some(v) = instruction { seq.push(v,) }

      //Update the remaining input.
      program = parse_sequence(&mut seq, value, remain,)
    },
    //No loops remain.
    (remain, _,) => break remain,
  } };
  //Consume any leading non control characters.
  let program = NON_CONTROL.parse(program,).remaining;
  //Parse the close of the loop.
  let program = END_LOOP.parse_slice(program,).map_err(|_,| program,)?.0;
  //If the loop is empty drop it.
  let result = Ok(if seq.is_empty() { None } else { Some(Brainfuck::Loop(seq)) });

  ParseResult::new(program, result,)
}

/// The parser function for a complex instruction i.e. a simple instruction or a loop.
pub type Complex<'a,> = impl ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]> + Copy;

/// A `Parser` which attempts to parse a single complex instruction from the input.
pub fn parse_complex<'a,>() -> Parser<Complex<'a,>> {
  let value = parse_simple();

  Parser::from_fn(move |mut program,| {
    //Parse a loop, ignoring empty loops.
    while let Ok((remain, instruction,)) = parse_loop(&value, program,).into_result() {
      //Return the non empty loop.
      if let Some(v) = instruction { return ParseResult::new(remain, Ok(v),) }
      //Update the remaining input.
      else { program = remain }
    }

    //Parse a simple value.
    value.parse(program,)
  },)
}

/// The parser function for a full program i.e. either a single complex instruction or
/// seqence of complex instructions.
pub type Program<'a,> = impl ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]> + Copy;

/// A `Parser` which attempts to parse a complete program.
pub fn parse_program<'a,>() -> Parser<Program<'a,>> {
  //Parse a sequence of instructions.
  parse_complex().many1()
  //If the sequence is a single instruction return it alone.
  .map_ok(|mut seq,| if seq.len() > 1 { Brainfuck::Sequence(seq) }
    else { seq.pop().unwrap() },
  )
}

/// The parser function which converts the error from a parse into a more usable `io::Error`.
pub type ErrorParser<'a, P,> = impl ParseFn<&'a [u8], Output = io::Result<Brainfuck>, Remain = &'a [u8]> + Copy;

/// The `Parser` which converts the error from a parse into a more usable `io::Error`.
pub fn error_parser<'a, P,>(parser: Parser<P,>,) -> Parser<ErrorParser<'a, P,>>
  where P: ParseFn<&'a [u8], Output = Result<Brainfuck, &'a [u8]>, Remain = &'a [u8]> + Copy, {
  Parser::from_fn(move |program,| {
    //Map the error.
    parser.parse(program,).map_err(|e,| {
      //Get the index of the interpreter failure.
      let index = program.len() - e.len();
      //Get the character which failed.
      let character = program.get(index).map(|&c,| c as char)
        .unwrap_or(std::char::REPLACEMENT_CHARACTER,);

      io::Error::new(
        io::ErrorKind::InvalidData,
        //Return the index of the byte that failed.
        format!("interperatation failed at byte {} `{}`", index, character.escape_default(),),
      )
    },)
  },)
}
