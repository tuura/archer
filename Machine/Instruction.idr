module Machine.Instruction

import Machine.Types

%default total
%access public export

data Instruction : Type where
  Load     : Register -> MemoryAddress -> Instruction
  LoadMI   : Register -> MemoryAddress -> Instruction
  Set      : Register -> Byte -> Instruction
  Store    : Register -> MemoryAddress -> Instruction
  Add      : Register -> MemoryAddress -> Instruction
  Jump     : Byte -> Instruction
  JumpZero : Byte -> Instruction

InstructionAddress : Type
InstructionAddress = Byte

Program : Type
Program = List (InstructionAddress, Instruction)