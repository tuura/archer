module Machine.Types

import Data.Fin

%default total
%access public export

Byte : Type
Byte = Bits8

Value : Type
Value = Bits32

addressSpaceSize : Nat
addressSpaceSize = 255

MemoryAddress : Type
MemoryAddress = Fin addressSpaceSize

data Flag = Zero | Halted

data Register = R0 | R1 | R2 | R3


