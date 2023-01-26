parsec:
- <?> for better parsing error messages!
- string literal: char '"' *> many (noneOf ['"']) <* char '"'

Questions:
- mutual recursion: need to load whole level?
- String = ListOf(Char)s ??

Tuple Application:
- f: A, B *> C
  pair: ( A, B )
  pair==>f: C

Function composition:
- not built-in
- given:
  - f0: Type0 -> Type1 
  - f1: Type1 -> Type2 
- we have:
  f2: Type0 -> Type2
    = f0 o> f1 

Compilation targets to consider:
- Haskell Core
- LLVM IR
- WebAssembly
- BEAM (Erlang virtual machine)
- RISC-V
- Java bytecode
- Python IR ?

Other:
- graph reduction
