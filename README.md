# lamda-cases language description and compiler

## Description
description.pdf

## Create compiler executable
```bash
make lcc
```

## Compile lcases program to executable

```bash
./lcc my_lcases_program.lc
```

Creates executable "my_lcases_program"

```bash
./my_lcases_program
```
to run

## Compile lcases program to Haskell
```bash
./lcc -h my_lcases_program.lc
```

Creates executable "my_lcases_program.hs"

```bash
ghci my_lcases_program.hs
```

## Run test_inputs
```bash
make
```
creates "test_output" directory where
- "test_output/compiled_progs" contains the compiled executables of all
"test_input/programs"
- "test_output/programs" contains the Haskell translation of all
"test_input/programs"
- "test_output/grammar_rules" contains the Haskell translation of various
examples for each grammar rule from "test_input/grammar_rules"
