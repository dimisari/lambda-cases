# lamda-cases language description and compiler

## Description
description.pdf

## Create compiler executable
```bash
make lcc
```

## Compile lcases program to executable

```bash
./lcc hello_world.lc
```

Creates executable "hello_world"

```bash
./hello_world
```

## Compile lcases program to Haskell
```bash
./lcc -h hello_world.lc
```

Creates executable "hello_world.hs"

```bash
ghci hello_world.hs
```

## Run test_inputs
```bash
make
```
Creates "test_outputs" directory where
- test_outputs/compiled_progs
<br /> contains the compiled executables of all "test_inputs/programs"
- test_outputs/programs
<br /> contains the Haskell translation of all
"test_inputs/programs"
- test_outputs/grammar_rules
<br /> contains the Haskell translation of various
examples for each grammar rule from "test_inputs/grammar_rules"

also create "grules" executable that is run for "test_outputs/grammar_rules"

## Clean
```bash
make clean
```
Removes: test_outputs, lcc, grules, hello_world, hello_world.hs

## License
[GPL-2.0 license](LICENSE)
