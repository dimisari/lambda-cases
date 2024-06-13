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

## Compile lcases program to haskell
```bash
./lcc -h my_lcases_program.lc
```

Creates executable "my_lcases_program.hs"

```bash
ghci my_lcases_program.hs
```

