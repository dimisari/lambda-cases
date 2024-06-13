# lamda-cases compiler and language description
## Create compiler executable
make lcc

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
./my_lcases_program
```
