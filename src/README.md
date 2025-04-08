# Files

## ASTTypes.hs
defines the AST types
## grules.hs
runs parsers for individual AST types for testing
## lcc.hs
contains the main function of the compiler
## ShowInstances.hs
defines Show instances for every AST type for pretty printing ASTs

# Subdirectories

## PredefImports
contains main libraries that are imported into every lcases program
## Parsing
contains parsing code that produces the AST
## Generation
contains the code that translates the AST to haskell
