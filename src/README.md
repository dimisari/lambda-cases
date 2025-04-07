# Root directory of the source code
## Files
### ASTTypes.hs
defines the AST types
### grules.hs: runs parsers for individual AST types for testing
### lcc.hs: contains the main function for the lcc executable (the compiler)
### ShowInstances.hs: Show instances for every AST type
## Subdirectories
### PredefImports: contains stuff to be imported into every lcases program
### Parsing: contains the code that produces the AST
### Generation: contains the code that translates the AST to haskell
