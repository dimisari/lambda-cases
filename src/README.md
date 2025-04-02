# Root directory of the source code
## Subdirectories
### PredefImports: contains stuff to be imported into every lcases program
### Parsing: contains the code that produced the AST
### Generation: contains the code that translated the AST to haskell
## Files
### ASTTypes.hs: defines the AST types
### grules.hs: runs parsers for individual AST types for testing
### lcc.hs: contains the main function for the lcc executable (the compiler)
### ShowInstances.hs: Show instances for every  AST type
