# AST.hs
This file implements a parser for every type of the AST by implementing an
instance of the HasParser type class

# TypesAndHelpers.hs
This file defines:
- types for parsing
- state parsers: for the indentation level and the equal line
- helper parsers: spaces, commas, equal symbols, arrows, parentheses etc
- parsers for char and string literals
