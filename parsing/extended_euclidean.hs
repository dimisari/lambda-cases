import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

-- Helpers

[ letters, spaces1 ] = map many1 [ letter, space ]
  :: [ Parser String ]

spaces_then = ( spaces1 *> )
  :: Parser a -> Parser a

(.>) = flip (.)
  :: (a -> b) -> (b -> c) -> a -> c

parse_with = flip parse "" 
  :: Parser a -> String -> Either ParseError a

-- Type after parsing

data Tuple = T { get_tuple_name :: String, get_combiner :: Combiner }
  deriving (Eq, Show)

data Combiner = C { get_combiner_name :: String, get_combiner_pairs :: [ NameAndType ] }
  deriving (Eq, Show)

data NameAndType = CP { get_name :: String, get_type :: TypeExpr }
  deriving (Eq, Show)

data LowestTypeExpr = IntType | Parenthesis TypeExpr deriving (Eq, Show)

data TypeExpr =
  IntType | ProductType [ TypeExpr ] | TypeWithVariableName String TypeExpr |
  FunctionType TypeExpr TypeExpr
  deriving (Eq, Show)

parse_int :: Parser TypeExpr 
parse_int = string "Int" >> return IntType

parse_comma_seperated :: Parser TypeExpr -> Parser [ TypeExpr ]
parse_comma_seperated = \parse_type_expr -> sepBy1 parse_type_expr (string ", ")

parse_product_parser :: Parser TypeExpr -> Parser TypeExpr 
parse_product_parser = parse_comma_seperated .> fmap ProductType

data ValueExpr =
  ValueName String | Paren ValueExpr | Product [ ValueExpr ] |
  LeftApplication ValueExpr ValueExpr | RightApplication ValueExpr ValueExpr |
  Abstraction String ValueExpr |
  Case [ (ValueExpr, ValueExpr) ]
  deriving (Eq, Show)

data AssignmentExpr = NameAndValue String ValueExpr  deriving (Eq, Show)

data TypeAssignmentExpr = TypeAndAssignment TypeExpr AssignmentExpr  deriving (Eq, Show)
  
-- Statement parsers

data Statement =
  Print String | Assign String Int | Add String String | While String [ Statement ]
  deriving (Eq, Show)

[ printp, assignment, addition, while, statement ] = 
  [ Print  <$> ( string "print" *> spaces_then letters )
  , Assign <$> letters <*> ( spaces_then ( char '=' ) *> spaces_then int )
  , Add    <$> letters <*> ( spaces_then ( string "+=" ) *> spaces_then letters )
  , While  <$> ( string "while" *> spaces_then letters ) <*> positive_then_while_body 
  , try printp <|> try assignment <|> try addition <|> while
  ] :: [ Parser Statement ]

-- Statements parsers

[ statements, positive_then_while_body ] = 
  [ many ( statement <* spaces1 )
  , spaces_then ( string "positive" ) *>
    spaces_then ( char '{' ) *> spaces_then statements <* spaces_then ( char '}' )
  ] :: [ Parser [ Statement ] ]

parse_string :: String -> Either ParseError [ Statement ]
parse_string = parse ( statements <* eof ) "temp.in"

main = readFile "temp.in" >>= parse_string .> print
