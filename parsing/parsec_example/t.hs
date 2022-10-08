import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

-- Helpers

[ letters, spaces1 ] = map many1 [ letter, space ] :: [ Parser String ]
spaces_then = ( spaces1 *> ) :: Parser a -> Parser a
(.>) = flip (.) :: (a -> b) -> (b -> c) -> a -> c

parse_with :: Parser a -> String -> Either ParseError a
parse_with = flip parse "" 

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
