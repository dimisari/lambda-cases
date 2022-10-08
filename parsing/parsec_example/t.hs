import Text.Parsec
  ( parse, ParseError, (<|>), try
  , space, char, letter, string, eof
  , many, many1)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

-- Helpers

[ letters, spaces1 ] =
  map many1 [ letter, space ]
  :: [ Parser String ]

spaces_then =
    ( spaces1 *> )
    :: Parser a -> Parser a

(.>) =
  flip (.)
  :: (a -> b) -> (b -> c) -> a -> c

-- Statement parsers

data Statement =
  Print String | Assign String Int | Add String String | While String [ Statement ]
  deriving (Eq, Show)

[ printp, assignment, addition, while, statement ] = 
  [ Print  <$> ( string "print" *> spaces_then letters )
  , Assign <$> letters <*> ( spaces_then ( char '=' ) *> spaces_then int )
  , Add    <$> letters <*> ( spaces_then ( string "+=" ) *> spaces_then letters )
  , While  <$> ( string "while" *> spaces_then letters ) <*> positive_then_while_body 
  , try printp <|> try assignment <|> try addition <|> while ]
  :: [ Parser Statement ]

-- Statements parsers

[ statements, positive_then_while_body ] = 
  [ many ( statement <* spaces1 )
  , spaces_then ( string "positive {" ) *> spaces_then statements <* char '}' ]
  :: [ Parser [ Statement ] ]

parse_string =
  parse ( statements <* eof ) "temp.in"
  :: String -> Either ParseError [ Statement ]

main =
  readFile "temp.in" >>= parse_string .> print
  :: IO ()
