import Text.Parsec
import Text.Parsec.String (Parser)

data AddInts = 
  SimplyInt Int | AddInts AddInts Int
  deriving Show

int_p =
  read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  :: Parser Int

my_chainl = ( \first_f p op ->
  let
  rest x =
    ( op >>= \f -> p >>= \y -> rest (f x y)) <|>
    return x
  in
  p >>= \x -> 
  rest (first_f x)
  ) :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b

plus_p =
  try (string " + ") *> return AddInts
  :: Parser (AddInts -> Int -> AddInts)

add_ints_p = my_chainl SimplyInt int_p plus_p
  :: Parser AddInts

my_parse = ( \p s -> parse p "" s )
  :: Parser a -> String -> Either ParseError a

parse_add_ints = my_parse add_ints_p
  :: String -> Either ParseError AddInts

