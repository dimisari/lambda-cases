import Text.Parsec
import Text.Parsec.String (Parser)

data AddInts = 
  SimplyInt Int | AddInts AddInts Int
  deriving Show

int_p = read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  :: Parser Int

my_chainl :: Parser Int -> Parser (AddInts -> Int -> AddInts) -> Parser AddInts
my_chainl pa op = pa >>= \a -> rest (SimplyInt a) pa op

rest ::
  AddInts -> Parser Int -> Parser (AddInts -> Int -> AddInts) -> Parser AddInts
rest x pa op = ( op >>= \f -> pa >>= \y -> rest (f x y) pa op ) <|> return x

plus_p = try (string " + ") *> return AddInts
  :: Parser (AddInts -> Int -> AddInts)

add_ints_p = my_chainl int_p plus_p
  :: Parser AddInts

my_parse = ( \p s -> parse p "" s )
  :: Parser a -> String -> Either ParseError a

parse_add_ints = my_parse add_ints_p
  :: String -> Either ParseError AddInts
