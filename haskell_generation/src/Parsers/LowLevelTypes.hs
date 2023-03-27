module Parsers.LowLevelTypes where

import Text.Parsec
import Text.Parsec.String (Parser)

import HaskellTypes.LowLevelTypes (TypeName(..))

-- TypeName: type_name_p
 
type_name_p =
  upper >>= \initial_upper -> many (lower <|> upper) >>= \lowers_uppers ->
  return $ TN (initial_upper : lowers_uppers)
  :: Parser TypeName
