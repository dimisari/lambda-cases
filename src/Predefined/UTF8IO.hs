module Predefined.UTF8IO where

import Prelude qualified as P
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.ByteString.UTF8 qualified as U

import Predefined.Types qualified as PT
import Predefined.Operators ((.>))

read_file' :: P.String -> PT.ProgramWith' P.String
read_file' = BS.readFile .> P.fmap U.toString

write'to_file' :: (P.String, P.String) -> PT.Program
write'to_file' = \(str, f) -> BS.writeFile f (U.fromString str)

append'to_file' :: (P.String, P.String) -> PT.Program
append'to_file' = \(str, f) -> BS.appendFile f (U.fromString str)

print_string' :: P.String -> PT.Program
print_string' = U.fromString .> BS.putStr

get_line :: PT.ProgramWith' P.String
get_line = P.fmap U.toString C.getLine

