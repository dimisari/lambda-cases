module Predefined where

type FromIO = IO
type OnlyIO = IO ()

print_line = putStrLn
get_line = getLine
split_words = words
apply'to_all = map

from_string :: Read a => String -> a
from_string = read

wrap :: Monad m => a -> m a
wrap = return
throw_err = error
