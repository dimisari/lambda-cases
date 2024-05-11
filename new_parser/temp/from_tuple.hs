{-# language MultiParamTypeClasses, FlexibleInstances #-}

class FromTuple3 a b c d where
  from_tuple :: (a, b, c) -> d

instance FromTuple3 a b c (a, b, c) where
  from_tuple = id

-- tuple_type TupleOne(T1)
-- value (x1, x2, x3) : Int x T1 x String

data TupleOne a = TupleOne' { x1 :: Int, x2 :: a, x3 :: String }

instance FromTuple3 Int a String (TupleOne a) where
  from_tuple = \(x1, x2, x3) -> TupleOne' x1 x2 x3

-- tuple_type TupleTwo(T1)
-- value (y1, y2, y3) : Int x T1 x String

data TupleTwo a = TupleTwo' { y1 :: Int, y2 :: a, y3 :: String }

instance FromTuple3 Int a String (TupleTwo a) where
  from_tuple = \(x1, x2, x3) -> TupleTwo' x1 x2 x3

a :: (Int, Char, String)
a = from_tuple (1 :: Int, 'a', "hi")

b :: TupleOne Char
b = from_tuple (1 :: Int, 'a', "hi")

c :: TupleTwo Char
c = from_tuple (1 :: Int, 'a', "hi")
