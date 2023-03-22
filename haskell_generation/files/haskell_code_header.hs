{-# language LambdaCase #-}

class HasFirst a b where
  get_1st :: a -> b

instance HasFirst (a, b) a where
  get_1st = \(a, _) -> a

instance HasFirst (a, b, c) a where
  get_1st = \(a, _, _) -> a

instance HasFirst (a, b, c, d) a where
  get_1st = \(a, _, _, _) -> a

instance HasFirst (a, b, c, d, e) a where
  get_1st = \(a, _, _, _, _) -> a

class HasSecond a b where
  get_2nd :: a -> b

instance HasSecond (a, b) b where
  get_2nd = \(_, b) -> b

instance HasSecond (a, b, c) b where
  get_2nd = \(_, b, _) -> b

instance HasSecond (a, b, c, d) b where
  get_2nd = \(_, b, _, _) -> b

instance HasSecond (a, b, c, d, e) b where
  get_2nd = \(_, b, _, _, _) -> b

class HasThird a b where
  get_3rd :: a -> b

instance HasThird (a, b, c) c where
  get_3rd = \(_, _, c) -> c

instance HasThird (a, b, c, d) c where
  get_3rd = \(_, _, c, _) -> c

instance HasThird (a, b, c, d, e) c where
  get_3rd = \(_, _, c, _, _) -> c

class HasFourth a b where
  get_4th :: a -> b

instance HasFourth (a, b, c, d) d where
  get_4th = \(_, _, _, d) -> d

instance HasFourth (a, b, c, d, e) d where
  get_4th = \(_, _, _, d, _) -> d

class HasFifth a b where
  get_5th :: a -> b

instance HasFifth (a, b, c, d, e) e where
  get_5th = \(_, _, _, _, e) -> e

