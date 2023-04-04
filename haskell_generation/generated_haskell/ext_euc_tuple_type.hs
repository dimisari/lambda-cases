{-# language LambdaCase #-}

-- AllButFirst

class AllButFirst a b where
  get_all_but_1st :: a -> b

instance AllButFirst (a, b) b where
  get_all_but_1st = \(_, b) -> b

instance AllButFirst (a, b, c) (b, c) where
  get_all_but_1st = \(_, b, c) -> (b, c)

instance AllButFirst (a, b, c, d) (b, c, d) where
  get_all_but_1st = \(_, b, c, d) -> (b, c, d)

instance AllButFirst (a, b, c, d, e) (b, c, d, e) where
  get_all_but_1st = \(_, b, c, d, e) -> (b, c, d, e)

-- HasFirst

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

-- HasSecond

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

-- HasThird

class HasThird a b where
  get_3rd :: a -> b

instance HasThird (a, b, c) c where
  get_3rd = \(_, _, c) -> c

instance HasThird (a, b, c, d) c where
  get_3rd = \(_, _, c, _) -> c

instance HasThird (a, b, c, d, e) c where
  get_3rd = \(_, _, c, _, _) -> c

-- HasFourth

class HasFourth a b where
  get_4th :: a -> b

instance HasFourth (a, b, c, d) d where
  get_4th = \(_, _, _, d) -> d

instance HasFourth (a, b, c, d, e) d where
  get_4th = \(_, _, _, d, _) -> d

-- HasFifth

class HasFifth a b where
  get_5th :: a -> b

instance HasFifth (a, b, c, d, e) e where
  get_5th = \(_, _, _, _, e) -> e

-- Generated

data PrevCoeffs =
  CPrevCoeffs { get_prev_prev :: Int, get_prev :: Int }
  deriving Show

data GcdAndCoeffs =
  CGcdAndCoeffs { get_gcd :: Int, get_a :: Int, get_b :: Int }
  deriving Show

extended_euclidean :: Int -> Int -> GcdAndCoeffs
extended_euclidean = ee_recursion (init_a_coeffs) (init_b_coeffs)

init_a_coeffs :: PrevCoeffs
init_a_coeffs = 
  CPrevCoeffs (1) (0)

init_b_coeffs :: PrevCoeffs
init_b_coeffs = 
  CPrevCoeffs (0) (1)

ee_recursion :: PrevCoeffs -> PrevCoeffs -> Int -> Int -> GcdAndCoeffs
ee_recursion = \a_coeffs b_coeffs x -> \case
  0 -> 
    CGcdAndCoeffs (x) (get_prev_prev a_coeffs) (get_prev_prev b_coeffs)
  y -> 
    ee_recursion (next a_coeffs) (next b_coeffs) (y) (mod x y) where
    next :: PrevCoeffs -> PrevCoeffs
    next = \tuple@(CPrevCoeffs prev_prev prev) -> 
      CPrevCoeffs (prev) (prev_prev - div x y * prev)

