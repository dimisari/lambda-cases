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

-- main

main = print res

-- Generated

data Rod =
  Cleft | Cmiddle | Cright
  deriving Show

data Move =
  CMove { get_from :: Rod, get_to :: Rod }
  deriving Show

data NonEmptyMoves =
  CNonEmptyMoves { get_move1 :: Move, get_other_moves :: Moves }
  deriving Show

data Moves =
  Cmoves NonEmptyMoves | Cno_moves
  deriving Show

hanoi :: Rod -> Rod -> Rod -> Int -> Moves
hanoi = \source target auxiliary -> \case
  1 -> Cmoves 
    (CNonEmptyMoves (
    (CMove (source) (target))) (Cno_moves))
  rings -> 
    let    
    moves1 :: Moves
    moves1 = hanoi source auxiliary target (rings - 1)

    moves2 :: Moves
    moves2 = hanoi auxiliary target source (rings - 1)
    in
    add_moves2 moves1 (Cmoves 
      (CNonEmptyMoves (
      (CMove (source) (target))) (moves2)))

add_moves2 :: Moves -> Moves -> Moves
add_moves2 = \case
  Cmoves value@(CNonEmptyMoves move1 other_moves) -> \moves2 -> Cmoves 
    (CNonEmptyMoves (move1) (add_moves2 other_moves moves2))
  Cno_moves -> \moves2 -> moves2

res :: Moves
res = hanoi Cleft Cright Cmiddle 3
