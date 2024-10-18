class A'Is's_First b0 b1 where
  a'first :: b1 -> b0

class A'Has_String_Repr b19 where
  a'to_string :: b19 -> P.String

class A'Has_A_Wrapper b19 where
  wrap' :: a1 -> b19 a1

class A'Has_Internal_App b19 where
  apply'inside' :: (a1 -> a2, b19 a1) -> b19 a2

class A'Is's_First_One b0 b1 where
  a'first1 :: b1 -> b0

class A'Is's_First_Two b0 b1 where
  a'first2 :: b1 -> b0

class A'Has_String_Repr_One b19 where
  a'to_string1 :: b19 -> P.String

class Show' b19 where
  show :: b19 -> P.String

class A''To' b0 b1 b2 where
  ab_to_c :: (b0, b1) -> b2

