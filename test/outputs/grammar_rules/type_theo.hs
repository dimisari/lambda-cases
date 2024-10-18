instance A'Has_A_Wrapper Possibly' where
  wrap' = (\pA0 -> P.Just(pA0))

instance A'Has_A_Wrapper ListOf's where
  wrap' = (\pA0 -> [pA0])

instance A'Has_Internal_App Possibly' where
  apply'inside' =
    \(f', pA0) ->
    case pA0 of
      no_value -> no_value
      P.Just x -> P.Just(f'(x))

instance A'Has_Internal_App ListOf's where
  apply'inside' =
    \(f', pA0) ->
    case pA0 of
      [] -> []
      head : tail -> f'(head) !+ apply'inside'((\pA0 -> f'(pA0)), tail)

instance A'And'Can_Be_Equal b0 b1 => A'And'Can_Be_Unequal b0 b1 where
  a !!= b = not'(a !== b)

instance A'Can_Be_Greater_Than' b0 b1 => A'Can_Be_Le_Or_Eq_To' b0 b1 where
  a !<= b = not'(a !> b)

instance A'And'Have_Eq_And_Gr b0 b1 => A'Can_Be_Gr_Or_Eq_To' b0 b1 where
  a !>= b = a !== b !| a !> b

instance A'Has_A_Wrapper Possibly' where
  wrap' = (\pA0 -> P.Just(pA0))

instance A'Has_A_Wrapper ListOf's where
  wrap' =
    (\pA0 -> [pA0])

instance A'Has_A_Wrapper ListOf's where
  wrap' =
    let
    a :: P.Integer
    a =
      b
    in
    a

instance A'And'Add_To' (ListOf's a1) a1 (ListOf's a1) where
  l !+ a =
    l &> \pA0 ->
    case pA0 of
      empty_l -> Cnon_empty_l(ft2(a, empty_l))
      Cnon_empty_l (head, tail) -> Cnon_empty_l(ft2(head, tail !+ a))

instance A'And'Add_To' (ListOf's a1) a1 (ListOf's a1) where
  l !+ a =
    l

