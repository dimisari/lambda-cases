\pA0 ->
case pA0 of
  true -> print'("It's true!! :)")
  false -> print'("It's false... :(")

\pA0 ->
case pA0 of
  green -> print'("It's green! Let's go!!! :)")
  amber -> print'("Go go go, fast!")
  red -> print'("Stop right now! You're going to kill us!!")

\pA0 ->
case pA0 of
  green -> true
  amber -> true
  red -> false

\pA0 ->
case pA0 of
  17 -> true
  42 -> true
  _ -> false

\(pA0, pA1) ->
case (pA0, pA1) of
  (green, green) -> true
  (amber, amber) -> true
  (red, red) -> true
  _ -> false

\(x, pA0) ->
case pA0 of
  0 -> x
  y -> gcd''(y, x &> mod <& y)

\pA0 ->
case pA0 of
  [_] -> true
  _ -> false

\pA0 ->
case pA0 of
  empty_l -> true
  Cnon_empty_l (Cnon_empty_l _) -> false

\(f, pA0) ->
case pA0 of
  empty_l -> empty_l
  Cnon_empty_l list -> Cnon_empty_l(ft2(f'(head(list)), apply'to_all_in'(f, tail(list))))

\pA0 ->
case pA0 of
  x1 : x2 : xs ->
    (x1 !< x2) !& a'is_sorted(x2 !+ xs)
  _ -> true

