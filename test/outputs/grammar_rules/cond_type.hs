A'And'Can_Be_Equal b0 b1 => (b0, b1) -> P.Bool

A'And'Add_To' b0 b1 b2 => (b0, b1) -> b2

A'Is's_First b0 b1 => b1 -> b0

A'Has_Str_Rep b19 => b19 -> P.String

A'Has_Use b4 => (b4 a1, a1 -> b4 a2) -> b4 a2

A'Has_Internal_App b19 => (a1 -> a2, b19 a1) -> b19 a2

