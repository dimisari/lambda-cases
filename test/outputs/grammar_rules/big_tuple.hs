ft6
( "Hey, I'm the first field and I'm also a relatively big string."
, (42 :: P.Integer), (3.14 :: P.Double), "Hey, I'm the first small string", "Hey, I'm the second small string"
, ft3("Hey, I'm a string inside the nested tuple", (2.71 :: P.Double), (1.61 :: P.Double))
)

ft6
( "Hey, I'm the first field and I'm also a relatively big string.", (42 :: P.Integer)
, (3.14 :: P.Double), "Hey, I'm the first small string", "Hey, I'm the second small string"
, ft3("Hey, I'm a string inside the nested tuple", (2.71 :: P.Double), (1.61 :: P.Double))
)

\pA0 -> 
ft3
( "Hey, I'm the first field and I'm also a pretty big string."
, "Hey, I'm the second field and I'm a smaller string"
, pA0
)

