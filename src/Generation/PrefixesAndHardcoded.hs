{-# language LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Generation.PrefixesAndHardcoded where

import Prelude qualified as P

-- prefixes

lower_prefix :: P.String
lower_prefix = "a"

upper_prefix :: P.String
upper_prefix = "A"

spid_projection_prefix :: P.String
spid_projection_prefix = "p"

change_prefix :: P.String
change_prefix = "c0"

spid_change_prefix :: P.String
spid_change_prefix = "c"

constructor_prefix :: P.String
constructor_prefix = "C"

param_t_var_prefix :: P.String
param_t_var_prefix = "a"

ad_hoc_t_var_prefix :: P.String
ad_hoc_t_var_prefix = "b"

param_prefix :: P.String
param_prefix = "pA"

-- hardcoded

show_class :: P.String
show_class = "P.Show"

show_val1 :: P.String
show_val1 = "show"

show_val2 :: P.String
show_val2 = "P.show"

bool :: P.String
bool = "P.Bool"

integer :: P.String
integer = "P.Integer"

double :: P.String
double = "P.Double"

char :: P.String
char = "P.Char"

string :: P.String
string = "P.String"

just :: P.String
just = "P.Just"

left :: P.String
left = "P.Left"

right :: P.String
right = "P.Right"

true :: P.String
true = "P.True"

false :: P.String
false = "P.False"

pnothing :: P.String
pnothing = "P.Nothing"

pprint :: P.String
pprint = "P.print"

pundefined :: P.String
pundefined = "P.undefined"

ppi :: P.String
ppi = "P.pi"

under_pfarg_param :: P.String
under_pfarg_param = "x'"

