{-# language LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Generation.PrefixesAndHardcoded where

-- prefixes

lower_prefix :: String
lower_prefix = "a"

upper_prefix :: String
upper_prefix = "A"

spid_projection_prefix :: String
spid_projection_prefix = "p"

change_prefix :: String
change_prefix = "c0"

spid_change_prefix :: String
spid_change_prefix = "c"

constructor_prefix :: String
constructor_prefix = "C"

param_t_var_prefix :: String
param_t_var_prefix = "a"

ad_hoc_t_var_prefix :: String
ad_hoc_t_var_prefix = "b"

param_prefix :: String
param_prefix = "pA"

-- hardcoded

show_class :: String
show_class = "P.Show"

show_val1 :: String
show_val1 = "show"

show_val2 :: String
show_val2 = "P.show"

bool :: String
bool = "P.Bool"

integer :: String
integer = "P.Integer"

double :: String
double = "P.Double"

char :: String
char = "P.Char"

string :: String
string = "P.String"

just :: String
just = "P.Just"

left :: String
left = "P.Left"

right :: String
right = "P.Right"

true :: String
true = "P.True"

false :: String
false = "P.False"

pnothing :: String
pnothing = "P.Nothing"

pprint :: String
pprint = "P.print"

pundefined :: String
pundefined = "P.undefined"

ppi :: String
ppi = "P.pi"

under_pfarg_param :: String
under_pfarg_param = "x'"

