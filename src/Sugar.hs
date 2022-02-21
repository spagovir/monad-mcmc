module Sugar where 
-- syntactic sugar. mostly OCaml/F# style function pipe arrows.

infixl 0 |> 
(|>) = flip ($)

infixl 9 |>> 
(|>>) = flip (.)

curry3 f a b c = f (a,b,c)
trd3 (_,_,c) = c
