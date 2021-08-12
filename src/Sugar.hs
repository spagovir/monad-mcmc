module Sugar where 
-- syntactic sugar. mostly OCaml/F# style function pipe arrows.

infixl 0 |> 
(|>) = flip ($)

infixl 9 |>> 
(|>>) = flip (.)
