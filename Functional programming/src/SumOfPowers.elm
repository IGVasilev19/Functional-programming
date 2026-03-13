module SumOfPowers exposing (..)

sumpow: Int -> Int
sumpow n =
    if n > 0 then
        2^(n-1) + sumpow(n-1)
    else
        0

-- make myMap function
-- make myFilter function
-- Assume/Ensure(code contracts) for functions