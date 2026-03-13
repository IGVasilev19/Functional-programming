module Sequence exposing (..)


seq: Int -> Int -> List Int
seq start size =
    if size > 0 then
        [ start ] ++ seq (start + 1) (size - 1)
    else
        []
