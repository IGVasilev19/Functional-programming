module Sorting exposing (..)


merge: List comparable -> List comparable -> List comparable
merge left right =
    case (left, right) of
        ([],[])->
            []
        ([],r)->
            r
        (l,[])->
            l
        (xl :: l, xr :: r) ->
            if xl > xr then
                xr :: merge (xl :: l) r
            else
                xl :: merge l (xr :: r)
        

msort: List comparable -> List comparable
msort list =
    case list of
        [] -> 
            []

        [x] ->
            [x]

        _->
            let
                leftSide = List.take ((List.length list)//2) list
                rightSide = List.drop (List.length leftSide) list
            in
                merge (msort leftSide) (msort rightSide)