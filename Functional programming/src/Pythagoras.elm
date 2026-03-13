
module Pythagoras exposing(..)

sqr: Int -> Int
sqr n = n * n

isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
    sqr a + sqr b == sqr c

leg1: Int -> Int -> Int
leg1 x y = 
    if checkValid x y then
    x*x - y*y
    else
    0

leg2: Int -> Int -> Int
leg2 x y =
    if checkValid x y then
    2*x*y
    else
    0

hyp: Int -> Int -> Int
hyp x y = 
    if checkValid x y then
    x*x + y*y
    else
    0

checkValid: Int -> Int -> Bool
checkValid x y =
    x > y && x > 0 && y > 0

pythTriple: (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    (leg1 x y, leg2 x y, hyp x y)

isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (x, y, z) = 
    isTriple x y z


pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list =
    List.map pythTriple list

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of
        first :: rest ->
            [pythTriple(first)] ++ pythTriplesRec(rest)

        [] ->
            []


arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list =
    List.filter isTripleTuple list

arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec list =
    case list of
        first :: rest ->
            if isTripleTuple first then
                [first] ++ arePythTriplesRec(rest)
            else
                arePythTriplesRec(rest)

        [] ->
            []

