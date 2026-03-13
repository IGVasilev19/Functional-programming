
module Caesar exposing (..)
import List exposing (indexedMap)

charToNum: Char -> Int
charToNum char =
    Char.toCode char

getShiftedChar: Char -> Int -> Char -> Char
getShiftedChar char shift base =
    Char.fromCode ((modBy 26 (charToNum char - charToNum base + shift)) + charToNum base)


encode: Int -> Char -> Char
encode shift char =
    if Char.isLower char then 
    getShiftedChar char shift 'a'
    else 
    getShiftedChar char shift 'A'
        
decode: Int -> Char -> Char
decode shift char =
    if Char.isLower char then 
    getShiftedChar char (-1 * shift) 'a' 
    else 
    getShiftedChar char (-1 * shift) 'A'


normalize: String -> String
normalize str =
    case String.uncons str of
        Just (head, tail) ->
            if Char.isAlpha head then
                String.cons head (normalize tail)
            else
                normalize tail
            
        Nothing ->
            ""



encrypt: Int -> String -> String
encrypt step str =
    case String.uncons str of
        Just (head, tail) ->
            if Char.isAlpha head then
                String.cons (encode step head) (encrypt step tail)
            else
            String.cons head (encrypt step tail)
            
        Nothing ->
            ""


decrypt: Int -> String -> String
decrypt step str =
    case String.uncons str of
        Just (head, tail) ->
            if Char.isAlpha head then
                String.cons (decode step head) (decrypt step tail)
            else
            String.cons head (decrypt step tail)
            
        Nothing ->
            ""


checkStringContainsCandidate: String -> String -> Bool
checkStringContainsCandidate candidate decryptStr =
    case String.uncons candidate of
        Just (headCan, tailCan) ->
            case String.uncons decryptStr of
                Just (head, tail) ->
                    if headCan == head then
                        tailCan == String.left (String.length tailCan) tail
                    else
                        checkStringContainsCandidate candidate tail
                Nothing ->
                    False
        Nothing ->
            False

checkAllCandidatesForAString: List String -> String -> Bool
checkAllCandidatesForAString candidates decryptStr =
    case List.head candidates of
        Just head ->
            if checkStringContainsCandidate head decryptStr then
                True
            else
                case List.tail candidates of
                    Just tail ->
                        checkAllCandidatesForAString tail decryptStr
                    Nothing ->
                        False
        Nothing ->
            case List.tail candidates of
                    Just tail ->
                        checkAllCandidatesForAString tail decryptStr
                    Nothing ->
                        False
                

-- candidates: List String -> String -> List (Int, String)
-- candidates listCandidates strToDecrypt =
--     List.range 1 25
--         |> List.filterMap (\shift-> 
--         if checkStringContainsCandidate 

--         )

        




-- go through all keys
-- decrypt the initial string
-- check if a candidate is in it 
-- save the combination of int and string if it is
-- continue until no more keys