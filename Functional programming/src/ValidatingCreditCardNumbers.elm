module ValidatingCreditCardNumbers exposing (..)

reverseDigitList: List Int -> List Int
reverseDigitList digitList =
    List.foldl (::) [] digitList

convertStringToListInt: String -> List Int
convertStringToListInt str =
    case String.uncons str of
        Just (head, tail) ->
           Maybe.withDefault 0 (String.toInt (String.fromChar head)) :: convertStringToListInt tail
        Nothing ->
            []

doubleEverySecondDigit: List Int -> Bool -> List Int
doubleEverySecondDigit rvDigitList isSecond =
    case rvDigitList of
        head :: tail ->
            if isSecond then
                if head*2 >= 10 then 
                    head*2-9 :: doubleEverySecondDigit tail False
                else
                    head*2 :: doubleEverySecondDigit tail False
            else
                head :: doubleEverySecondDigit tail True
        [] ->
            []

isValid: String -> Bool
isValid strCardNumber =
    let
        listDigits = reverseDigitList (convertStringToListInt strCardNumber)
        readyToBeSummed = doubleEverySecondDigit listDigits False 
        sumOfDigits = List.foldl(+) 0 readyToBeSummed
    in
        if Basics.modBy 10 sumOfDigits == 0 then
            True
        else
            False

checkAllCardNumbers: List String -> List Bool
checkAllCardNumbers cardNums =
    case cardNums of
        head :: tail ->
            isValid head :: checkAllCardNumbers tail
        [] ->
            []
