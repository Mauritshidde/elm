module Main exposing(..)
import Basics
import List exposing (tail, drop)

getElementFromList:List Char -> Int -> Int -> Char
getElementFromList list target current = 
    if target == current then
        Maybe.withDefault ' ' (List.head list)
    else
        let
            newList = List.drop 1 list
            newCurrent = current + 1
        in
            getElementFromList newList target newCurrent

toString:String -> List Char
toString string = 
    let 
        val = String.toUpper string
    in
        String.toList val

palindroomHelper:List Char -> Int -> Int -> Bool
palindroomHelper list index1 index2 = 
    if index1 >= (List.length list)-1 then
        if (getElementFromList list index1 0) == (getElementFromList list index2 0) then
            True
        else 
            False
    else
        if (getElementFromList list index1 0) == (getElementFromList list index2 0) then
            let
                newIndex1 = index1 + 1
                newIndex2 = index2 - 1
            in
                palindroomHelper list newIndex1 newIndex2
        else
            False


palindroom:String -> Bool
palindroom word = 
    let 
        list = String.toList word
        endIndex = (List.length list) - 1
    in
        palindroomHelper list 0 endIndex