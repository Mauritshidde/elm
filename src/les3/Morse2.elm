module Morse2 exposing(..)
import Basics
import List exposing (tail, drop)
import Dict exposing (Dict)

morseDict : Dict String Char
morseDict =
    Dict.fromList
        [ (".-", 'A')
        , ("-...", 'B')
        , ("-.-.", 'C')
        , ("-..", 'D')
        , (".", 'E')
        , ("..-.", 'F')
        , ("--.", 'G')
        , ("....", 'H')
        , ("..", 'I')
        , (".---", 'J')
        , ("-.-", 'K')
        , (".-..", 'L')
        , ("--", 'M')
        , ("-.", 'N')
        , ("---", 'O')
        , (".--.", 'P')
        , ("--.-", 'Q')
        , (".-.", 'R')
        , ("...", 'S')
        , ("-", 'T')
        , ("..-", 'U')
        , ("...-", 'V')
        , (".--", 'W')
        , ("-..-", 'X')
        , ("-.--", 'Y')
        , ("--..", 'Z')
        , ("-----", '0')
        , (".----", '1')
        , ("..---", '2')
        , ("...--", '3')
        , ("....-", '4')
        , (".....", '5')
        , ("-....", '6')
        , ("--...", '7')
        , ("---..", '8')
        , ("----.", '9')
        ]

toString:String -> List Char
toString string = 
    let 
        val = String.toUpper string
    in
        String.toList val

decodeMorseHelper:List String -> String -> String
decodeMorseHelper list string= 
    let 
        firstElement = List.head list
        newList = drop 1 list
        notMaybe = Maybe.withDefault "" firstElement
        newString = string ++ String.fromChar (Maybe.withDefault ' ' (Dict.get notMaybe morseDict))
    in
        if (List.length list) == 0 then
            newString
        else 
            decodeMorseHelper newList newString

        -- Maybe.withDefault ' ' firstElement

decodeMorse:String -> String
decodeMorse word = 
    let 
        -- list = toString word
        list = String.split " " word
    in
        decodeMorseHelper list ""


            
