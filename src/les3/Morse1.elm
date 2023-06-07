module Main exposing(..)
import Basics
import List exposing (tail, drop)
import Dict exposing (Dict)

morseDict : Dict Char String
morseDict =
    Dict.fromList
        [ ('A', ".-")
        , ('B', "-...")
        , ('C', "-.-.")
        , ('D', "-..")
        , ('E', ".")
        , ('F', "..-.")
        , ('G', "--.")
        , ('H', "....")
        , ('I', "..")
        , ('J', ".---")
        , ('K', "-.-")
        , ('L', ".-..")
        , ('M', "--")
        , ('N', "-.")
        , ('O', "---")
        , ('P', ".--.")
        , ('Q', "--.-")
        , ('R', ".-.")
        , ('S', "...")
        , ('T', "-")
        , ('U', "..-")
        , ('V', "...-")
        , ('W', ".--")
        , ('X', "-..-")
        , ('Y', "-.--")
        , ('Z', "--..")
        , ('0', "-----")
        , ('1', ".----")
        , ('2', "..---")
        , ('3', "...--")
        , ('4', "....-")
        , ('5', ".....")
        , ('6', "-....")
        , ('7', "--...")
        , ('8', "---..")
        , ('9', "----.")
        ]

toString:String -> List Char
toString string = 
    let 
        val = String.toUpper string
    in
        String.toList val

toMorseHelper:List Char -> String
toMorseHelper list = 
    let 
        firstElement = List.head list
        newList = drop 1 list
        notMaybe = Maybe.withDefault ' ' firstElement
    in
        if (List.length list) == 0 then
            ""
        else 
            let 
                morseString = String.append " " (String.append (Maybe.withDefault "" (Dict.get notMaybe morseDict)) (toMorseHelper newList))
            in
                morseString

toMorse:String -> String
toMorse word = 
    let 
        list = toString word
    in
        toMorseHelper list