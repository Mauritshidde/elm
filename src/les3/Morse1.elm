module Morse1 exposing(..)
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

toMorseHelper:List Char -> String -> String
toMorseHelper list string= 
    let 
        firstElement = List.head list
        newList = drop 1 list
        notMaybe = Maybe.withDefault ' ' firstElement
        newString = String.append string (Maybe.withDefault "" (Dict.get notMaybe morseDict))
        addSpaces = String.append newString " "
    in
        if (List.length list) == 0 then
            newString
        else 
            toMorseHelper newList addSpaces

        -- Maybe.withDefault ' ' firstElement

toMorse:String -> String
toMorse word = 
    let 
        list = toString word
    in
        toMorseHelper list ""