module Main exposing(..)
import Basics
import List exposing (tail, drop)

intToBinary:Int -> String
intToBinary getal = 
    if getal >= 1 then
        let
            rest = modBy 2 getal
        in
            if rest == 0 then
                let
                    newGetal = getal // 2
                    return = String.append (intToBinary newGetal) (String.fromInt rest)
                in
                    return
            else
                let
                    tussenstap = getal - 1
                    newGetal = tussenstap // 2
                    return = String.append (intToBinary newGetal) (String.fromInt rest)
                in
                    return
                    
    else
        ""


-- intToBinaryHelper:Int -> String -> String
-- intToBinaryHelper getal binaryString =
--     if getal >= 1 then
--         let
--             rest = modBy 2 getal
--             newString = String.append (String.fromInt rest) binaryString
--         in
--             if rest == 0 then
--                 let
--                     newGetal = getal // 2
--                 in
--                     intToBinaryHelper newGetal newString
--             else
--                 let
--                     tussenstap = getal - 1
--                     newGetal = tussenstap // 2
--                 in
--                     intToBinaryHelper newGetal newString
--     else
--         binaryString

-- intToBinary:Int -> String
-- intToBinary getal = 
--     intToBinaryHelper getal ""
    