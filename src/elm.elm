module Main exposing(..)
-- import Basics
import List exposing (tail, drop)
import Dict exposing (Dict)

powHelper:Float -> Int -> Float -> Float
powHelper x p v = 
    case p of
    1 ->
        v
    _ ->
        let
            newValue = v * x
            newP = p-1
        in
            powHelper x newP newValue

pow:Float -> Int -> Float
pow x p = 
    powHelper x p x