module abc exposing(..)
import Basics

abc:Float -> Float -> Float -> (Float, Float)
abc a b c = 
    let
        d = b * b - 4 * a * c
    in
        if d == 0 then
            let
                val = (-b) / (2 * a)
            in
                (val, 0/0)
        else if d > 0 then
            let
                root = sqrt d
                rootb = root - b
                rootb2 = -root - b
                vala = 2 * a
                val = rootb/vala
                val2 = rootb2/vala
            in
                (val, val2)
        else
            (0/0, 0/0)

