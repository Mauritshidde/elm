module Main exposing(..)

product:Int -> Int -> Int -> Int
product n r v = 
    case n of
    0 -> 
        v
    _ -> 
        let
            more = v + r
            new_n = n-1
        in
            product new_n r more
