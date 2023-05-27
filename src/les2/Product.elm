module Product exposing(..)

productHelper:Int -> Int -> Int -> Int
productHelper n r v = 
    case n of
    0 -> 
        v
    _ -> 
        let
            more = v + r
            new_n = n-1
        in
            productHelper new_n r more

product:Int -> Int -> Int
product n r = 
    productHelper n r 0
