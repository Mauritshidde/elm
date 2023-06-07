module Main exposing(..)

product:Int -> Int -> Int
product input factor = 
    case input of
    0 -> 
        0
    _ -> 
        let  
            newInput = input - 1
            sum = factor + (product newInput factor)
        in 
            sum