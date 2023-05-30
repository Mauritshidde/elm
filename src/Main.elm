-- module Main exposing(..)

-- import Html exposing (text, div, input, Attribute)
-- import Browser exposing (sandbox)
-- import Html.Events exposing (on, keyCode, onInput)
-- import Html.Attributes exposing(..)
-- import Json.Decode as Json
-- import String exposing(split)
-- import List exposing(..)
-- import Maybe exposing(..)
-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)

-- type alias Point =
--     { x : Float
--     , y : Float
--     }


-- koch : Point -> Point -> Int -> List Point -> List Point
-- koch a b limit points =
--     let
--         ( dx, dy ) =
--             ( b.x - a.x, b.y - a.y )

--         dist =
--             dx * dx + dy * dy |> sqrt

--         unit =
--             dist / 3

--         angle =
--             atan2 dy dx

--         p1 =
--             Point (a.x + dx / 3) (a.y + dy / 3)

--         p2 =
--             Point
--                 (p1.x
--                     + (cos (angle - pi / 3))
--                     * unit
--                 )
--                 (p1.y
--                     + (sin (angle - pi / 3))
--                     * unit
--                 )

--         p3 =
--             Point (b.x - dx / 3) (b.y - dy / 3)

--         p4 = 2
--     in
--         if limit > 1 then
--             let
--                 l =
--                     limit - 1
--             in
--                 List.concat
--                     [ points
--                     , koch a p1 l points
--                     , koch p1 p2 l points
--                     , koch p2 p3 l points
--                     , koch p3 p4 l points
--                     , koch p4 b l points
--                     ]
--         else
--             a :: p1 :: p2 :: p3 :: p4 :: b :: points


-- startP1 : Point
-- startP1 =
--     Point 0 -150

-- startP2 : Point
-- startP2 =
--     Point 150 100


-- startP3 : Point
-- startP3 =
--     Point -150 100

-- startP4 : Point
-- startP4 =
--     Point -200 150

-- pointsListToString: List Point -> String
-- pointsListToString l =

--    if List.isEmpty l then
--        ""
--    else
--       let
--         h = withDefault (Point 0 0) (head (take 1 l))
--       in
--         (String.fromFloat h.x) ++ "," ++ (String.fromFloat h.y) ++ " " ++ (pointsListToString (drop 1 l))

-- main = Browser.sandbox { init = init, update = update, view = view }

-- type alias Model = { content : String }

-- init : Model
-- init = { content = "1" }

-- view model =
--    let
--         path =
--             pointsListToString (koch startP1 startP2 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP2 startP3 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP3 startP1 (Maybe.withDefault 1 (String.toInt model.content)) [])
--    in
--         div []
--           [ input [ placeholder "numbers separated by ,", value model.content, onInput Change ] []
--           , svg [ viewBox "0 0 200 200", Svg.Attributes.width "400px" ]

--            [ g [ transform "translate(100, 100) scale(0.5,-0.5)" ]

--                  [
--                     polyline [ fill "none", stroke "black", points path] []

--                 ]

--            ]
--           ]

-- type Msg
--   = Change String

-- update : Msg -> Model -> Model
-- update msg model =
--   case msg of
--     Change newContent ->
--         { model | content = newContent }

module Main exposing(..)
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

decodeMorseHelper:List String -> String
decodeMorseHelper list = 
    let 
        firstElement = List.head list
        newList = drop 1 list
        notMaybe = Maybe.withDefault "" firstElement
        
    in
        if (List.length list) == 0 then
            ""
        else 
            let 
                newString = (decodeMorseHelper newList) ++ String.fromChar (Maybe.withDefault ' ' (Dict.get notMaybe morseDict))
            in
                newString

decodeMorse:String -> String
decodeMorse word = 
    let 
        -- list = toString word
        list = String.split " " word
    in
        decodeMorseHelper list