module Main exposing (..)

import Html exposing (text, div, input, Attribute)
import Browser exposing (sandbox)
import Html.Events exposing (on, keyCode, onInput)
import Html.Attributes exposing(..)
import Json.Decode as Json
import String exposing(split)
import List exposing(..)
import Maybe exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Point =
    { x : Float
    , y : Float
    }


koch : Point -> Point -> Int -> List Point -> List Point
koch a b limit points =
    let
        ( dx, dy ) =
            ( b.x - a.x, b.y - a.y )

        dist =
            dx * dx + dy * dy |> sqrt

        unit =
            dist / 3

        angle =
            atan2 dy dx

        p1 =
            Point (a.x + dx / 3) (a.y + dy / 3)

        p2 =
            Point
                (p1.x
                    + (cos (angle - pi / 3))
                    * unit
                )
                (p1.y
                    + (sin (angle - pi / 3))
                    * unit
                )

        p3 =
            Point (b.x - dx / 3) (b.y - dy / 3)
        p4 =
            Point (b.x - dx / 3) (b.y - dy / 3)
        p5 =
            Point (b.x - dx / 3) (b.y - dy / 3)
        p6 =
            Point (b.x - dx / 3) (b.y - dy / 3)
    in
        if limit > 1 then
            let
                l =
                    limit - 1
            in
                List.concat
                    [ points
                    , koch a p1 l points
                    , koch p1 p2 l points
                    , koch p2 p3 l points
                    , koch p3 p4 l points
                    , koch p4 p5 l points
                    , koch p5 p6 l points
                    , koch p6 b l points
                    ]
        else
            a :: p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: b :: points


startP1 : Point
startP1 =
    Point -200 -150

startP2 : Point
startP2 =
    Point -100 150

startP3 : Point
startP3 =
    Point 100 150
    
startP4 : Point
startP4 =
    Point 200 -150

startP5 : Point
startP5 =
    Point 100 0

startP6 : Point
startP6 =
    Point -100 0

pointsListToString: List Point -> String
pointsListToString l =

   if List.isEmpty l then
       ""
   else
      let
        h = withDefault (Point 0 0) (head (take 1 l))
      in
        (String.fromFloat h.x) ++ "," ++ (String.fromFloat h.y) ++ " " ++ (pointsListToString (drop 1 l))

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model = { content : String }

init : Model
init = { content = "1" }

view model =
   let
        path =
            pointsListToString (koch startP1 startP2 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP2 startP3 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP3 startP4 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP4 startP5 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP5 startP6 (Maybe.withDefault 1 (String.toInt model.content)) []) ++ pointsListToString (koch startP6 startP1 (Maybe.withDefault 1 (String.toInt model.content)) [])
   in
        div []
          [ input [ placeholder "numbers separated by ,", value model.content, onInput Change ] []
          , svg [ viewBox "0 0 200 200", Svg.Attributes.width "400px" ]

           [ g [ transform "translate(100, 100) scale(0.5,-0.5)" ]

                 [ 
                    polyline [ fill "black", stroke "red", points path] []

                ]

           ]
          ]
          
type Msg 
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
        { model | content = newContent }
-- compile-code