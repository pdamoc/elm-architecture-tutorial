module SpinSquare exposing (Model, Msg(Tick), init, update, view) 

import Easing exposing (ease, easeOutBounce, float)
import Svg exposing (Svg, svg, rect, g, text, text')
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL

type alias Model =
    { angle : Float
    , prevClockTime : Time
    , elapsedTime : Maybe Time
    }


init : Model
init =
  { angle = 0
  , prevClockTime = 0
  , elapsedTime = Nothing }
  

rotateStep = 90
duration = second


-- UPDATE

type Msg
    = Spin
    | Tick Time


update : Msg -> Model -> Model
update msg model =
  case msg of
    Spin ->
      case model.elapsedTime of
        Nothing ->
          { model | elapsedTime = Just -1 } 
        _ -> 
          model 

    Tick clockTime ->
      case model.elapsedTime of 
        Nothing -> 
          model 
        Just -1 -> 
          {model | prevClockTime = clockTime, elapsedTime = Just 0}
        _ -> 
          let 
            newElapsedTime = clockTime-model.prevClockTime
          in 
            if newElapsedTime > duration then
              { model |
                angle = model.angle + rotateStep
              , elapsedTime = Nothing
              } 
            else
              { model |
                angle = model.angle
              , elapsedTime = Just newElapsedTime
              } 


-- VIEW

toOffset : Maybe Float -> Float
toOffset elapsedTime =
  case elapsedTime of
    Nothing ->
      0
    Just -1 -> 
      0
    Just t ->
      ease easeOutBounce float 0 rotateStep duration t


view : Model -> Svg Msg
view model =
  let
    angle =
      model.angle + toOffset model.elapsedTime
  in
    svg
      [ width "200", height "200", viewBox "0 0 200 200" ]
      [ g [ transform ("translate(100, 100) rotate(" ++ toString angle ++ ")")
          , onClick Spin
          ]
          [ rect
              [ x "-50"
              , y "-50"
              , width "100"
              , height "100"
              , rx "15"
              , ry "15"
              , style "fill: #60B5CC;"
              ]
              []
          , text' [ fill "white", textAnchor "middle" ] [ text "Click me!" ]
          ]
      ]
