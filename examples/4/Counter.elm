module Counter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int

init : Int -> Model 
init v = v

-- UPDATE

type LocalMsg = Increment | Decrement

type Msg = Local LocalMsg | Remove

update : Msg -> Model -> Model
update msg model =
  case msg of
    Local Increment ->
      model + 1

    Local Decrement ->
      model - 1

    Remove -> 
      model

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (Local Decrement) ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick (Local Increment) ] [ text "+" ]
    ]

viewWithRemoveButton : Model -> Html Msg
viewWithRemoveButton model =
  div []
    [ button [ onClick (Local Decrement) ] [ text "-" ]
    , span [ countStyle ] [ text (toString model) ]
    , button [ onClick (Local Increment) ] [ text "+" ]
    , button [ onClick Remove ] [ text "X" ]
    ]


countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]



