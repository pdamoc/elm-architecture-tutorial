module Counter exposing (Model, Msg, Dispatch(Remove), init, update, view, viewWithRemoveButton)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int

init : Int -> Model 
init v = v

-- UPDATE

type Dispatch = Remove

type Msg = Increment | Decrement | RemoveSelf

update : Msg -> Model -> (Model, Maybe Dispatch)
update msg model =
  case msg of
    Increment ->
      (model + 1, Nothing) 

    Decrement ->
      (model - 1, Nothing) 

    RemoveSelf -> 
      (model, Just Remove)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

viewWithRemoveButton : Model -> Html Msg
viewWithRemoveButton model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , span [ countStyle ] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick RemoveSelf ] [ text "X" ]
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



