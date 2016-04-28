module Counter exposing (..)

import Html exposing (..)
import Html.App as H
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int

init : Int -> Model 
init v = v

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
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

viewWithRemoveButton : (Msg -> pMsg) -> pMsg -> Model -> Html pMsg
viewWithRemoveButton toParent removeMsg model =
  div []
    [ H.map toParent <| button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , H.map toParent <| button [ onClick Increment ] [ text "+" ]
    , div [ countStyle ] []
    , button [ onClick removeMsg ] [ text "X" ]
    ]


