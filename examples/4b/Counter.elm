module Counter exposing (Model, Msg, init, update, view, viewWithContext, Context)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App


-- MODEL


type alias Model =
    Int


init : Int -> Model
init v =
    v



-- UPDATE


type Dispatch
    = Remove


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- inform the parent that it should remove the counter
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
        [ ( "font-size", "20px" )
        , ( "font-family", "monospace" )
        , ( "display", "inline-block" )
        , ( "width", "50px" )
        , ( "text-align", "center" )
        ]


type alias Context parentMsg =
    { toParent : Msg -> parentMsg
    , remove : parentMsg
    }


viewWithContext : Context parentMsg -> Model -> Html parentMsg
viewWithContext context model =
    div []
        [ App.map context.toParent (button [ onClick Decrement ] [ text "-" ])
        , div [ countStyle ] [ text (toString model) ]
        , App.map context.toParent (button [ onClick Increment ] [ text "+" ])
        , button [ onClick context.remove ] [ text "X" ]
        ]
