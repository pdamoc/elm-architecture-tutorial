module CounterPair exposing (..)

import Counter
import Html exposing (..)
import Html.App
import Html.Events exposing (..)


-- MODEL

type alias Model =
    { topCounter : Counter.Model
    , bottomCounter : Counter.Model
    }


init : Int -> Int -> Model
init top bottom =
    { topCounter = Counter.init top
    , bottomCounter = Counter.init bottom
    }


-- UPDATE

type Msg
    = Reset
    | Top Counter.Msg
    | Bottom Counter.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> init 0 0

    Top act ->
      { model |
          topCounter = Counter.update act model.topCounter
      }

    Bottom act ->
      { model |
          bottomCounter = Counter.update act model.bottomCounter
      }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ Html.App.map Top <| Counter.view model.topCounter
    , Html.App.map Bottom <| Counter.view model.bottomCounter
    , button [ onClick Reset ] [ text "RESET" ]
    ]
