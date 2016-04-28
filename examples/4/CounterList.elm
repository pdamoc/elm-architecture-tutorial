module CounterList exposing (..)

import Counter
import Html exposing (..)
import Html.Events exposing (..)


-- MODEL

type alias Model =
    { counters : List ( ID, Counter.Model )
    , nextID : ID
    }

type alias ID = Int


init : Model
init =
    { counters = []
    , nextID = 0
    }


-- UPDATE

type Msg
    = Insert
    | Remove ID
    | Modify ID Counter.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    Insert ->
      { model |
          counters = ( model.nextID, Counter.init 0 ) :: model.counters,
          nextID = model.nextID + 1
      }

    Remove id ->
      { model |
          counters = List.filter (\(counterID, _) -> counterID /= id) model.counters
      }

    Modify id counterMsg ->
      let updateCounter (counterID, counterModel) =
              if counterID == id then
                  (counterID, Counter.update counterMsg counterModel)
              else
                (counterID, counterModel)
      in
          { model | counters = List.map updateCounter model.counters }


-- VIEW

view : Model -> Html Msg
view model =
  let insert = button [ onClick Insert ] [ text "Add" ]
  in
      div [] (insert :: List.map viewCounter model.counters)


viewCounter : (ID, Counter.Model) -> Html Msg
viewCounter (id, model) =
  Counter.viewWithRemoveButton (Modify id) (Remove id) model
