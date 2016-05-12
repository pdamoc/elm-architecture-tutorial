module CounterList exposing (..)

import Counter
import Html exposing (..)
import Html.Events exposing (..)
import Html.App


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
    | Modify ID Counter.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    Insert ->
      { model |
          counters = ( model.nextID, Counter.init 0 ) :: model.counters,
          nextID = model.nextID + 1
      }

    Modify id counterMsg ->
      let 
        updateCounter (counterID, counterModel) =
          case (counterID == id, Counter.update counterMsg counterModel) of
            (False, _) -> 
              Just (counterID, counterModel) -- not the current counter
            (True, (newCounterModel, Just Counter.Remove)) -> 
              Nothing -- current counter +  Remove; should no longer be in the list
            (True, (newCounterModel, _)) -> 
              Just (counterID, newCounterModel) -- current counter; other actions
      in
        { model | counters = List.filterMap updateCounter model.counters }


-- VIEW

view : Model -> Html Msg
view model =
  let insert = button [ onClick Insert ] [ text "Add" ]
  in
      div [] (insert :: List.map viewCounter model.counters)


viewCounter : (ID, Counter.Model) -> Html Msg
viewCounter (id, model) =
  Html.App.map (Modify id) (Counter.viewWithRemoveButton model)
