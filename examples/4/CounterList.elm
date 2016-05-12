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
      let 
        updateCounter (counterID, counterModel) (acc, prevMsg) =
          if counterID == id then
              let 
                (newCounterModel, dispatch) = Counter.update counterMsg counterModel
              in 
                (acc ++ [(counterID, newCounterModel)], dispatch)
          else
              (acc ++ [(counterID, counterModel)], prevMsg)

        (newCounters, todo) = List.foldl updateCounter ([], Nothing) model.counters
      in
        case todo of 
          Nothing ->  
            { model | counters = newCounters }
          Just Counter.Remove ->
            update (Remove id) { model | counters = newCounters }


-- VIEW

view : Model -> Html Msg
view model =
  let insert = button [ onClick Insert ] [ text "Add" ]
  in
      div [] (insert :: List.map viewCounter model.counters)


viewCounter : (ID, Counter.Model) -> Html Msg
viewCounter (id, model) =
  Html.App.map (Modify id) (Counter.viewWithRemoveButton model)
