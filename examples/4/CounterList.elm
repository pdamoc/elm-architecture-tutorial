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
      case counterMsg of
        Counter.Remove -> update (Remove id) model
        _ -> 
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
  Html.App.map (Modify id) (Counter.viewWithRemoveButton model)
