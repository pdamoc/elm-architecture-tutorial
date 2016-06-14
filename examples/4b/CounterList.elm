module CounterList exposing (..)

import Counter exposing (Context)
import Html exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { counters : List ( ID, Counter.Model )
    , nextID : ID
    }


type alias ID =
    Int


init : Model
init =
    { counters = []
    , nextID = 0
    }



-- UPDATE


type Msg
    = Insert
    | Modify ID Counter.Msg
    | Remove ID


update : Msg -> Model -> Model
update msg model =
    case msg of
        Insert ->
            { model
                | counters = ( model.nextID, Counter.init 0 ) :: model.counters
                , nextID = model.nextID + 1
            }

        Modify id counterMsg ->
            let
                updateCounter ( counterID, counterModel ) =
                    if counterID == id then
                        ( counterID, Counter.update counterMsg counterModel )
                    else
                        ( counterID, counterModel )
            in
                { model | counters = List.map updateCounter model.counters }

        Remove id ->
            { model | counters = List.filter (fst >> ((/=) id)) model.counters }



-- VIEW


view : Model -> Html Msg
view model =
    let
        insert =
            button [ onClick Insert ] [ text "Add" ]
    in
        div [] (insert :: List.map viewCounter model.counters)


viewCounter : ( ID, Counter.Model ) -> Html Msg
viewCounter ( id, model ) =
    Counter.viewWithContext (Context (Modify id) (Remove id)) model
