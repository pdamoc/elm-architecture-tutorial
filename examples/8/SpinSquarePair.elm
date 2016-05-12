module SpinSquarePair exposing (..)

import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (..)
import SpinSquare 
import Time exposing (Time)


-- MODEL

type alias Model =
    { left : SpinSquare.Model
    , right : SpinSquare.Model
    }


init : Model
init =
  Model SpinSquare.init SpinSquare.init

-- UPDATE

type Msg
    = Left SpinSquare.Msg
    | Right SpinSquare.Msg
    | Tick Time


update : Msg -> Model -> Model
update action model =
  case action of
    Left act ->
      Model (SpinSquare.update act model.left) model.right
      
    Right act ->
      Model model.left (SpinSquare.update act model.right)
    
    Tick time -> 
      let 
        --t = Debug.log "time:" time
        left = SpinSquare.update (SpinSquare.Tick time) model.left
        right = SpinSquare.update (SpinSquare.Tick time) model.right
      in 
        (Model left right) 

-- VIEW

(=>) = (,)


view : Model -> Html Msg
view model =
  div [ style [ "display" => "flex" ] ]
    [ map Left <| SpinSquare.view model.left
    , map Right <| SpinSquare.view model.right
    ]