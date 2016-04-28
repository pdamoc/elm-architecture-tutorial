module RandomGifList exposing (..)

import Html.App as H
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

import RandomGif


-- MODEL

type alias Model =
    { topic : String
    , gifList : List (Int, RandomGif.Model)
    , uid : Int
    }


init : (Model, Cmd Msg)
init =
    ( Model "" [] 0
    , Cmd.none
    )


-- UPDATE

type Msg
    = Topic String
    | Create
    | SubMsg Int RandomGif.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        Topic topic ->
            ( { model | topic = topic }
            , Cmd.none
            )

        Create ->
            let
                (newRandomGif, fx) =
                    RandomGif.init model.topic

                newModel =
                    Model "" (model.gifList ++ [(model.uid, newRandomGif)]) (model.uid + 1)
            in
                ( newModel
                , Cmd.map (SubMsg model.uid) fx
                )

        SubMsg msgId msg ->
            let
                subUpdate ((id, randomGif) as entry) =
                    if id == msgId then
                        let
                            (newRandomGif, fx) = RandomGif.update msg randomGif
                        in
                            ( (id, newRandomGif)
                            , Cmd.map (SubMsg id) fx
                            )
                    else
                        (entry, Cmd.none)

                (newGifList, fxList) =
                    model.gifList
                        |> List.map subUpdate
                        |> List.unzip
            in
                ( { model | gifList = newGifList }
                , Cmd.batch fxList
                )


-- VIEW

(=>) = (,)


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "What kind of gifs do you want?"
            , value model.topic
            , onEnter model.topic
            , on "input" (Json.map Topic targetValue) 
            , inputStyle
            ]
            []
        , div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
            (List.map elementView model.gifList)
        ]


elementView : (Int, RandomGif.Model) -> Html Msg
elementView (id, model) =
    H.map (SubMsg id) <| RandomGif.view model


inputStyle : Attribute Msg
inputStyle =
    style
        [ ("width", "100%")
        , ("height", "40px")
        , ("padding", "10px 0")
        , ("font-size", "2em")
        , ("text-align", "center")
        ]


onEnter : String -> Attribute Msg
onEnter topic = 
  let 
    createOnEnter code = 
      if code == 13 then 
        Create
      else (Topic topic)
  in 
      on "keydown" (Json.map createOnEnter keyCode)
