import SpinSquarePair exposing (init, update, view, Msg(Tick))
import AnimationFrame
import Html.App as App


main =
  App.program
    { init = init ! []
    , update = \msg model -> update msg model ! []
    , view = view
    , subscriptions = (\_ -> AnimationFrame.times Tick)
    }


