import Html.App as Html

import RandomGif exposing (init, update, view)

main : Program Never
main =
  Html.program
    { init = init "cats", update = update, view = view, subscriptions = \_ -> Sub.none }
