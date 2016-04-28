import Html.App as Html

import RandomGifList exposing (init, update, view)

main : Program Never
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
