import Html.App as Html

import RandomGifPair exposing (init, update, view)

main : Program Never
main =
  Html.program
    { init = init "funny cats" "funny dogs"
    , update = update, view = view, subscriptions = \_ -> Sub.none }
