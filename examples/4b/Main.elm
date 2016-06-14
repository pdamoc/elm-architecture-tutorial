import Html.App as Html

import CounterList exposing (init, update, view)

main : Program Never
main =
  Html.beginnerProgram
    { model = init, update = update, view = view}
