import Html.App as Html

import CounterPair exposing (init, update, view)

main : Program Never
main =
  Html.beginnerProgram
    { model = init 0 0, update = update, view = view}
