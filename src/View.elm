module View exposing (view, document)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

document model = {title = "Elm Package Update", body = [view model]}

view model =
  div [] [ text "hi" ]
