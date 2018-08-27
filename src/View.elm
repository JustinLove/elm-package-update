module View exposing (view, document)

import Package exposing (Package)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

document model = {title = "Elm Package Update", body = [view model]}

view model =
  ul []
    (model.packages
      |> List.map displayPackage
    )

displayPackage : Package -> Html msg
displayPackage package =
  li []
    [ text package.packageText
    , ul []
      (List.map (text>>List.singleton>>(li [])) package.dependencies)
    ]

