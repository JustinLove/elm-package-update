module View exposing (view, document)

import Package exposing (Package)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

css = """
.found { color: green; }
.missing { color: red; }
"""

document model = {title = "Elm Package Update", body = [view model]}
view model =
  div []
    [ node "style" [] [ text css ]
    , ul []
        (model.packages
          |> List.map (displayPackage model.repository)
        )
    ]

displayPackage : (List String) -> Package -> Html msg
displayPackage repository package =
  li []
    [ text (Maybe.withDefault "--" package.name)
    , ul []
      (List.map (displayDependency repository) package.dependencies)
    ]

displayDependency : (List String) -> String -> Html msg
displayDependency repository name =
  li [ if List.member name repository then
         class "found"
       else
         class "missing" 
     ]
    [ text name ]
