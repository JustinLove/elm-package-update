module View exposing (view, document)

import Package exposing (Package)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

css = """
.found { color: green; }
.missing { color: red; }
.renamed .missing { opacity: 0.5; }
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
  let
    newName = translatePackageName name
    found = List.member newName repository
  in
    (if name == newName then
      li [ ] [ packageName found name ]
    else
      li [ class "renamed" ]
        [ packageName found newName
        , text " ("
        , packageName False name
        , text ")"
        ]
    )

packageName : Bool -> String -> Html msg
packageName found name =
  span [ if found then
         class "found"
       else
         class "missing" 
     ]
    [ text name ]

translatePackageName : String -> String
translatePackageName name =
  case name of
    "elm-lang/dom" -> "elm/browser"
    "elm-tools/parser" -> "elm/parser"
    _ -> String.replace "elm-lang/" "elm/" name
