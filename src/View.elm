module View exposing (Msg(..), view, document)

import Package exposing (Package)
import FileInput

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)

type Msg
  = LoadPackage FileInput.Files
  | RemovePackage Int

css = """
.found { color: green; }
.missing { color: red; }
.renamed .missing { opacity: 0.5; }
"""

document tagger model =
  { title = "Elm Package Update"
  , body = [view model |> Html.map tagger]
  }

view model =
  div []
    [ node "style" [] [ text css ]
    , input
      [ type_ "file"
      , on "change" (FileInput.targetFiles LoadPackage)
      ]
      []
    , p []
      [ text (model.repository |> List.length |> String.fromInt)
      , text " known packages"
      ]
    , ul []
        (model.packages
          |> List.indexedMap (displayPackage model.repository)
        )
    ]

displayPackage : (List String) -> Int -> Package -> Html Msg
displayPackage repository index package =
  li []
    [ text (Maybe.withDefault "--" package.name)
    , text " "
    , button [ onClick (RemovePackage index) ] [ text "X" ]
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
  a
    [ if found then
        class "found"
      else
        class "missing"
    , href ("https://package.elm-lang.org/packages/" ++ name ++ "/latest")
    ]
    [ text name ]

translatePackageName : String -> String
translatePackageName name =
  case name of
    "evancz/url-parser" -> "elm/url"
    "elm-tools/parser" -> "elm/parser"
    "elm-lang/animation-frame" -> "elm/browser"
    "elm-lang/dom" -> "elm/browser"
    "elm-lang/keyboard" -> "elm/browser"
    "elm-lang/mouse" -> "elm/browser"
    "elm-lang/navigation" -> "elm/browser"
    "elm-lang/page-visibility" -> "elm/browser"
    "elm-lang/window" -> "elm/browser"
    _ -> String.replace "elm-lang/" "elm/" name
