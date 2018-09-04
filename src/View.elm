module View exposing (Msg(..), view, document)

import Package exposing (Package)
import FileInput

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode

type Msg
  = LoadPackage FileInput.Files
  | RemovePackage Int
  | SelectPackage Package
  | RenamePackage Package String

css = """
.found { color: green; }
.missing {
  color: white;
  background-color: red;
  padding-left: 0.2em;
  padding-right: 0.2em;
}
.old-name .missing { color: red; background-color: transparent; opacity: 0.5; }
.project-name.found {
  color: white;
  background-color: green;
  padding-left: 0.2em;
  padding-right: 0.2em;
}
input[readonly] { background-color: #eee; }
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
          |> List.indexedMap (displayPackageSummary model.repository model.selectedPackage)
        )
    , model.selectedPackage
      |> Maybe.map (displayPackageDetail model.repository)
      |> Maybe.withDefault (text "")
    ]

displayPackageSummary : (List String) -> Maybe Package -> Int -> Package -> Html Msg
displayPackageSummary repository selectedPackage index package =
  let
    (updated, missing) = List.partition (isUpdated repository) package.dependencies
  in
  li []
    [ button [ onClick (RemovePackage index) ] [ text "X" ]
    , text " "
    , a
      [ classList
        [ ("found", List.isEmpty missing)
        , ("project-name", True)
        ]
      , href "#"
      , onClick (SelectPackage package)
      ]
      [ text (Maybe.withDefault "--" package.name) ]
    , text " "
    , if List.isEmpty missing then
        text ""
      else
        span [ class "missing" ] [ text (String.fromInt (List.length missing)) ]
    , text " "
    , if List.isEmpty updated then
        text ""
      else
        span [ class "found" ] [ text (String.fromInt (List.length updated)) ]
    ]

displayPackageDetail : (List String) -> Package -> Html Msg
displayPackageDetail repository package =
  li []
    [ input
      [ class "project-name"
      , id ("project-detail")
      , onClick (SelectPackage package)
      , on "change" <| targetValue (RenamePackage package)
      , value (Maybe.withDefault "--" package.name)
      ] []
    , text " "
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
        [ span [ class "new-name" ] [ packageName found newName ]
        , text " ("
        , span [ class "old-name" ] [ packageName False name ]
        , text ")"
        ]
    )

isUpdated : (List String) -> String -> Bool
isUpdated repository name =
  let
    newName = translatePackageName name
  in
    List.member newName repository

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

targetValue : (String -> msg) -> Json.Decode.Decoder msg
targetValue tagger =
  Json.Decode.map tagger Html.Events.targetValue
