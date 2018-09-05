module View exposing (Msg(..), view, document)

import Package exposing (Package)
import FileInput

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode

type Msg
  = LoadPackage FileInput.Files
  | LoadUrl String
  | RemovePackage Int
  | SelectPackage Package
  | RenamePackage Package String

css = """
/* Dead Simple Grid (c) 2015 Vladimir Agafonkin */

.row .row { margin:  0 -1.5em; }
.col      { padding: 0  1.5em; }

.row:after {
    content: "";
    clear: both;
    display: table;
}

@media only screen { .col {
    float: left;
    width: 100%;
    box-sizing: border-box;
}}
/* End DSG */

.pick-file { max-width: 20em; }
.pick-url { max-width: 20em; }
.known-packages { max-width: 20em; }
.package-summary { max-width: 20em; }
.package-detail { max-width: 30em; }

@media only screen and (min-width: 50em) {
  .pick-file { width: 33%; }
  .pick-url { width: 33%; }
  .known-packages { width: 33%; }
  .package-summary { width: 40%; }
  .package-detail { width: 60%; }
}

header { margin-bottom: 1em; }
.package-summary { margin-top: 0; }
.known-packages { margin-top: 0; margin-bottom: 0;}

.found, .found a  { color: green; }
.missing, .missing a {
  color: white;
  background-color: red;
  padding-left: 0.2em;
  padding-right: 0.2em;
}
.old-name .missing, .old-name .missing a { color: red; background-color: transparent; opacity: 0.5; }
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
  div [ class "col" ]
    [ node "style" [] [ text css ]
    , header [ class "row" ]
      [ p [ class "known-packages col" ]
        [ text (model.repository |> List.length |> String.fromInt)
        , text " known packages"
        ]
      , div [ class "pick-file col" ]
        [ input
          [ type_ "file"
          , on "change" (FileInput.targetFiles LoadPackage)
          ]
          []
        ]
      , div [ class "pick-url col" ]
        [ input
          [ class "package-url"
          , id ("package-url")
          , on "change" <| targetValue LoadUrl
          , placeholder "https://../elm-package.json"
          , value ""
          ] []
        ]
      ]
    , div [ class "row" ]
      [ table [ class "package-summary col" ]
          (model.packages
            |> List.indexedMap (displayPackageSummary model.repository model.selectedPackage)
          )
      , div [ class "package-detail col" ]
        [ model.selectedPackage
          |> Maybe.map (displayPackageDetail model.repository)
          |> Maybe.withDefault (text "")
        ]
      ]
    ]

displayPackageSummary : (List String) -> Maybe Package -> Int -> Package -> Html Msg
displayPackageSummary repository selectedPackage index package =
  let
    (updated, missing) = List.partition (isUpdated repository) package.dependencies
  in
  tr []
    [ td [] [ button [ onClick (RemovePackage index) ] [ text "X" ] ]
    , td [] [ if List.isEmpty updated then
        text ""
      else
        span [ class "found" ] [ text (String.fromInt (List.length updated)) ]
      ]
    , td [] [ if List.isEmpty missing then
        text ""
      else
        span [ class "missing" ] [ text (String.fromInt (List.length missing)) ]
        ]
    , td [] [ a
      [ classList
        [ ("found", List.isEmpty missing)
        , ("project-name", True)
        ]
      , href "#"
      , onClick (SelectPackage package)
      ]
      [ text (Maybe.withDefault "--" package.name) ]
      ]
    ]

displayPackageDetail : (List String) -> Package -> Html Msg
displayPackageDetail repository package =
  div []
    [ input
      [ class "project-name"
      , id ("project-detail")
      , on "change" <| targetValue (RenamePackage package)
      , value (Maybe.withDefault "--" package.name)
      ] []
    , text " "
    , ul []
      (List.map (displayDependency repository) package.dependencies)
    ]

displayDependency : (List String) -> String -> Html Msg
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

packageName : Bool -> String -> Html Msg
packageName found name =
  span
    [ if found then
        class "found"
      else
        class "missing"
    ]
    [ a
      [ href ("https://package.elm-lang.org/packages/" ++ name ++ "/latest")
      ]
      [ text name ]
    , if not found then
        span []
          [ text " "
          , button
            [ onClick (LoadUrl ("https://raw.githubusercontent.com/" ++ name ++ "/master/elm-package.json")) ]
            [ text "view" ]
          ]
      else
        text ""
    ]

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
    "elm-community/elm-test" -> "elm-explorations/test"
    "mgold/elm-random-pcg" -> "elm/random"
    _ -> String.replace "elm-lang/" "elm/" name

targetValue : (String -> msg) -> Json.Decode.Decoder msg
targetValue tagger =
  Json.Decode.map tagger Html.Events.targetValue
