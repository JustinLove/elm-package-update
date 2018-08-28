module ElmPackageUpdate exposing (..)

import FileInput
import Package exposing (Package)
import PackageRepository
import View

import Browser
import Http
import Json.Decode

type Msg
  = PackageList (Result Http.Error (List String))
  | PackageLoaded (Result Json.Decode.Error Package)
  | UI (View.Msg)

type alias Model =
  { packages : List Package
  , repository : List String
  }

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = View.document UI
    , update = update
    , subscriptions = subscriptions
    }

init : flags -> (Model, Cmd Msg)
init flags =
  ( { packages =
      case Package.package samplePackage of
        Ok pack ->
          [pack]
        Err err ->
          let _ = Debug.log "decode error" err in []
    , repository = []
    }
    , fetchPackageList)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PackageList (Ok packages) ->
      ({model | repository = packages}, Cmd.none)
    PackageList (Err err) ->
      let _ = Debug.log "failed to get package list" err in
      (model, Cmd.none)
    PackageLoaded (Ok package) ->
      ({model | packages = package :: model.packages}, Cmd.none)
    PackageLoaded (Err err) ->
      let _ = Debug.log "elm-package.json import failed" err in
      (model, Cmd.none)
    UI (View.LoadPackage files) ->
      (model, FileInput.read files)

fetchPackageList : Cmd Msg
fetchPackageList =
  Http.get "https://b49edyybqh.execute-api.us-east-1.amazonaws.com/production/search.json" PackageRepository.packageList
    |> Http.send PackageList

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ FileInput.fileContents receivePackageFile
    ]

receivePackageFile : String -> Msg
receivePackageFile string =
  string
    |> Package.package
    |> Result.mapError (Debug.log "package decode error")
    |> PackageLoaded

samplePackage = """
{
    "version": "1.0.0",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "Apache-2.0",
    "source-directories": [
        "src"
    ],
    "exposed-modules": [],
    "dependencies": {
        "JustinLove/elm-twitch-api": "2.0.0 <= v < 3.0.0",
        "antivanov/eunit": "1.0.0 <= v < 2.0.0",
        "elm-lang/core": "5.1.1 <= v < 6.0.0",
        "elm-lang/dom": "1.1.1 <= v < 2.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0",
        "elm-lang/http": "1.0.0 <= v < 2.0.0",
        "elm-lang/svg": "2.0.0 <= v < 3.0.0",
        "elm-tools/parser": "2.0.1 <= v < 3.0.0",
        "timjs/elm-collage": "1.6.0 <= v < 2.0.0",
        "truqu/elm-base64": "2.0.3 <= v < 3.0.0"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
"""
