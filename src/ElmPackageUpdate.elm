module ElmPackageUpdate exposing (..)

import Package exposing (Package)
import View

import Browser

type Msg
  = None

type alias Model =
  { packages : List Package
  }

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = View.document
    , update = update
    , subscriptions = subscriptions
    }

init : flags -> (Model, Cmd Msg)
init flags =
  ( { packages = [Package.package samplePackage]
    }
  , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
    ]

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
