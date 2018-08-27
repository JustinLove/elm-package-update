module ElmPackageUpdate exposing (..)

import View

import Browser

type Msg
  = None

type alias Model =
  {
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
  ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
    ]
