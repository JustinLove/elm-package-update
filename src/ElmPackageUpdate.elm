module ElmPackageUpdate exposing (..)

import FileInput
import LocalStorage
import Package exposing (Package)
import PackageRepository
import Persist exposing (Persist)
import Persist.Encode
import Persist.Decode
import View

import Browser
import Http
import Json.Decode

type Msg
  = Loaded (Maybe Persist)
  | PackageList (Result Http.Error (List String))
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
  ( { packages = []
    , repository = []
    }
    , fetchPackageList)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( case mstate of
        Just state ->
          ( {model | packages = state.packages
              |> List.map loadPackage
              |> List.filterMap Result.toMaybe
            }
          , Cmd.none)
        Nothing ->
          (model, Cmd.none)
      )
    PackageList (Ok packages) ->
      ({model | repository = packages}, Cmd.none)
    PackageList (Err err) ->
      let _ = Debug.log "failed to get package list" err in
      (model, Cmd.none)
    PackageLoaded (Ok package) ->
      {model | packages = package :: model.packages}
        |> persist
    PackageLoaded (Err err) ->
      let _ = Debug.log "elm-package.json import failed" err in
      (model, Cmd.none)
    UI (View.LoadPackage files) ->
      (model, FileInput.read files)

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist
      (List.map
        (\{packageText, name} -> Persist.Package name packageText)
        model.packages
      )
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

loadPackage : Persist.Package -> Result Json.Decode.Error Package
loadPackage pack =
  Package.package pack.packageText
    |> Result.map (\mp -> {mp | name = pack.name})

fetchPackageList : Cmd Msg
fetchPackageList =
  Http.get "https://b49edyybqh.execute-api.us-east-1.amazonaws.com/production/search.json" PackageRepository.packageList
    |> Http.send PackageList

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ FileInput.fileContents receivePackageFile
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    ]

receivePackageFile : String -> Msg
receivePackageFile string =
  string
    |> Package.package
    |> Result.mapError (Debug.log "package decode error")
    |> PackageLoaded
