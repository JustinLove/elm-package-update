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
import Dict
import Http
import Json.Decode

type Msg
  = Loaded (Maybe Persist)
  | PackageList (Result Http.Error (List String))
  | PackageLoaded (Result Json.Decode.Error Package)
  | PackageUrlLoaded (Result Http.Error Package)
  | UI (View.Msg)

type alias Model =
  { packages : List Package
  , repository : List String
  , selectedPackage : Maybe Package
  }

main : Program String Model Msg
main =
  Browser.document
    { init = init
    , view = View.document UI
    , update = update
    , subscriptions = subscriptions
    }

init : String -> (Model, Cmd Msg)
init search =
  let
    slocal = Debug.log "slocal" (extractSearchArgument "local" search |> Maybe.withDefault "")
    local = Debug.log "local" (not (slocal == "" || slocal == "false"))
    murl = Debug.log "url" (extractSearchArgument "url" search)
  in
  ( { packages = []
    , repository = []
    , selectedPackage = Nothing
    }
    , Cmd.batch 
      [ if local then
          localPackageList
        else
          fetchPackageList
      , murl |> Maybe.map fetchPackageFromUrl |> Maybe.withDefault Cmd.none
      ]
     )

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
      { model
      | packages =
        if List.any (\{packageText} -> packageText == package.packageText) model.packages then
          model.packages
        else
          package :: model.packages
      , selectedPackage = Just package
      }
        |> persist
    PackageLoaded (Err err) ->
      let _ = Debug.log "elm-package.json import failed" err in
      (model, Cmd.none)
    PackageUrlLoaded (Ok package) ->
      { model
      | packages =
        if List.any (\{packageText} -> packageText == package.packageText) model.packages then
          model.packages
        else
          package :: model.packages
      , selectedPackage = Just package
      }
        |> persist
    PackageUrlLoaded (Err err) ->
      let _ = Debug.log "elm-package.json fetch failed" err in
      (model, Cmd.none)
    UI (View.LoadPackage files) ->
      (model, FileInput.read files)
    UI (View.LoadUrl url) ->
      (model, fetchPackageFromUrl url)
    UI (View.RemovePackage index) ->
      { model
      | packages = List.append
        (List.take index model.packages)
        (List.drop (index+1) model.packages)
      , selectedPackage = Nothing
      }
        |> persist
    UI (View.SelectPackage package) ->
      ({model | selectedPackage = Just package}, Cmd.none)
    UI (View.RenamePackage package name) ->
      { model
      | packages = model.packages |> List.map (\pack ->
        if pack == package then
          {pack | name = Just name}
        else
          pack
        )
      }
        |> persist

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

fetchPackageFromUrl : String -> Cmd Msg
fetchPackageFromUrl url =
  Http.getString url
    |> Http.send receivePackageResponse

localPackageList : Cmd Msg
localPackageList =
  Http.get "search.json" PackageRepository.packageList
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

receivePackageResponse : (Result Http.Error String) -> Msg
receivePackageResponse res =
  case res of
    Ok string ->
      string
        |> Package.package
        |> Result.mapError (Debug.log "package decode error")
        |> Result.mapError (\err ->
          ( Http.BadPayload
            (Debug.toString err)
            (Http.Response "url" {code = 200, message = "OK"} Dict.empty string)
          )
        )
        |> PackageUrlLoaded
    Err err ->
      PackageUrlLoaded (Err err)

extractSearchArgument : String -> String -> Maybe String
extractSearchArgument key search =
  search
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map (String.split "=")
    |> List.filter (\x -> case List.head x of
      Just s ->
        (String.toLower s) == (String.toLower key)
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
