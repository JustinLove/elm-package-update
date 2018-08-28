module PackageRepository exposing (packageList)

import Json.Decode exposing (..)

packageList : Decoder (List String)
packageList =
  list (field "name" string)
