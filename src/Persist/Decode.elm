module Persist.Decode exposing (persist)

import Persist exposing (Persist, Package)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map Persist
    (field "packages" (list package))

package : Decoder Package
package =
  map2 Package
    (field "name" (nullable string))
    (field "packageText" string)
