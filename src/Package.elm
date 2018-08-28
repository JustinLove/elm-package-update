module Package exposing (Package, package, dependencies)

import Json.Decode exposing (..)
import Url

type alias Package =
  { packageText : String
  , name : Maybe String
  , dependencies : List String
  }

package : String -> Result Json.Decode.Error Package
package text =
  decodeString (packageDecoder text) text

packageDecoder : String -> Decoder Package
packageDecoder text =
  map2 (Package text)
    packageName
    dependencies

packageName : Decoder (Maybe String)
packageName =
  map (Url.fromString >> (Maybe.map (.path >>(String.dropRight 4)>>(String.dropLeft 1))))
    (field "repository" string)

dependencies : Decoder (List String)
dependencies =
  map (List.map Tuple.first)
    (field "dependencies" (keyValuePairs string))
