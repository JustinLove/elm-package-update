module Package exposing (Package, package, dependencies)

import Json.Decode exposing (..)

type alias Package =
  { packageText : String
  , dependencies : List String
  }

package : String -> Package
package text =
  case (decodeString dependencies text) of
    Ok deps ->
      Package text deps
    Err msg ->
      let _ = Debug.log "package decode failure" msg in
      Package text []

dependencies : Decoder (List String)
dependencies =
  map (List.map Tuple.first)
  (field "dependencies" (keyValuePairs string))
