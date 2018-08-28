module Persist.Encode exposing (persist)

import Persist exposing (Persist, Package)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("packages", list package p.packages)
    ]

package : Package -> Value
package pack =
  object 
    [ ("name", maybe string pack.name)
    , ("packageText", string pack.packageText)
    ]

maybe : (a -> Value) -> Maybe a -> Value
maybe encode m =
  case m of
    Just v -> encode v
    Nothing -> null
