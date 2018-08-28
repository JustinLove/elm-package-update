module Persist exposing (Persist, Package)

type alias Persist =
  { packages : List Package
  }

type alias Package =
  { name : Maybe String
  , packageText : String
  }
