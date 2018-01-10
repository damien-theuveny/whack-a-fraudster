port module Ports exposing (..)

import Json.Decode


port sendScores : (Json.Decode.Value -> msg) -> Sub msg


port requestForScores : () -> Cmd msg


port storeScore : ( String, Int ) -> Cmd msg
