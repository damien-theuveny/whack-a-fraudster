port module Ports exposing (..)

import Json.Decode


port connectionOpenSignal : (() -> msg) -> Sub msg


port connections : (Int -> msg) -> Sub msg


port gameStartedByLead : () -> Cmd msg


port registeredAsLeadPlayer : (Bool -> msg) -> Sub msg


port requestForScores : () -> Cmd msg


port sendGridContents : String -> Cmd msg


port sendScores : (Json.Decode.Value -> msg) -> Sub msg


port updateGridContents : (Json.Decode.Value -> msg) -> Sub msg


port sendPlayerIsReady : () -> Cmd msg


port sendPlayerName : String -> Cmd msg


port startGame : (() -> msg) -> Sub msg


port storeScore : ( String, Int ) -> Cmd msg


port updateReadyCount : (Int -> msg) -> Sub msg
