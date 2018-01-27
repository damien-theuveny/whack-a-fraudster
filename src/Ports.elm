port module Ports exposing (..)

import Json.Decode


port connectionOpenSignal : (() -> msg) -> Sub msg


port connections : (Json.Decode.Value -> msg) -> Sub msg


port clickBox : Int -> Cmd msg


port clientNames : (Json.Decode.Value -> msg) -> Sub msg


port gameStartedByLead : () -> Cmd msg


port invalidName : (() -> msg) -> Sub msg


port registeredAsLeadPlayer : (Json.Decode.Value -> msg) -> Sub msg


port requestForScores : () -> Cmd msg


port resetServer : () -> Cmd msg


port sendEndGameSignal : () -> Cmd msg


port sendGridContents : String -> Cmd msg


port sendLevel : Int -> Cmd msg


port sendResetSignal : () -> Cmd msg


port sendScores : (Json.Decode.Value -> msg) -> Sub msg


port sendPlayerIsReady : () -> Cmd msg


port sendPlayerName : String -> Cmd msg


port startGame : (() -> msg) -> Sub msg


port storeScore : ( String, Int ) -> Cmd msg


port updateClickBox : (Int -> msg) -> Sub msg


port updateEndGame : (() -> msg) -> Sub msg


port updateGridContents : (String -> msg) -> Sub msg


port updateLevel : (Int -> msg) -> Sub msg


port updateReadyCount : (Int -> msg) -> Sub msg
