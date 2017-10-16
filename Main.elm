module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Random
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gameState : GameState
    , gridContents : Dict Int ContentType
    , level : Maybe Level
    , score : Int
    , startedTime : Maybe Time
    }


initialModel : Model
initialModel =
    { gameState = Welcome
    , gridContents = Dict.empty
    , level = Nothing
    , score = 0
    , startedTime = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getTime )



-- MESSAGES


type GameState
    = Welcome
    | Playing
    | Results


type Level
    = Level1
    | Level2
    | Level3


type ContentType
    = Empty
    | Fraudster
    | Client


type Msg
    = ClickBox Int
    | CreateClients
    | CreateFraudsters
    | InitialiseLevel
    | NoOp
    | StartGame
    | StartedTime Time



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickBox index ->
            let
                _ =
                    Debug.log "clicked" (Dict.get index model.gridContents)
            in
            ( model, Cmd.none )

        CreateClients ->
            ( model, Cmd.none )

        CreateFraudsters ->
            ( model, Cmd.none )

        InitialiseLevel ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            let
                ( numberOfClients, numberOfFraudsters, rows ) =
                    case updatedModel.level of
                        Just Level1 ->
                            ( 2, 1, 3 )

                        Just Level2 ->
                            ( 4, 2, 5 )

                        Just Level3 ->
                            ( 6, 3, 7 )

                        Nothing ->
                            ( 0, 0, 0 )

                clientRandomList =
                    case updatedModel.startedTime of
                        Just time ->
                            randomSequence numberOfClients 1 (rows * rows) (floor time)

                        Nothing ->
                            []

                fraudsterRandomList =
                    case updatedModel.startedTime of
                        Just time ->
                            randomSequence numberOfFraudsters 1 (rows * rows) (floor (time / 2))

                        Nothing ->
                            []

                gridContents =
                    List.range 1 (rows * rows)
                        |> List.filter
                            (\index ->
                                let
                                    floatRows =
                                        toFloat rows
                                in
                                not (toFloat index == ((floatRows * floatRows + 1) / 2))
                            )
                        |> List.map
                            (\index ->
                                let
                                    cellType =
                                        if List.member index clientRandomList then
                                            Client
                                        else if List.member index fraudsterRandomList then
                                            Fraudster
                                        else
                                            Empty
                                in
                                ( index, cellType )
                            )
                        |> Dict.fromList

                ( updatedModel, updatedCmd ) =
                    update
                        InitialiseLevel
                        { model
                            | gameState = Playing
                            , level = Just Level1
                        }
            in
            ( { updatedModel | gridContents = gridContents }, updatedCmd )

        StartedTime time ->
            ( { model | startedTime = Just time }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( levelClass, grid ) =
            case model.level of
                Just Level1 ->
                    ( [ class "grid-container--level1" ], makeGrid 3 model.gridContents )

                Just Level2 ->
                    ( [ class "grid-container--level2" ], makeGrid 5 model.gridContents )

                Just Level3 ->
                    ( [ class "grid-container--level3" ], makeGrid 7 model.gridContents )

                Nothing ->
                    ( [], [] )
    in
    case model.gameState of
        Welcome ->
            div [ class "welcome-container" ]
                [ h1 [] [ text "Welcome to Whack-a-Fraudster" ]
                , button [ onClick StartGame ] [ text "Start" ]
                ]

        Playing ->
            div []
                [ div ([ class "grid-container" ] ++ levelClass) grid ]

        Results ->
            div [] [ text (toString model.score) ]


getTime =
    Task.perform StartedTime Time.now


randomSequence : Int -> Int -> Int -> Int -> List Int
randomSequence numberOfValues min max seed =
    let
        generator =
            Random.list numberOfValues (Random.int min max)

        randomSeed =
            Random.initialSeed seed

        ( result, nextSeed ) =
            Random.step generator randomSeed
    in
    result


scoreToLevel : Int -> Level
scoreToLevel score =
    if score < 350 then
        Level1
    else if score >= 350 && score < 1000 then
        Level2
    else
        Level3


makeGrid : Int -> Dict Int ContentType -> List (Html Msg)
makeGrid rows gridContents =
    List.range 1 (rows * rows)
        |> List.map
            (\index ->
                let
                    floatRows =
                        toFloat rows

                    logoItem =
                        if toFloat index == ((floatRows * floatRows + 1) / 2) then
                            [ class "grid-logo" ]
                        else
                            []

                    contentClass =
                        case Dict.get index gridContents of
                            Just Client ->
                                [ class "client" ]

                            Just Fraudster ->
                                [ class "fraudster" ]

                            _ ->
                                []
                in
                div
                    ([ class "grid-item" ]
                        ++ [ class ("grid-item-" ++ toString index) ]
                        ++ logoItem
                        ++ contentClass
                        ++ [ onClick (ClickBox index) ]
                    )
                    []
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
