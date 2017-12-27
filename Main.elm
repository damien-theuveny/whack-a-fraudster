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
    , randomSequence : List Int
    , score : Int
    , startedTime : Maybe Time
    , lastTick : Maybe Time
    }


initialModel : Model
initialModel =
    { gameState = Welcome
    , gridContents = Dict.empty
    , level = Nothing
    , randomSequence = []
    , score = 0
    , startedTime = Nothing
    , lastTick = Nothing
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
    | Level4


type ContentType
    = Empty
    | Fraudster
    | Client


type Msg
    = ClickBox Int
    | CreateClients
    | CreateFraudsters
    | GameEnded
    | InitialiseLevel
    | NoOp
    | StartGame
    | StartedTime Time
    | Tick Time



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickBox index ->
            case Dict.get index model.gridContents of
                Just Fraudster ->
                    ( { model
                        | score = model.score + 50
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                Just Client ->
                    ( { model
                        | score = model.score - 100
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CreateClients ->
            ( model, Cmd.none )

        CreateFraudsters ->
            ( model, Cmd.none )

        GameEnded ->
            ( { model | gameState = Results }, Cmd.none )

        InitialiseLevel ->
            let
                ( numberOfClients, numberOfFraudsters, rows ) =
                    case model.level of
                        Just Level1 ->
                            ( 2, 1, 3 )

                        Just Level2 ->
                            ( 4, 2, 5 )

                        Just Level3 ->
                            ( 6, 3, 7 )

                        Just Level4 ->
                            ( 8, 5, 9 )

                        Nothing ->
                            ( 0, 0, 0 )

                emptySpaces =
                    (rows * rows) - 1

                generators =
                    createRandomNumberGeneratorList emptySpaces (numberOfClients + numberOfFraudsters)

                randomNumbers =
                    -- correctNumbers ((toFloat rows * toFloat rows + 1) / 2) [ 1, 1, 1 ]
                    case ( model.lastTick, model.startedTime ) of
                        ( Just time, _ ) ->
                            randomSequence generators (floor time)
                                |> correctNumbers ((toFloat rows * toFloat rows + 1) / 2)

                        ( _, Just time ) ->
                            randomSequence generators (floor time)
                                |> correctNumbers ((toFloat rows * toFloat rows + 1) / 2)

                        _ ->
                            []

                nextAvailibleNumber currentNumber centre list =
                    let
                        filtered =
                            List.range 1 (rows * rows)
                                |> List.filter
                                    (\item ->
                                        (toFloat item /= centre) && not (List.member item list)
                                    )
                    in
                    List.drop (currentNumber - 1) filtered
                        |> List.head

                correctNumbers centre list =
                    list
                        |> List.map
                            (\item ->
                                if toFloat item >= centre then
                                    item + 1
                                else
                                    item
                            )
                        |> List.indexedMap
                            (\index randomNumber ->
                                let
                                    listSoFar =
                                        List.take index list
                                            |> correctNumbers centre

                                    correctedRandomNumber =
                                        case nextAvailibleNumber randomNumber centre listSoFar of
                                            Just number ->
                                                number

                                            _ ->
                                                randomNumber
                                in
                                correctedRandomNumber
                            )

                clientRandomList =
                    randomNumbers
                        |> List.take numberOfClients

                fraudsterRandomList =
                    randomNumbers
                        |> List.drop numberOfClients

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
            in
            ( { model | gridContents = gridContents }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            update
                InitialiseLevel
                { model
                    | gameState = Playing
                    , level = Just Level1
                    , score = 0
                }

        StartedTime time ->
            ( { model | startedTime = Just time }, Cmd.none )

        Tick time ->
            let
                level =
                    Just (scoreToLevel model.score)

                gameEnded =
                    case ( level, model.startedTime ) of
                        ( Just Level1, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 20

                        ( Just Level2, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 40

                        ( Just Level3, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 60

                        ( Just Level4, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 80

                        _ ->
                            False
            in
            if gameEnded then
                update GameEnded model
            else
                update InitialiseLevel { model | lastTick = Just time, level = level }


insertContentType : Int -> ContentType -> Dict Int ContentType -> Dict Int ContentType
insertContentType index contentType gridContents =
    Dict.insert index contentType gridContents



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Welcome ->
            welcomeView

        Playing ->
            inGameView model

        Results ->
            resultsView model


welcomeView : Html Msg
welcomeView =
    div [ class "welcome-container" ]
        [ h1 [] [ text "Welcome to Whack-a-Fraudster" ]
        , button [ class "start", onClick StartGame ] [ text "Start" ]
        ]


inGameView : Model -> Html Msg
inGameView model =
    let
        ( levelClass, grid ) =
            case model.level of
                Just Level1 ->
                    ( [ class "grid-container--level1" ], makeGrid 3 model.gridContents )

                Just Level2 ->
                    ( [ class "grid-container--level2" ], makeGrid 5 model.gridContents )

                Just Level3 ->
                    ( [ class "grid-container--level3" ], makeGrid 7 model.gridContents )

                Just Level4 ->
                    ( [ class "grid-container--level4" ], makeGrid 9 model.gridContents )

                Nothing ->
                    ( [], [] )
    in
    div []
        [ div [ class "score" ] [ text (toString model.score) ]
        , button [ class "reset", onClick NoOp ] [ text "X" ]
        , div ([ class "grid-container" ] ++ levelClass) grid
        ]


resultsView : Model -> Html Msg
resultsView model =
    div []
        [ div [ class "score" ] [ text (toString model.score) ]

        -- , div [] [ text Time.inSeconds (model.lastTick - model.startedTime)]
        ]


getTime =
    Task.perform StartedTime Time.now


createRandomNumberGeneratorList : Int -> Int -> List (Random.Generator Int)
createRandomNumberGeneratorList emptyCells countOfGenerators =
    List.range 1 emptyCells
        |> List.filterMap
            (\index ->
                if index <= countOfGenerators then
                    Just (Random.int 1 (emptyCells - (index - 1)))
                else
                    Nothing
            )


randomSequence : List (Random.Generator Int) -> Int -> List Int
randomSequence generators seed =
    generators
        |> List.indexedMap
            (\index generator ->
                let
                    randomSeed =
                        Random.initialSeed seed

                    ( generatorOutput, _ ) =
                        Random.step generator (Random.initialSeed ((index + 1) * seed))
                in
                generatorOutput
            )


scoreToLevel : Int -> Level
scoreToLevel score =
    if score < 350 then
        Level1
    else if score >= 350 && score < 1000 then
        Level2
    else if score >= 1000 && score < 2000 then
        Level3
    else
        Level4


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
    let
        interval =
            case model.level of
                Just Level1 ->
                    Time.second * 2

                Just Level2 ->
                    Time.second * 1.5

                Just Level3 ->
                    Time.second * 1

                _ ->
                    Time.second * 0.75
    in
    case model.gameState of
        Playing ->
            Time.every interval Tick

        _ ->
            Sub.none
