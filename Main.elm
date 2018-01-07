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
    { bonus : Bool
    , gameState : GameState
    , gridContents : Dict Int ContentType
    , level : Maybe Level
    , randomSequence : List Int
    , superBadGuyTick : Maybe Int
    , score : ( Int, Int, Int )
    , startedTime : Maybe Time
    , lastTick : Maybe Time
    , tickCount : Int
    }


initialModel : Model
initialModel =
    { bonus = False
    , gameState = Welcome
    , gridContents = Dict.empty
    , level = Nothing
    , randomSequence = []
    , superBadGuyTick = Nothing
    , score = ( 0, 0, 0 )
    , startedTime = Nothing
    , lastTick = Nothing
    , tickCount = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform StartedTime Time.now )



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
    | SuperFraudster
    | Client


type Msg
    = ClickBox Int
    | CreateClients
    | CreateFraudsters
    | GameEnded
    | InitialiseLevel
    | NoOp
    | Reset
    | StartGame
    | StartedTime Time
    | Tick Time



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickBox index ->
            let
                ( fraudsters, customers, superbadGuy ) =
                    model.score
            in
            case Dict.get index model.gridContents of
                Just SuperFraudster ->
                    ( { model
                        | score = ( fraudsters, customers, superbadGuy + 1 )
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                Just Fraudster ->
                    ( { model
                        | score = ( fraudsters + 1, customers, superbadGuy )
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
                        | score = ( fraudsters, customers + 1, superbadGuy )
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
                isSuperBadGuyTick =
                    case model.superBadGuyTick of
                        Just superBadGuyTick ->
                            superBadGuyTick == model.tickCount

                        Nothing ->
                            False

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
                    if isSuperBadGuyTick then
                        (rows * rows) - 2
                    else
                        (rows * rows) - 1

                superBadGuyGenerator =
                    if isSuperBadGuyTick then
                        createRandomNumberGeneratorList ((rows * rows) - 1) 1
                    else
                        []

                superBadGuyRandomNumber =
                    case model.lastTick of
                        Just time ->
                            randomSequence superBadGuyGenerator (floor time)

                        Nothing ->
                            []

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
                    let
                        superFraudster =
                            model.gridContents
                                |> Dict.toList
                                |> List.filter
                                    (\( index, value ) -> value == SuperFraudster)

                        superFraudsterHere index superFraudsterKeyValue =
                            case superFraudsterKeyValue of
                                Just ( superFraudsterKey, superFraudsterValue ) ->
                                    superFraudsterKey == index

                                Nothing ->
                                    False

                        pickCellType index superBadGuyRandomNumber clientRandomList fraudsterRandomList =
                            if List.member index superBadGuyRandomNumber then
                                SuperFraudster
                            else if List.member index clientRandomList then
                                Client
                            else if List.member index fraudsterRandomList then
                                Fraudster
                            else
                                Empty

                        _ =
                            Debug.log "test" ( superFraudster, model.tickCount, model.superBadGuyTick )
                    in
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
                                        if not (List.isEmpty superFraudster) && superFraudsterHere index (List.head superFraudster) then
                                            case model.superBadGuyTick of
                                                Just superBadGuyTick ->
                                                    if (superBadGuyTick + 3) > model.tickCount then
                                                        SuperFraudster
                                                    else
                                                        pickCellType index superBadGuyRandomNumber clientRandomList fraudsterRandomList

                                                Nothing ->
                                                    pickCellType index superBadGuyRandomNumber clientRandomList fraudsterRandomList
                                        else
                                            pickCellType index superBadGuyRandomNumber clientRandomList fraudsterRandomList
                                in
                                ( index, cellType )
                            )
                        |> Dict.fromList
            in
            ( { model | gridContents = gridContents }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { model | gameState = Welcome }, Cmd.none )

        StartGame ->
            let
                ( updatedState, updatedCmd ) =
                    update
                        InitialiseLevel
                        { model
                            | gameState = Playing
                            , level = Just Level1
                            , score = ( 0, 0, 0 )
                            , tickCount = 0
                            , superBadGuyTick = Nothing
                        }
            in
            ( updatedState, Cmd.batch [ updatedCmd, Task.perform StartedTime Time.now ] )

        StartedTime time ->
            let
                superBadGuyTick =
                    randomTick (floor time)
            in
            ( { model
                | startedTime = Just time
                , superBadGuyTick = Just superBadGuyTick
              }
            , Cmd.none
            )

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
                update
                    InitialiseLevel
                    { model
                        | lastTick = Just time
                        , level = level
                        , tickCount = model.tickCount + 1
                    }


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
        [ div [ class "score" ] [ span [] [ text "Score: " ], span [] [ text (toString (translateScore model.score)) ] ]
        , button [ class "reset", onClick Reset ] [ div [] [ text "X" ], span [] [ text "Reset" ] ]
        , div ([ class "grid-container" ] ++ levelClass) grid
        , button [ class "end-game", onClick GameEnded ] [ text "End Game" ]
        ]


resultsView : Model -> Html Msg
resultsView model =
    let
        ( fraudstersPercentage, customersPercentage, superbadGuyPercentage ) =
            scoreToPercentage model.score

        ( fraudsters, customers, superbadGuy ) =
            model.score
    in
    div []
        [ div [ class "score" ] [ span [] [ text "Score: " ], span [] [ text (toString (translateScore model.score)) ] ]
        , button [ class "reset", onClick Reset ] [ div [] [ text "X" ], span [] [ text "Reset" ] ]
        , div [ class "result-graph" ]
            [ div [ class "left-side" ]
                [ div [ class "label" ] [ text "Fraudsters" ]
                , div
                    [ class "bar"
                    , title ("- " ++ toString (customers * 100) ++ " points")
                    , style
                        [ ( "width", toString customersPercentage ++ "%" )
                        , ( "margin-left", toString (100 - customersPercentage) ++ "%" )
                        ]
                    ]
                    []
                , div [ class "label" ] [ text "Super Fraudster" ]
                ]
            , div [ class "seperator" ] []
            , div [ class "right-side" ]
                [ div
                    [ class "bar"
                    , title (toString (fraudsters * 50) ++ " points")
                    , style [ ( "width", toString fraudstersPercentage ++ "%" ) ]
                    ]
                    []
                , div [ class "label" ] [ text "Customers" ]
                , div
                    [ class "bar"
                    , title (toString (superbadGuy * 250) ++ " points")
                    , style [ ( "width", toString superbadGuyPercentage ++ "%" ) ]
                    ]
                    []
                ]
            ]
        , div [ class "playing-time" ] [ text (calculatePlayingTime model.lastTick model.startedTime) ]
        ]


scoreToPercentage : ( Int, Int, Int ) -> ( Float, Float, Float )
scoreToPercentage ( fraudsters, customers, superbadGuy ) =
    let
        fraudstersScore =
            fraudsters * 50

        customersScore =
            customers * 100

        superbadGuyScore =
            superbadGuy * 250

        total =
            fraudstersScore + customersScore + superbadGuyScore
    in
    ( if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat fraudstersScore / toFloat total) * 100
    , if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat customersScore / toFloat total) * 100
    , if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat superbadGuyScore / toFloat total) * 100
    )


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


randomTick : Int -> Int
randomTick seed =
    let
        ( generatorOutput, _ ) =
            Random.step (Random.int 1 12) (Random.initialSeed seed)
    in
    generatorOutput


scoreToLevel : ( Int, Int, Int ) -> Level
scoreToLevel ( fraudsters, customers, superbadGuy ) =
    let
        translatedScore =
            superbadGuy * 250 + fraudsters * 50
    in
    if translatedScore < 350 then
        Level1
    else if translatedScore >= 350 && translatedScore < 1000 then
        Level2
    else if translatedScore >= 1000 && translatedScore < 2000 then
        Level3
    else
        Level4


translateScore : ( Int, Int, Int ) -> Int
translateScore ( fraudsters, customers, superbadGuy ) =
    superbadGuy * 250 + fraudsters * 50 - customers * 100


calculatePlayingTime : Maybe Time -> Maybe Time -> String
calculatePlayingTime lastTick startedTime =
    case ( lastTick, startedTime ) of
        ( Just lastTick_, Just startedTime_ ) ->
            if lastTick_ > startedTime_ then
                "Played for " ++ toString (Time.inSeconds (lastTick_ - startedTime_)) ++ " Seconds"
            else
                ""

        _ ->
            ""


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

                            Just SuperFraudster ->
                                [ class "super-fraudster" ]

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
