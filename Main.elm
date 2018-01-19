module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Encode
import Ports
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
    , multiplayerMode :
        { connectedClients : Maybe Int
        , playerColour : String
        , playerNames : List ClientProperties
        , followingLead : Bool
        , multiplayer : Bool
        , playerIsLead : Bool
        , readyClients : Int
        , requestingPlayerName : Bool
        }
    , playerName : String
    , playerScores : List PlayerScore
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
    , multiplayerMode =
        { connectedClients = Nothing
        , playerColour = "black"
        , playerNames = []
        , followingLead = False
        , multiplayer = False
        , playerIsLead = False
        , readyClients = 0
        , requestingPlayerName = False
        }
    , playerName = ""
    , playerScores = []
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
    = ApplyTick
    | ChangeName String
    | ClickBox Int
    | GameEnded
    | MultiplayerConnectionOpenned
    | PlayerRegistered (Result String Registration)
    | PlaySinglePlayer
    | ReceiveScores (Result String (List PlayerScore))
    | RequestNewName
    | Reset
    | SendName
    | SendScore
    | StartGame
    | StartMultiplayerGame
    | StartedTime Time
    | Tick Time
    | UpdateClientNames (Result String (List ClientProperties))
    | UpdateConnections Int
    | UpdateMultiplayerGridContents (Result String (List ( Int, String )))
    | UpdateMultiplayerLevel Int
    | UpdateReadyCount Int


type alias PlayerScore =
    { name : String
    , score : Int
    }


type alias Registration =
    { lead : Bool
    , colour : String
    }


type alias ClientProperties =
    { name : String
    , colour : String
    , screensize : Screensize
    , lastLocation : Location
    }


type alias Location =
    { x : Int
    , y : Int
    }


type alias Screensize =
    { width : Int
    , height : Int
    }



-- ApplyTick Helpers


getLevelConfig : Maybe Level -> ( Int, Int, Int )
getLevelConfig level =
    case level of
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


isSuperBadGuyTick : Maybe Int -> Int -> Bool
isSuperBadGuyTick superBadGuyTick tickCount =
    case superBadGuyTick of
        Just superBadGuyTick ->
            superBadGuyTick == tickCount

        Nothing ->
            False


getSuperBadGuyFromGrid : Maybe Int -> Int -> Dict Int ContentType -> List ( Int, ContentType )
getSuperBadGuyFromGrid superBadGuyTick tickCount gridContents =
    case superBadGuyTick of
        Just superBadGuyTick ->
            if (superBadGuyTick + 2) >= tickCount then
                Dict.toList gridContents
                    |> List.filter
                        (\( index, cellType ) ->
                            cellType == SuperFraudster
                        )
            else
                []

        Nothing ->
            []


adjustListsWithSuperBadGuy : Int -> List ( Int, ContentType ) -> List ( Int, ContentType ) -> List ( Int, ContentType )
adjustListsWithSuperBadGuy rows joinedLists superBadGuyRemains =
    case List.head superBadGuyRemains of
        Just ( index, _ ) ->
            let
                adjustedIndex =
                    if index >= floor ((toFloat rows * toFloat rows) / 2) then
                        index - 1
                    else
                        index

                beforeSuperBadGuy =
                    List.take adjustedIndex joinedLists

                afterSuperBadGuy =
                    List.drop adjustedIndex joinedLists
            in
                beforeSuperBadGuy
                    ++ [ ( index, SuperFraudster ) ]
                    ++ afterSuperBadGuy
                    |> List.indexedMap
                        (\index ( _, cellType ) ->
                            if index >= floor ((toFloat rows * toFloat rows) / 2) then
                                ( index + 1, cellType )
                            else
                                ( index, cellType )
                        )

        Nothing ->
            joinedLists



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplyTick ->
            let
                superBadGuyRemains =
                    getSuperBadGuyFromGrid model.superBadGuyTick model.tickCount model.gridContents

                ( numberOfClients, numberOfFraudsters, rows ) =
                    getLevelConfig model.level

                emptySpaces =
                    if List.isEmpty superBadGuyRemains then
                        (rows * rows) - 1
                    else
                        (rows * rows) - 2

                cellContentList =
                    createContentList emptySpaces numberOfFraudsters numberOfClients (isSuperBadGuyTick model.superBadGuyTick model.tickCount)

                randomNumberList =
                    case ( model.lastTick, model.startedTime ) of
                        ( Just time, _ ) ->
                            randomList (floor time) emptySpaces

                        ( _, Just time ) ->
                            randomList (floor time) emptySpaces

                        _ ->
                            []

                joinedLists =
                    List.map2 (\content randomNumber -> ( content, randomNumber )) cellContentList randomNumberList
                        |> List.sortBy (\( _, randomNumber ) -> randomNumber)
                        |> List.indexedMap
                            (\index ( cellType, _ ) ->
                                if index >= floor ((toFloat rows * toFloat rows) / 2) then
                                    ( index + 1, cellType )
                                else
                                    ( index, cellType )
                            )

                gridContents =
                    adjustListsWithSuperBadGuy rows joinedLists superBadGuyRemains
                        |> Dict.fromList
            in
                if model.multiplayerMode.playerIsLead && model.multiplayerMode.multiplayer then
                    ( { model | gridContents = gridContents }
                    , Ports.sendGridContents (encodeGridContents gridContents)
                    )
                else
                    ( { model | gridContents = gridContents }, Cmd.none )

        ClickBox index ->
            let
                ( fraudsters, customers, superbadGuy ) =
                    model.score

                cmd =
                    if model.multiplayerMode.multiplayer then
                        Ports.clickBox index
                    else
                        Cmd.none
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
                        , cmd
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
                        , cmd
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
                        , cmd
                        )

                    _ ->
                        ( model, Cmd.none )

        ChangeName name ->
            ( { model | playerName = name }, Cmd.none )

        GameEnded ->
            ( { model | gameState = Results }, Cmd.batch [ Ports.requestForScores (), Ports.sendEndGameSignal () ] )

        MultiplayerConnectionOpenned ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | multiplayer = True
                            , requestingPlayerName = True
                        }
                  }
                , Cmd.none
                )

        PlayerRegistered (Ok data) ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | playerIsLead = data.lead
                            , playerColour = data.colour
                        }
                  }
                , Cmd.none
                )

        PlayerRegistered (Err error) ->
            ( model, Cmd.none )

        PlaySinglePlayer ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | multiplayer = False
                            , requestingPlayerName = False
                        }
                  }
                , Cmd.none
                )

        ReceiveScores (Ok scores) ->
            ( { model | playerScores = scores }, Cmd.none )

        ReceiveScores (Err error) ->
            ( model, Cmd.none )

        RequestNewName ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model | multiplayerMode = { multiplayerMode | requestingPlayerName = True } }
                , Cmd.none
                )

        Reset ->
            ( { model | gameState = Welcome }, Cmd.none )

        SendName ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | requestingPlayerName = False
                        }
                  }
                , Ports.sendPlayerName model.playerName
                )

        SendScore ->
            ( model, Ports.storeScore ( model.playerName, translateScore model.score ) )

        StartGame ->
            if model.multiplayerMode.playerIsLead || not model.multiplayerMode.multiplayer then
                let
                    ( updatedState, updatedCmd ) =
                        update
                            ApplyTick
                            { model
                                | gameState = Playing
                                , level = Just Level1
                                , score = ( 0, 0, 0 )
                                , tickCount = 0
                                , superBadGuyTick = Nothing
                            }
                in
                    ( updatedState
                    , Cmd.batch
                        [ updatedCmd
                        , Task.perform StartedTime Time.now
                        , Ports.gameStartedByLead ()
                        , Ports.sendGridContents (encodeGridContents updatedState.gridContents)
                        ]
                    )
            else
                ( model, Ports.sendPlayerIsReady () )

        StartMultiplayerGame ->
            if model.multiplayerMode.playerIsLead then
                ( model, Cmd.none )
            else
                let
                    { multiplayerMode } =
                        model
                in
                    ( { model
                        | gameState = Playing
                        , level = Just Level1
                        , multiplayerMode = { multiplayerMode | followingLead = True }
                        , score = ( 0, 0, 0 )
                        , tickCount = 0
                        , superBadGuyTick = Nothing
                      }
                    , Cmd.none
                    )

        StartedTime time ->
            ( { model
                | startedTime = Just time
                , superBadGuyTick = Just (randomTick (floor time))
              }
            , Cmd.none
            )

        Tick time ->
            let
                gameEnded =
                    case ( scoreToLevel model.score, model.startedTime ) of
                        ( Level1, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 20

                        ( Level2, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 40

                        ( Level3, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 60

                        ( Level4, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 80

                        _ ->
                            False
            in
                if gameEnded then
                    update GameEnded model
                else
                    let
                        ( updatedModel, updatedCmd ) =
                            update
                                ApplyTick
                                { model
                                    | lastTick = Just time
                                    , level = Just (scoreToLevel model.score)
                                    , tickCount = model.tickCount + 1
                                }
                    in
                        ( updatedModel
                        , Cmd.batch
                            [ updatedCmd
                            , Ports.sendLevel (levelToInt updatedModel.level)
                            ]
                        )

        UpdateClientNames (Ok clientNames) ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | playerNames = clientNames
                        }
                  }
                , Cmd.none
                )

        UpdateClientNames (Err error) ->
            ( model, Cmd.none )

        UpdateConnections numberOfConnections ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | connectedClients = Just numberOfConnections
                        }
                  }
                , Cmd.none
                )

        UpdateMultiplayerGridContents (Ok gridContents) ->
            let
                convertToContentType contentString =
                    case contentString of
                        "Fraudster" ->
                            Fraudster

                        "SuperFraudster" ->
                            SuperFraudster

                        "Client" ->
                            Client

                        _ ->
                            Empty

                mappedContents =
                    gridContents
                        |> List.map
                            (\( index, contentType ) ->
                                ( index, (convertToContentType contentType) )
                            )
            in
                ( { model | gridContents = (Dict.fromList mappedContents) }, Cmd.none )

        UpdateMultiplayerGridContents (Err error) ->
            ( model, Cmd.none )

        UpdateMultiplayerLevel int ->
            let
                convertedIntToLevel =
                    case int of
                        1 ->
                            Just Level1

                        2 ->
                            Just Level2

                        3 ->
                            Just Level3

                        4 ->
                            Just Level4

                        _ ->
                            Nothing
            in
                ( { model | level = convertedIntToLevel }, Cmd.none )

        UpdateReadyCount readyClients ->
            let
                { multiplayerMode } =
                    model
            in
                ( { model
                    | multiplayerMode =
                        { multiplayerMode
                            | readyClients = readyClients
                        }
                  }
                , Cmd.none
                )


levelToInt : Maybe Level -> Int
levelToInt level =
    case level of
        Just level ->
            case level of
                Level1 ->
                    1

                Level2 ->
                    2

                Level3 ->
                    3

                Level4 ->
                    4

        Nothing ->
            0


createContentList : Int -> Int -> Int -> Bool -> List ContentType
createContentList emptySpaces numberOfFraudsters numberOfClients isSuperBadGuyTick =
    let
        fraudsters =
            List.range 1 numberOfFraudsters
                |> List.map (\_ -> Fraudster)

        clients =
            List.range 1 numberOfClients
                |> List.map (\_ -> Client)

        superbadGuy =
            if isSuperBadGuyTick then
                [ SuperFraudster ]
            else
                []

        empties =
            List.range 1 (emptySpaces - numberOfFraudsters - numberOfClients)
                |> List.map (\_ -> Empty)
    in
        fraudsters
            ++ clients
            ++ superbadGuy
            ++ (if isSuperBadGuyTick then
                    List.take (List.length empties - 1) empties
                else
                    empties
               )


insertContentType : Int -> ContentType -> Dict Int ContentType -> Dict Int ContentType
insertContentType index contentType gridContents =
    Dict.insert index contentType gridContents


calculateLeft : Float -> Float -> Maybe ClientProperties -> String
calculateLeft theirWidth thierX myScreen =
    case myScreen of
        Just properties ->
            (toString (((toFloat properties.screensize.width) / 2) + (thierX - (theirWidth / 2))))

        Nothing ->
            "0"


calculateTop : Float -> Float -> Maybe ClientProperties -> String
calculateTop thierHeight theirY myScreen =
    case myScreen of
        Just properties ->
            (toString (((toFloat properties.screensize.height) / 2) + (theirY - (thierHeight / 2))))

        Nothing ->
            "0"


createMyProperties : List ClientProperties -> String -> Maybe ClientProperties
createMyProperties playerNames playerName =
    playerNames
        |> List.filter (\{ name, colour, screensize, lastLocation } -> name == playerName)
        |> List.head


createCursorsToShow : List ClientProperties -> String -> Maybe ClientProperties -> List (Html Msg)
createCursorsToShow playerNames playerName myProperties =
    playerNames
        |> List.filter (\{ name, colour, screensize, lastLocation } -> name /= playerName)
        |> List.map
            (\{ name, colour, screensize, lastLocation } ->
                div
                    [ class "player-cursor"
                    , style
                        [ ( "background", colour )
                        , ( "left"
                          , ((calculateLeft
                                (toFloat screensize.width)
                                (toFloat lastLocation.x)
                                (myProperties)
                             )
                                ++ "px"
                            )
                          )
                        , ( "top"
                          , ((calculateTop
                                (toFloat screensize.height)
                                (toFloat lastLocation.y)
                                (myProperties)
                             )
                                ++ "px"
                            )
                          )
                        ]
                    ]
                    []
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Welcome ->
            welcomeView model

        Playing ->
            inGameView model

        Results ->
            resultsView model


welcomeView : Model -> Html Msg
welcomeView model =
    let
        multiplayerNameEntry =
            if model.multiplayerMode.requestingPlayerName then
                [ div [ class "multiplayer-overlay" ]
                    [ div [ class "multiplayer-from-container" ]
                        [ input [ placeholder "Player name", onInput ChangeName ] []
                        , button [ class "submit-name", onClick SendName ] [ text "Submit" ]
                        ]
                    , button [ class "play-singleplayer", onClick PlaySinglePlayer ] [ text "Single Player" ]
                    ]
                ]
            else
                []

        connectionsContainer =
            if model.multiplayerMode.multiplayer then
                case model.multiplayerMode.connectedClients of
                    Just numOfClients ->
                        [ div [ class "conections-container" ]
                            ([ span [] [ text (toString numOfClients ++ " Players Connected") ]
                             ]
                                ++ (model.multiplayerMode.playerNames
                                        |> List.map (\{ name, colour } -> div [ style [ ( "color", colour ) ] ] [ text name ])
                                   )
                            )
                        ]

                    Nothing ->
                        []
            else
                []

        startDisabled =
            if model.multiplayerMode.multiplayer then
                case model.multiplayerMode.connectedClients of
                    Just numOfClients ->
                        if numOfClients > 1 then
                            model.multiplayerMode.playerIsLead && ((numOfClients - 1) /= model.multiplayerMode.readyClients)
                        else
                            True

                    Nothing ->
                        True
            else
                False

        myProperties =
            createMyProperties model.multiplayerMode.playerNames model.playerName

        cursorsToShow =
            createCursorsToShow model.multiplayerMode.playerNames model.playerName myProperties
    in
        div [ class "welcome-container" ]
            (multiplayerNameEntry
                ++ connectionsContainer
                ++ [ h1 [] [ text "Welcome to Whack-a-Fraudster" ]
                   , button
                        [ class "start"
                        , onClick StartGame
                        , disabled startDisabled
                        ]
                        [ text "Start" ]
                   ]
                ++ cursorsToShow
            )


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

        myProperties =
            createMyProperties model.multiplayerMode.playerNames model.playerName

        cursorsToShow =
            createCursorsToShow model.multiplayerMode.playerNames model.playerName myProperties
    in
        div [ class "playing-container" ]
            ([ div [ class "score" ] [ span [] [ text "Score: " ], span [] [ text (toString (translateScore model.score)) ] ]
             , button [ class "reset", onClick Reset ] [ div [] [ text "X" ], span [] [ text "Reset" ] ]
             , div ([ class "grid-container" ] ++ levelClass) grid
             , button [ class "end-game", onClick GameEnded ] [ text "End Game" ]
             ]
                ++ cursorsToShow
            )


resultsView : Model -> Html Msg
resultsView model =
    let
        ( fraudstersPercentage, customersPercentage, superbadGuyPercentage ) =
            scoreToPercentage model.score

        ( fraudsters, customers, superbadGuy ) =
            model.score
    in
        div [ class "results-container" ]
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
            , div [ class "scoreing-container" ]
                [ div [] [ text "Submit your score" ]
                , input [ placeholder "Player name", onInput ChangeName ] []
                , button [ class "submit-score", onClick SendScore ] [ text "Submit" ]
                , div [ class "player-scores-label" ] [ text "Player scoreboard" ]
                , div [ class "player-scores" ] (playerScoreDisplay model.playerScores)
                ]
            ]


playerScoreDisplay : List PlayerScore -> List (Html Msg)
playerScoreDisplay playerScores =
    playerScores
        |> List.map
            (\{ name, score } ->
                div [] [ text (name ++ ": " ++ toString score) ]
            )


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


randomList : Int -> Int -> List Int
randomList seed count =
    Tuple.first <| Random.step (Random.list count (Random.int 0 1000)) (Random.initialSeed seed)


randomTick : Int -> Int
randomTick seed =
    Tuple.first <| Random.step (Random.int 1 12) (Random.initialSeed seed)


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
    List.range 0 ((rows * rows) - 1)
        |> List.map
            (\index ->
                let
                    logoItem =
                        if index == floor ((toFloat rows * toFloat rows) / 2) then
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
            Welcome ->
                Sub.batch
                    [ Ports.connectionOpenSignal (always MultiplayerConnectionOpenned)
                    , Ports.registeredAsLeadPlayer (decodeRegistration >> PlayerRegistered)
                    , Ports.clientNames (decodeClientNames >> UpdateClientNames)
                    , Ports.connections UpdateConnections
                    , Ports.updateReadyCount UpdateReadyCount
                    , Ports.startGame (always StartMultiplayerGame)
                    , Ports.invalidName (always RequestNewName)
                    ]

            Playing ->
                if model.multiplayerMode.followingLead then
                    Sub.batch
                        [ Ports.updateGridContents (decodeGridContents >> UpdateMultiplayerGridContents)
                        , Ports.updateClickBox ClickBox
                        , Ports.updateLevel UpdateMultiplayerLevel
                        , Ports.updateEndGame (always GameEnded)
                        , Ports.clientNames (decodeClientNames >> UpdateClientNames)
                        ]
                else
                    Sub.batch
                        [ Time.every interval Tick
                        , Ports.clientNames (decodeClientNames >> UpdateClientNames)
                        , Ports.updateClickBox ClickBox
                        ]

            Results ->
                Sub.batch
                    [ Ports.sendScores (decodePlayerScores >> ReceiveScores)
                    , Ports.clientNames (decodeClientNames >> UpdateClientNames)
                    ]


decodeClientNames : Json.Decode.Value -> Result String (List ClientProperties)
decodeClientNames =
    Json.Decode.decodeValue
        (Json.Decode.list decodeClientProperties)


decodeClientProperties : Json.Decode.Decoder ClientProperties
decodeClientProperties =
    Json.Decode.map4 ClientProperties
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "colour" Json.Decode.string)
        (Json.Decode.field "screensize" decodeScreensize)
        (Json.Decode.field "lastLocation" decodeLastLocation)


decodeScreensize : Json.Decode.Decoder Screensize
decodeScreensize =
    Json.Decode.map2 Screensize
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


decodeLastLocation : Json.Decode.Decoder Location
decodeLastLocation =
    Json.Decode.map2 Location
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


decodeRegistration : Json.Decode.Value -> Result String Registration
decodeRegistration =
    Json.Decode.decodeValue
        (Json.Decode.map2 Registration
            (Json.Decode.field "lead" Json.Decode.bool)
            (Json.Decode.field "colour" Json.Decode.string)
        )


decodePlayerScores : Json.Decode.Value -> Result String (List PlayerScore)
decodePlayerScores =
    Json.Decode.decodeValue
        (Json.Decode.list decodePlayerScore)


decodePlayerScore : Json.Decode.Decoder PlayerScore
decodePlayerScore =
    Json.Decode.map2 PlayerScore
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "score" Json.Decode.int)


decodeGridContents : String -> Result String (List ( Int, String ))
decodeGridContents =
    Json.Decode.decodeString
        (Json.Decode.keyValuePairs Json.Decode.string
            |> Json.Decode.andThen
                (\list ->
                    let
                        result =
                            list
                                |> List.map
                                    (Tuple.mapFirst String.toInt)

                        ( successList, errorList ) =
                            result
                                |> List.foldl
                                    (\( key, value ) ( success, errors ) ->
                                        case key of
                                            Ok key ->
                                                ( [ ( key, value ) ] ++ success, errors )

                                            Err error ->
                                                ( success, [ error ] ++ errors )
                                    )
                                    ( [], [] )
                    in
                        if List.isEmpty errorList then
                            Json.Decode.succeed successList
                        else
                            Json.Decode.fail (String.join ", " errorList)
                )
        )


encodeGridContents : Dict Int ContentType -> String
encodeGridContents gridContents =
    let
        convertContentType contentType =
            case contentType of
                Empty ->
                    "Empty"

                Fraudster ->
                    "Fraudster"

                SuperFraudster ->
                    "SuperFraudster"

                Client ->
                    "Client"
    in
        Dict.toList gridContents
            |> List.map (\( index, contentType ) -> ( (toString index), (Json.Encode.string (convertContentType contentType)) ))
            |> Json.Encode.object
            |> Json.Encode.encode 0
