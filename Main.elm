module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Random


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
    }


initialModel : Model
initialModel =
    { gameState = Welcome
    , gridContents = Dict.empty
    , level = Nothing
    , score = 0
    }


init : ( Model, Cmd Msg )
init =
    update NoOp initialModel



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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickBox index ->
            ( model, Cmd.none )

        CreateClients ->
            ( model, Cmd.none )

        CreateFraudsters ->
            ( model, Cmd.none )

        InitialiseLevel ->
            let
                ( updatedModelWithClients, updatedCmdWithClients ) =
                    update CreateClients model

                ( updatedModelWithFraudsters, updatedCmdWithFraudsters ) =
                    update CreateFraudsters model
            in
            ( updatedModelWithFraudsters, updatedCmdWithFraudsters )

        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            update InitialiseLevel { model | gameState = Playing, level = Just Level1 }



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( levelClass, grid ) =
            case model.level of
                Just Level1 ->
                    ( [ class "grid-container--level1" ], makeGrid 3 )

                Just Level2 ->
                    ( [ class "grid-container--level2" ], makeGrid 5 )

                Just Level3 ->
                    ( [ class "grid-container--level3" ], makeGrid 7 )

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


getRandomNumber : Int -> Int -> Random.Generator Int
getRandomNumber min max =
    Random.int min max


scoreToLevel : Int -> Level
scoreToLevel score =
    if score < 350 then
        Level1
    else if score >= 350 && score < 1000 then
        Level2
    else
        Level3


makeGrid : Int -> List (Html Msg)
makeGrid rows =
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
                in
                div
                    ([ class "grid-item" ]
                        ++ [ class ("grid-item-" ++ toString index) ]
                        ++ logoItem
                        ++ [ onClick (ClickBox index) ]
                    )
                    []
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
