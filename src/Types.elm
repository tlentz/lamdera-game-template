module Types exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)


type alias GameId =
    String


type GameState
    = NoOp
    | Finished


initGame : GameId -> Game
initGame gameId =
    { id_ = gameId
    , state = NoOp
    , players = []
    }


type alias Game =
    { id_ : GameId
    , state : GameState
    , players : List PlayerId
    }


type alias PlayerId =
    String


type ErrorType
    = GameRequired
    | GameExists
    | GameNotExist
    | GameStarted
    | PlayerNotExist
    | NameRequired
    | NameExists
    | Fatal String


errorToString : ErrorType -> String
errorToString errorType =
    case errorType of
        GameRequired ->
            "You must have a non-empty Game ID"

        GameExists ->
            "A game already exists with that ID"

        GameNotExist ->
            "A game does not exist with that ID"

        GameStarted ->
            "That game has already started"

        PlayerNotExist ->
            "A player does not exist with that ID"

        NameRequired ->
            "You must have a non-empty Player Name"

        NameExists ->
            "A player already exists with that name"

        Fatal someError ->
            "FATAL:  " ++ someError


type alias FrontendModel =
    { clientId : ClientId
    , errors : List ErrorType
    , message : String -- TODO: REMOVE

    -- Player
    , gameId : GameId
    , playerId : PlayerId

    -- Game
    , game : Maybe Game
    }


type alias ClientMap =
    Dict GameId (List ClientId)


type alias BackendModel =
    { gameClientMap : ClientMap
    , playerClientMap : Dict PlayerId ClientId
    , gameMap : Dict GameId Game
    }


type FrontendMsg
    = NoOpFront
    | UpdateGameId GameId
    | UpdatePlayerId PlayerId
    | HostGame
    | JoinGame
    | BootPlayer PlayerId
    | HostDeletesGame


type ToBackend
    = NoOpToBackend
    | HostStartsGame GameId PlayerId
    | PlayerJoins GameId PlayerId
    | DeletePlayer GameId PlayerId
    | DeleteGame Game


type BackendMsg
    = NoOpBack
    | GameFinished Game


type ToFrontend
    = NoOpToFrontend
    | Say String -- TODO:  REMOVE
    | Error (List ErrorType)
    | GameCreated Game
    | GameDeleted
    | GameOver Game
