module Backend exposing (..)

import Dict exposing (..)
import Lamdera exposing (ClientId, sendToFrontend)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = \_ cid msg mod -> updateFromFrontend cid msg mod
        , subscriptions = \_ -> Sub.none
        }


initModel : Model
initModel =
    { gameClientMap = Dict.empty
    , playerClientMap = Dict.empty
    , gameMap = Dict.empty
    }


init : ( Model, Cmd BackendMsg )
init =
    ( initModel, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBack ->
            ( model, Cmd.none )

        GameFinished game ->
            ( { model
                | gameClientMap = model.gameClientMap |> Dict.remove game.id_
              }
            , GameOver { game | state = Finished }
                |> broadcast model.gameClientMap game.id_
            )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        HostStartsGame gameId playerId ->
            if (model.gameClientMap |> Dict.member gameId) || (model.gameMap |> Dict.member gameId) then
                ( model, sendToFrontend clientId (Error [ GameExists ]) )

            else
                let
                    game =
                        initGame gameId
                in
                ( { model
                    | gameClientMap = model.gameClientMap |> Dict.insert gameId [ clientId ]
                    , gameMap = model.gameMap |> Dict.insert gameId game
                    , playerClientMap = model.playerClientMap |> Dict.insert playerId clientId
                  }
                , sendToFrontend clientId (GameCreated game)
                )

        PlayerJoins gameId playerId ->
            if model.gameClientMap |> Dict.member gameId then
                ( { model
                    | playerClientMap = model.playerClientMap |> Dict.insert playerId clientId
                  }
                , clientId |> say ("Player {" ++ playerId ++ "} joined on client {" ++ clientId ++ "}")
                )

            else
                ( model, sendToFrontend clientId (Error [ GameNotExist ]) )

        DeletePlayer gameId playerId ->
            if model.gameClientMap |> Dict.member gameId then
                if model.playerClientMap |> Dict.member playerId then
                    ( { model | playerClientMap = model.playerClientMap |> Dict.remove playerId }
                    , clientId |> say ("Player {" ++ playerId ++ "} removed")
                    )

                else
                    ( model, sendToFrontend clientId (Error [ PlayerNotExist ]) )

            else
                ( model, sendToFrontend clientId (Error [ GameNotExist ]) )

        DeleteGame game ->
            ( { model
                | gameClientMap = model.gameClientMap |> Dict.remove game.id_
                , gameMap = model.gameMap |> Dict.remove game.id_
                , playerClientMap =
                    model.playerClientMap
                        |> Dict.filter (\k _ -> game.players |> List.member k |> not)
              }
            , GameDeleted |> broadcast model.gameClientMap game.id_
            )


say : String -> ClientId -> Cmd BackendMsg
say message clientId =
    sendToFrontend clientId (Say message)


broadcast : ClientMap -> GameId -> ToFrontend -> Cmd BackendMsg
broadcast clientMap gameId msg =
    case Dict.get gameId clientMap of
        Nothing ->
            Cmd.none

        Just clientList ->
            clientList
                |> List.map (\c -> sendToFrontend c msg)
                |> Cmd.batch


broadcastError : BackendModel -> GameId -> ErrorType -> ( Model, Cmd BackendMsg )
broadcastError model gameId err =
    ( model, Error [ err ] |> broadcast model.gameClientMap gameId )
