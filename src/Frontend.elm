module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Types exposing (..)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> NoOpFront
        , onUrlChange = \_ -> NoOpFront
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { clientId = ""
      , errors = []
      , message = ""
      , gameId = ""
      , playerId = ""
      , game = Nothing
      }
    , sendToBackend NoOpToBackend
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFront ->
            ( model, Cmd.none )

        UpdateGameId gameId ->
            ( { model | gameId = gameId }, Cmd.none )

        UpdatePlayerId playerId ->
            ( { model | playerId = playerId }, Cmd.none )

        HostGame ->
            ( model, sendToBackend (HostStartsGame model.gameId model.playerId) )

        JoinGame ->
            ( model, sendToBackend (PlayerJoins model.gameId model.playerId) )

        BootPlayer playerId ->
            ( model, sendToBackend (DeletePlayer model.gameId playerId) )

        HostDeletesGame ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    ( model, sendToBackend (DeleteGame game) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Error errorList ->
            ( { model | errors = errorList }, Cmd.none )

        Say message ->
            -- TODO: REMOVE
            ( { model | message = message }, Cmd.none )

        GameCreated game ->
            ( { model | game = Just game }, Cmd.none )

        GameDeleted ->
            ( { model | game = Nothing, gameId = "" }, Cmd.none )

        GameOver game ->
            ( { model | game = Just game }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    case model.game of
        Nothing ->
            viewLogin model

        Just game ->
            case game.state of
                Finished ->
                    viewGameOver model

                _ ->
                    viewDefault model


viewLogin : Model -> Browser.Document FrontendMsg
viewLogin model =
    { title = ""
    , body =
        [ div [ style "text-align" "center", style "padding-top" "40px" ]
            [ img [ src "https://lamdera.app/lamdera-logo-black.png", width 150 ] []
            , div
                [ style "font-family" "sans-serif"
                , style "padding-top" "40px"
                ]
                [ div
                    []
                    [ p [] [ text "Game ID: " ]
                    , input [ invalidEmptyStyle model.gameId, onInput UpdateGameId ] []
                    , button
                        [ disabled (model.gameId == "" || model.playerId == "")
                        , onClick HostGame
                        ]
                        [ text "Host Game" ]
                    , button
                        [ disabled (model.gameId == "")
                        , onClick HostDeletesGame
                        ]
                        [ text "Delete Game" ]
                    ]
                , div
                    []
                    [ p [] [ text "Player ID: " ]
                    , input [ invalidEmptyStyle model.playerId, onInput UpdatePlayerId ] []
                    , button
                        [ disabled (model.gameId == "" || model.playerId == "")
                        , onClick JoinGame
                        ]
                        [ text "Join Game" ]
                    , button
                        [ disabled (model.playerId == "")
                        , onClick (BootPlayer model.playerId)
                        ]
                        [ text "Boot Player" ]
                    ]
                , h1 [] [ text "Message:" ]
                , text model.message
                , h2 [] [ text "Error:" ]
                , text (model.errors |> List.map errorToString |> String.join "<br />")
                ]
            ]
        ]
    }


viewGameOver : Model -> Browser.Document FrontendMsg
viewGameOver model =
    { title = ""
    , body =
        [ div [] [ text "TODO:  Game Over Screen [Frontend.elm::viewGameOver()]" ]
        ]
    }


viewDefault : Model -> Browser.Document FrontendMsg
viewDefault model =
    { title = ""
    , body =
        [ div [] [ text "TODO:  Default Game Screen [Frontend.elm::viewDefault()]" ]
        ]
    }


invalidEmptyStyle : String -> Attribute msg
invalidEmptyStyle content =
    if content == "" then
        style "border" "1px solid red"

    else
        style "border" "1px solid black"
