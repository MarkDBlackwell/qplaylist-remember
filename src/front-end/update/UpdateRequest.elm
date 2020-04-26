{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateRequest exposing
    ( commentSendHand
    , likeButtonProcessHand
    , songsRecentRefreshHand
    )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaitingElmCycle
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import Http
    exposing
        ( getString
        , send
        )
import ModelType
    exposing
        ( Model
        )
import SongHelper
    exposing
        ( songsRememberedNewFromMaybeWithUpdate
        )
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateHelper
    exposing
        ( elmCycleDefault
        , likeOrCommentRequestUriText
        , relative
        , stateVector
        )
import UpdateLog
    exposing
        ( logRequest
        )
import UpdateRequestType
    exposing
        ( UriText
        )
import Utilities
    exposing
        ( selectOneFromIndexMaybe
        )



-- UPDATE


commentSendHand : Model -> ElmCycle
commentSendHand model =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            alertMessageTextServerAwaitingElmCycle model

        _ ->
            if String.isEmpty model.commentText then
                ( { model
                    | alertMessageText = alertMessageTextInit
                  }
                , focusInputPossibly model
                )

            else
                let
                    commentRequest : Cmd Msg
                    commentRequest =
                        getString commentRequestUriText
                            |> send CommentResponse

                    commentRequestUriText : UriText
                    commentRequestUriText =
                        let
                            commentCategory : UriText
                            commentCategory =
                                "c"
                        in
                        likeOrCommentRequestUriText
                            model.userIdentifier
                            model.songCommentingMaybe
                            commentCategory
                            model.commentText
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = True
                  }
                , Cmd.batch
                    [ logRequest commentRequestUriText
                    , commentRequest
                    , focusInputPossibly model
                    ]
                )


likeButtonProcessHand : Model -> SongsRememberedIndex -> ElmCycle
likeButtonProcessHand model songsRememberedIndex =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            alertMessageTextServerAwaitingElmCycle model

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex
                        |> songsRememberedNewFromMaybeWithUpdate model

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    selectOneFromIndexMaybe songsRememberedNew songsRememberedIndex
            in
            case songsRememberedSelectOneMaybe of
                Nothing ->
                    elmCycleDefault model

                _ ->
                    let
                        likeRequest : Cmd Msg
                        likeRequest =
                            getString likeRequestUriText
                                |> send LikeResponse

                        likeRequestUriText : UriText
                        likeRequestUriText =
                            let
                                commentCategory : UriText
                                commentCategory =
                                    "l"

                                commentText : UriText
                                commentText =
                                    "Loved it!"
                            in
                            likeOrCommentRequestUriText
                                model.userIdentifier
                                songsRememberedSelectOneMaybe
                                commentCategory
                                commentText
                    in
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                        , songLikingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ logRequest likeRequestUriText
                        , likeRequest
                        , focusInputPossibly model
                        ]
                    )


songsRecentRefreshHand : Model -> ElmCycle
songsRecentRefreshHand model =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            alertMessageTextServerAwaitingElmCycle model

        _ ->
            let
                requestUriText : UriText
                requestUriText =
                    let
                        basename : UriText
                        basename =
                            "LatestFew.json"
                    in
                    relative
                        [ basename
                        ]
                        []

                songsRecentRequest : Cmd Msg
                songsRecentRequest =
                    getString requestUriText
                        |> send SongsRecentResponse
            in
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = True
              }
            , Cmd.batch
                [ logRequest requestUriText
                , songsRecentRequest
                , focusInputPossibly model
                ]
            )
