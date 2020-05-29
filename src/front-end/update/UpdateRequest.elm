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
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import Http
import ModelType
    exposing
        ( Model
        )
import SongHelper
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateFocus
import UpdateHelper
    exposing
        ( elmCycleDefault
        , likeOrCommentRequestUriText
        , relative
        , stateVector
        )
import UpdateLog
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
            Alert.alertMessageTextServerAwaitingElmCycle model

        _ ->
            if String.isEmpty model.commentText then
                ( { model
                    | alertMessageText = Alert.alertMessageTextInit
                  }
                , UpdateFocus.focusInputPossibly model
                )

            else
                let
                    commentRequest : Cmd Msg
                    commentRequest =
                        Http.get
                            { url = commentRequestUriText
                            , expect =
                                CommentResponse
                                    |> Http.expectString
                            }

                    commentRequestUriText : UriText
                    commentRequestUriText =
                        let
                            commentCategory : UriText
                            commentCategory =
                                "c"
                        in
                        model.commentText
                            |> likeOrCommentRequestUriText
                                model.userIdentifier
                                model.songCommentingMaybe
                                commentCategory
                in
                ( { model
                    | alertMessageText = Alert.alertMessageTextInit
                    , awaitingServerResponse = True
                  }
                , Cmd.batch
                    [ UpdateLog.logRequest commentRequestUriText
                    , commentRequest
                    , UpdateFocus.focusInputPossibly model
                    ]
                )


likeButtonProcessHand : Model -> SongsRememberedIndex -> ElmCycle
likeButtonProcessHand model songsRememberedIndex =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            Alert.alertMessageTextServerAwaitingElmCycle model

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedIndex
                        |> selectOneFromIndexMaybe model.songsRemembered
                        |> SongHelper.songsRememberedNewFromMaybeWithUpdate model

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    songsRememberedIndex
                        |> selectOneFromIndexMaybe songsRememberedNew
            in
            case songsRememberedSelectOneMaybe of
                Nothing ->
                    elmCycleDefault model

                _ ->
                    let
                        likeRequest : Cmd Msg
                        likeRequest =
                            Http.get
                                { url = likeRequestUriText
                                , expect =
                                    LikeResponse
                                        |> Http.expectString
                                }

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
                            commentText
                                |> likeOrCommentRequestUriText
                                    model.userIdentifier
                                    songsRememberedSelectOneMaybe
                                    commentCategory
                    in
                    ( { model
                        | alertMessageText = Alert.alertMessageTextInit
                        , awaitingServerResponse = True
                        , songLikingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ UpdateLog.logRequest likeRequestUriText
                        , likeRequest
                        , UpdateFocus.focusInputPossibly model
                        ]
                    )


songsRecentRefreshHand : Model -> ElmCycle
songsRecentRefreshHand model =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            Alert.alertMessageTextServerAwaitingElmCycle model

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
                    Http.get
                        { url = requestUriText
                        , expect =
                            SongsRecentResponse
                                |> Http.expectString
                        }
            in
            ( { model
                | alertMessageText = Alert.alertMessageTextInit
                , awaitingServerResponse = True
              }
            , Cmd.batch
                [ requestUriText
                    |> UpdateLog.logRequest
                , songsRecentRequest
                , UpdateFocus.focusInputPossibly model
                ]
            )
