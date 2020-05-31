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
        ( Msg(..)
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
import UpdateLog
import UpdateRequestType
    exposing
        ( UrlText
        )
import Utilities
    exposing
        ( selectOneFromIndexMaybe
        )



-- UPDATE


commentSendHand : Model -> ElmCycle.ElmCycle
commentSendHand model =
    case UpdateHelper.stateVector model of
        --(awaitingServer, commentArea)
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            if String.isEmpty model.commentText then
                ( { model
                    | alertMessageText = Alert.messageTextInit
                  }
                , UpdateFocus.cmdFocusInputPossibly model
                )

            else
                let
                    cmdCommentRequest : Cmd ElmCycle.Msg
                    cmdCommentRequest =
                        Http.get
                            { url = commentRequestUrlText
                            , expect =
                                MsgCommentResponse
                                    |> Http.expectString
                            }

                    commentRequestUrlText : UrlText
                    commentRequestUrlText =
                        let
                            commentCategory : UrlText
                            commentCategory =
                                "c"
                        in
                        model.commentText
                            |> UpdateHelper.likeOrCommentRequestUrlText
                                model.userIdentifier
                                model.songCommentingMaybe
                                commentCategory
                in
                ( { model
                    | alertMessageText = Alert.messageTextInit
                    , awaitingServerResponse = True
                  }
                , Cmd.batch
                    [ UpdateLog.cmdLogRequest commentRequestUrlText
                    , cmdCommentRequest
                    , UpdateFocus.cmdFocusInputPossibly model
                    ]
                )


likeButtonProcessHand : Model -> SongsRememberedIndex -> ElmCycle.ElmCycle
likeButtonProcessHand model songsRememberedIndex =
    case UpdateHelper.stateVector model of
        --(awaitingServer, commentArea)
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

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
                    UpdateHelper.elmCycleDefault model

                _ ->
                    let
                        cmdLikeRequest : Cmd ElmCycle.Msg
                        cmdLikeRequest =
                            Http.get
                                { url = likeRequestUrlText
                                , expect =
                                    MsgLikeResponse
                                        |> Http.expectString
                                }

                        likeRequestUrlText : UrlText
                        likeRequestUrlText =
                            let
                                commentCategory : UrlText
                                commentCategory =
                                    "l"

                                commentText : UrlText
                                commentText =
                                    "Loved it!"
                            in
                            commentText
                                |> UpdateHelper.likeOrCommentRequestUrlText
                                    model.userIdentifier
                                    songsRememberedSelectOneMaybe
                                    commentCategory
                    in
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , awaitingServerResponse = True
                        , songLikingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ UpdateLog.cmdLogRequest likeRequestUrlText
                        , cmdLikeRequest
                        , UpdateFocus.cmdFocusInputPossibly model
                        ]
                    )


songsRecentRefreshHand : Model -> ElmCycle.ElmCycle
songsRecentRefreshHand model =
    case UpdateHelper.stateVector model of
        --(awaitingServer, commentArea)
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            let
                requestUrlText : UrlText
                requestUrlText =
                    let
                        basename : UrlText
                        basename =
                            "LatestFew.json"
                    in
                    UpdateHelper.relative
                        [ basename
                        ]
                        []

                cmdSongsRecentRequest : Cmd ElmCycle.Msg
                cmdSongsRecentRequest =
                    Http.get
                        { url = requestUrlText
                        , expect =
                            MsgSongsRecentResponse
                                |> Http.expectString
                        }
            in
            ( { model
                | alertMessageText = Alert.messageTextInit
                , awaitingServerResponse = True
              }
            , Cmd.batch
                [ requestUrlText
                    |> UpdateLog.cmdLogRequest
                , cmdSongsRecentRequest
                , UpdateFocus.cmdFocusInputPossibly model
                ]
            )
