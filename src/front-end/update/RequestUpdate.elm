{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module RequestUpdate exposing
    ( commentSendHand
    , likeSendHand
    , songsRecentRefreshHand
    )

import Alert
import ElmCycle
    exposing
        ( Msg(..)
        )
import FocusUpdate
import Http
import LogUpdate
import ModelType
    exposing
        ( Model
        )
import RequestUpdateType
    exposing
        ( QueryPairs
        , UrlText
        )
import SongHelper
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateHelper
import Url.Builder



-- UPDATE


commentSendHand : Model -> ElmCycle.ElmCycle
commentSendHand model =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            if String.isEmpty model.commentText then
                ( { model
                    | alertMessageText = Alert.messageTextInit
                  }
                , FocusUpdate.cmdFocusInputPossibly model
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
                                model.songCommentingOnNowMaybe
                                commentCategory
                in
                ( { model
                    | alertMessageText = Alert.messageTextInit
                    , awaitingServerResponse = True
                  }
                , Cmd.batch
                    [ commentRequestUrlText
                        |> LogUpdate.cmdLogRequest
                    , cmdCommentRequest
                    , FocusUpdate.cmdFocusInputPossibly model
                    ]
                )


likeSendHand : Model -> SongsRememberedIndex -> ElmCycle.ElmCycle
likeSendHand model songsRememberedIndex =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    model.songsRemembered
                        |> List.drop songsRememberedIndex
                        |> List.head
                        |> SongHelper.songsRememberedNewFromMaybeWithUpdate model

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    songsRememberedNew
                        |> List.drop songsRememberedIndex
                        |> List.head
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
                        , songLikingNowMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ likeRequestUrlText
                            |> LogUpdate.cmdLogRequest
                        , cmdLikeRequest
                        , FocusUpdate.cmdFocusInputPossibly model
                        ]
                    )


songsRecentRefreshHand : Model -> ElmCycle.ElmCycle
songsRecentRefreshHand model =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            let
                requestUrlText : UrlText
                requestUrlText =
                    let
                        path : List String
                        path =
                            "LatestFew.json"
                                |> List.singleton

                        queryPairs : QueryPairs
                        queryPairs =
                            []
                    in
                    queryPairs
                        |> Url.Builder.relative path

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
                    |> LogUpdate.cmdLogRequest
                , cmdSongsRecentRequest
                , FocusUpdate.cmdFocusInputPossibly model
                ]
            )
