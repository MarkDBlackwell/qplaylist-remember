{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Update exposing (update)

import Alert
import CommentUpdate
import ElmCycle
    exposing
        ( Msg(..)
        )
import FocusUpdate
import KeyboardUpdate
import ModelType
    exposing
        ( Model
        , PageIsExpanded
        )
import RequestUpdate
import RequestUpdateType
    exposing
        ( ActionLikeOrComment(..)
        )
import ResponseUpdate
import SongHelper
    exposing
        ( songsRememberedAppendOneUniqueFromMaybe
        , songsRememberedUpdateTimestampFromMaybe
        )
import SongPort
import SongType
    exposing
        ( SongRecentMaybe
        , SongRememberedMaybe
        , SongTimeless
        , SongsRemembered
        , SongsRememberedIndexList
        , SongsRememberedIndexMaybe
        )
import UpdateHelper
import UserIdentifierUpdate
import Utilities
    exposing
        ( cmdMsg2Cmd
        , selectOneFromIndexMaybe
        , withoutOneFromMaybe
        )



-- UPDATE


update : ElmCycle.Msg -> Model -> ElmCycle.ElmCycle
update msg model =
    case msg of
        MsgCommentAreaInputTextChangeCaptureHand text ->
            text
                |> CommentUpdate.commentAreaInputTextChangeCaptureHand model

        MsgCommentAreaOpenHand songsRememberedIndex ->
            songsRememberedIndex
                |> CommentUpdate.commentAreaOpenHand model

        MsgCommentCancelHand ->
            CommentUpdate.commentCancelHand model

        MsgCommentResponse (Err httpError) ->
            Comment
                |> ResponseUpdate.likeOrCommentResponseErr model httpError

        MsgCommentResponse (Ok httpResponseText) ->
            Comment
                |> ResponseUpdate.likeOrCommentResponseOk model httpResponseText

        MsgCommentSendHand ->
            RequestUpdate.commentSendHand model

        MsgFocusAttempt id ->
            id
                |> FocusUpdate.focusAttempt model

        MsgKeystrokeHand keyChar ->
            keyChar
                |> KeyboardUpdate.keystrokeHand model

        MsgLikeResponse (Err httpError) ->
            Like
                |> ResponseUpdate.likeOrCommentResponseErr model httpError

        MsgLikeResponse (Ok httpResponseText) ->
            Like
                |> ResponseUpdate.likeOrCommentResponseOk model httpResponseText

        MsgLikeSendHand songsRememberedIndex ->
            songsRememberedIndex
                |> RequestUpdate.likeSendHand model

        MsgNone ->
            UpdateHelper.elmCycleDefault model

        MsgPageMorphHand ->
            case UpdateHelper.commentAreaStateVector model of
                --( AwaitingServerResponse, CommentAreaOptional )
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

                _ ->
                    let
                        pageIsExpandedNew : PageIsExpanded
                        pageIsExpandedNew =
                            let
                                bothListsAreEmpty : Bool
                                bothListsAreEmpty =
                                    --Can't combine, in a list, lists of different types:
                                    [ List.isEmpty model.songsRecent
                                    , List.isEmpty model.songsRemembered
                                    ]
                                        |> List.foldl (&&) True
                            in
                            if bothListsAreEmpty then
                                model.pageIsExpanded

                            else
                                model.pageIsExpanded
                                    |> not
                    in
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , FocusUpdate.cmdFocusInputPossibly model
                    )

        MsgSongForgetHand songsRememberedIndex ->
            case UpdateHelper.commentAreaStateVector model of
                --( AwaitingServerResponse, CommentAreaOptional )
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

                _ ->
                    let
                        songsRememberedSelectOneMaybe : SongRememberedMaybe
                        songsRememberedSelectOneMaybe =
                            songsRememberedIndex
                                |> selectOneFromIndexMaybe model.songsRemembered
                    in
                    if model.songCommentingMaybe == songsRememberedSelectOneMaybe then
                        ( { model
                            | alertMessageText = Alert.messageTextInit
                          }
                        , FocusUpdate.cmdFocusInputPossibly model
                        )

                    else
                        let
                            songsRememberedNew : SongsRemembered
                            songsRememberedNew =
                                songsRememberedSelectOneMaybe
                                    |> withoutOneFromMaybe model.songsRemembered
                        in
                        ( { model
                            | alertMessageText = Alert.messageTextInit
                            , songsRemembered = songsRememberedNew
                          }
                        , Cmd.batch
                            [ MsgSongsRememberedStore
                                |> cmdMsg2Cmd
                            , FocusUpdate.cmdFocusSetId "refresh"
                            , FocusUpdate.cmdFocusInputPossibly model
                            ]
                        )

        MsgSongRememberHand songsRecentIndex ->
            case UpdateHelper.commentAreaStateVector model of
                --( AwaitingServerResponse, CommentAreaOptional )
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

                _ ->
                    let
                        songsRememberedNew : SongsRemembered
                        songsRememberedNew =
                            let
                                songsRecentSelectOneMaybe : SongRecentMaybe
                                songsRecentSelectOneMaybe =
                                    songsRecentIndex
                                        |> selectOneFromIndexMaybe model.songsRecent

                                songsRememberedAppended : SongsRemembered
                                songsRememberedAppended =
                                    songsRecentSelectOneMaybe
                                        |> songsRememberedAppendOneUniqueFromMaybe
                                            model.songsRemembered
                                            model.songsRecent
                            in
                            songsRecentSelectOneMaybe
                                |> songsRememberedUpdateTimestampFromMaybe
                                    songsRememberedAppended
                                    model.songsRecent
                    in
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ MsgSongsRememberedStore
                            |> cmdMsg2Cmd
                        , FocusUpdate.cmdFocusInputPossibly model
                        ]
                    )

        MsgSongsRecentRefreshHand ->
            RequestUpdate.songsRecentRefreshHand model

        MsgSongsRecentResponse (Err httpError) ->
            httpError
                |> ResponseUpdate.songsRecentResponseErr model

        MsgSongsRecentResponse (Ok httpResponseText) ->
            httpResponseText
                |> ResponseUpdate.songsRecentResponseOk model

        MsgSongsRememberedStore ->
            SongPort.songsRememberedStore model

        MsgUserIdentifierEstablish randomInt ->
            randomInt
                |> UserIdentifierUpdate.userIdentifierEstablish model
