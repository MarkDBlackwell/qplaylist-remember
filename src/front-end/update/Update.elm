{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Update exposing (update)

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
        )
import RequestUpdate
import RequestUpdateType
    exposing
        ( ActionLikeOrComment(..)
        )
import ResponseUpdate
import SongListUpdate
import SongPort
import UpdateHelper
import UserIdentifierUpdate



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
            SongListUpdate.pageMorphHand model

        MsgSongForgetHand songsRememberedIndex ->
            songsRememberedIndex
                |> SongListUpdate.songForgetHand model

        MsgSongRememberHand songsRecentIndex ->
            songsRecentIndex
                |> SongListUpdate.songRememberHand model

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
