{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ElmCycle exposing
    ( ElmCycle
    , Msg(..)
    )

import Http
import ModelType
    exposing
        ( CommentText
        , Model
        )
import RequestUpdateType
    exposing
        ( HttpResponseText
        )
import SongType
    exposing
        ( SongsRecentIndex
        , SongsRememberedIndex
        )
import UserIdentifierType
    exposing
        ( UserIdentifierNumberSpaceInt
        )
import ViewType
    exposing
        ( Id
        , KeyChar
        )



-- MODEL


type alias ElmCycle =
    ( Model, Cmd Msg )


type Msg
    = MsgCommentAreaInputTextChangeCaptureHand CommentText
    | MsgCommentAreaOpenHand SongsRememberedIndex
    | MsgCommentCancelHand
    | MsgCommentResponse ResultErrorHttp
    | MsgCommentSendHand
    | MsgFocusAttempt Id
    | MsgKeystrokeHand KeyChar
    | MsgLikeButtonProcessHand SongsRememberedIndex
    | MsgLikeResponse ResultErrorHttp
    | MsgNone
    | MsgPageMorphHand
    | MsgSongForgetHand SongsRememberedIndex
    | MsgSongRememberHand SongsRecentIndex
    | MsgSongsRecentRefreshHand
    | MsgSongsRecentResponse ResultErrorHttp
    | MsgSongsRememberedStore
    | MsgUserIdentifierEstablish UserIdentifierNumberSpaceInt


type alias ResultErrorHttp =
    Result Http.Error HttpResponseText
