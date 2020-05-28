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
import SongType
    exposing
        ( SongsRecentIndex
        , SongsRememberedIndex
        )
import UpdateRequestType
    exposing
        ( HttpResponseText
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
    = CommentAreaInputTextChangeCaptureHand CommentText
    | CommentAreaOpenHand SongsRememberedIndex
    | CommentCancelHand
    | CommentResponse ResultErrorHttp
    | CommentSendHand
    | FocusAttempt Id
    | KeystrokeHand KeyChar
    | LikeButtonProcessHand SongsRememberedIndex
    | LikeResponse ResultErrorHttp
    | None
    | PageMorphHand
    | SongForgetHand SongsRememberedIndex
    | SongRememberHand SongsRecentIndex
    | SongsRecentRefreshHand
    | SongsRecentResponse ResultErrorHttp
    | SongsRememberedStore
    | UserIdentifierEstablish UserIdentifierNumberSpaceInt


type alias ResultErrorHttp =
    Result Http.Error HttpResponseText
