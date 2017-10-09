{- Copyright (C) 2017 Mark D. Blackwell.
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module MessageType
    exposing
        ( ElmCycle
        , Msg(..)
        )

import Dom
    exposing
        ( Id
        )
import Http
    exposing
        ( Error
        )
import ModelType
    exposing
        ( CommentText
        , Model
        )
import SongType
    exposing
        ( SongsLatestIndex
        , SongsRememberedIndex
        )
import UpdateRequestType
    exposing
        ( ActionName
        , HttpRequestOrResponseTextMaybe
        , HttpResponseText
        )


-- MODEL


type alias ElmCycle =
    ( Model, Cmd Msg )


type Msg
    = CommentAreaInputTextChangeCaptureHand CommentText
    | CommentAreaOpenHand SongsRememberedIndex
    | CommentCancelHand
    | CommentResponse (Result Error HttpResponseText)
    | CommentSendHand
    | FocusResult (Result Dom.Error ())
    | FocusAttempt Id
    | HttpRequestOrResponseTextLog ActionName HttpRequestOrResponseTextMaybe
    | InitialSetUp Int
    | LikeButtonProcessHand SongsRememberedIndex
    | LikeResponse (Result Error HttpResponseText)
    | PageMorphHand
    | SongBuyAnchorProcessHand
    | SongForgetHand SongsRememberedIndex
    | SongRememberHand SongsLatestIndex
    | SongsLatestRefreshHand
    | SongsLatestResponse (Result Error HttpResponseText)
    | SongsRememberedStore
