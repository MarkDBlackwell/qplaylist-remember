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


module MessageDetails exposing (Msg(..))

import Dom exposing (Id)
import Http exposing (Error)
import ModelDetails exposing (LikeOrCommentText)
import ModelDetailsUpdate
    exposing
        ( HttpResponseText
        , SongLatestFewIndex
        , SongRememberedIndex
        )


-- UPDATE


type Msg
    = CommentAreaInputTextChangeCaptureHand LikeOrCommentText
    | CommentAreaOpenHand SongRememberedIndex
    | CommentCancelHand
    | CommentResponse (Result Error HttpResponseText)
    | CommentSendHand
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | HttpResponseTextLog HttpResponseText
    | LikeButtonProcessHand SongRememberedIndex
    | LikeRequest
    | LikeResponse (Result Error HttpResponseText)
    | PageMorphHand
    | SongBuyAnchorProcessHand
    | SongForgetHand SongRememberedIndex
    | SongRememberHand SongLatestFewIndex
    | SongsLatestFewRefreshHand
    | SongsLatestFewResponse (Result Error HttpResponseText)
