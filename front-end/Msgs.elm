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


module Msgs exposing (..)

import Dom
    exposing
        ( Id
        )
import Http
    exposing
        ( Error
        )
import Types exposing (..)



-- UPDATE


type alias HttpResponseText =
    String


type alias LikeOrCommentText =
    String


type alias SongLatestFewIndex =
    Int


type alias SongRememberedIndex =
    Int


type Msg
    = BuySongAnchorProcess
    | CommentInputCancel
    | CommentInputOk
    | CommentInputSetUp SongRememberedIndex
    | CommentInputTextChangeCapture LikeOrCommentText
    | CommentResponse (Result Error HttpResponseText)
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | LikeButtonProcess SongRememberedIndex
    | LikeRequest
    | LikeResponse (Result Error HttpResponseText)
    | PageMorph
    | SongForget SongRememberedIndex
    | SongRemember SongLatestFewIndex
    | SongsLatestFewRefresh
    | SongsLatestFewResponse (Result Error HttpResponseText)
