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


module ModelDetails exposing (..)

-- MODEL


type alias AlertMessage =
    String


type alias Artist =
    String


type alias AwaitingServerResponse =
    Bool


type alias LikeOrCommentText =
    String


type alias LikedOrCommented =
    Bool


type alias Model =
    { alertMessage : AlertMessage
    , awaitingServerResponse : AwaitingServerResponse
    , likeOrCommentText : LikeOrCommentText
    , pageIsExpanded : PageIsExpanded
    , processingComment : ProcessingComment
    , processingLike : ProcessingLike
    , songRememberedCommentingIndex : Maybe SongRememberedCommentingIndex
    , songsLatestFew : SongsLatestFew
    , songsRemembered : SongsRemembered
    }


type alias PageIsExpanded =
    Bool


type alias ProcessingComment =
    Bool


type alias ProcessingLike =
    Bool


type alias SongLatestFew =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongRememberedCommentingIndex =
    Int


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsRemembered =
    List SongRemembered


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String



-- UPDATE
-- SUBSCRIPTIONS
-- VIEW
