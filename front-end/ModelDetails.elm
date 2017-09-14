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


type alias AlertMessageText =
    String


type alias Artist =
    String


type alias AwaitingServerResponse =
    Bool


type alias CommentAreaOptional =
    Optional


type alias CommentText =
    String


type alias LikedOrCommented =
    Bool


type alias Model =
    { alertMessageText : AlertMessageText
    , awaitingServerResponse : AwaitingServerResponse
    , commentAreaOptional : CommentAreaOptional
    , commentText : CommentText
    , pageIsExpanded : PageIsExpanded
    , processingComment : ProcessingComment
    , processingLike : ProcessingLike
    , songCommenting : SongCommenting
    , songCommentingIndex : SongCommentingIndex
    , songLiking : SongLiking
    , songsLatestFew : SongsLatestFew
    , songsRemembered : SongsRemembered
    }


type Optional
    = Closed
    | Open


type alias PageIsExpanded =
    Bool


type alias ProcessingComment =
    Bool


type alias ProcessingLike =
    Bool


type alias SongBasic =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongCommenting =
    Maybe SongBasic


type alias SongCommentingIndex =
    Maybe Int


type alias SongLatestFew =
    SongBasic


type alias SongLiking =
    Maybe SongBasic


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongsBasic =
    List SongBasic


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


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


songBasic2SongRemembered : SongBasic -> SongRemembered
songBasic2SongRemembered song =
    SongRemembered
        song.artist
        likedOrCommentedInit
        song.time
        song.timeStamp
        song.title


songRemembered2SongBasic : SongRemembered -> SongBasic
songRemembered2SongBasic song =
    SongBasic
        song.artist
        song.time
        song.timeStamp
        song.title
