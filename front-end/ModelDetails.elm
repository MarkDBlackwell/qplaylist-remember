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

import SongsBasic
    exposing
        ( Artist
        , SongBasic
        , SongsBasic
        , Time
        , Timestamp
        , Title
        )


-- MODEL


type alias AlertMessageText =
    String


type alias AwaitingServerResponse =
    Bool


type alias CommentText =
    String


type alias LikedOrCommented =
    Bool


type alias Model =
    { alertMessageText : AlertMessageText
    , awaitingServerResponse : AwaitingServerResponse
    , commentText : CommentText
    , pageIsExpanded : PageIsExpanded
    , songCommenting : SongCommenting
    , songLiking : SongLiking
    , songsLatestFew : SongsLatestFew
    , songsRemembered : SongsRemembered
    , userIdentifier : UserIdentifier
    }


type Optional
    = Closed
    | Open


type alias PageIsExpanded =
    Bool


type alias SongCommenting =
    Maybe SongBasic


type alias SongLatestFew =
    SongBasic


type alias SongLiking =
    Maybe SongBasic


type alias SongLikingOrCommenting =
    Maybe SongBasic


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsRemembered =
    List SongRemembered


type alias UserIdentifier =
    String
