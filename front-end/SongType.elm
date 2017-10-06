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


module SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongCommentingMaybe
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongLatest
        , SongLikingMaybe
        , SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , SongRemembered
        , SongTimeless
        , SongsLatest
        , SongsLatestIndex
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , SongsTimeless
        , Time
        , Timestamp
        , Title
        )

-- MODEL


type alias Artist =
    String


type alias LikedOrCommented =
    Bool


type alias SongCommenting =
    SongLatest


type alias SongCommentingMaybe =
    Maybe SongCommenting


type alias SongLatest =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongLiking =
    SongLatest


type alias SongLikingMaybe =
    Maybe SongLiking


type alias SongLikingOrCommenting =
    SongLatest


type alias SongLikingOrCommentingMaybe =
    Maybe SongLikingOrCommenting


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongTimeless =
    { artist : Artist
    , title : Title
    }


type alias SongsLatest =
    List SongLatest


type alias SongsLatestIndex =
    Int


type alias SongsRemembered =
    List SongRemembered


type alias SongsRememberedIndex =
    Int


type alias SongsTimeless =
    List SongTimeless


type alias SongsTimelessIndex =
    Int


type alias Time =
    String


type alias Timestamp =
    String


type alias Title =
    String



-- VIEW


type SongGroup
    = Played
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsLatestOrRememberedIndex =
    Int
