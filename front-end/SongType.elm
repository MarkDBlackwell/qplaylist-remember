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
        , SongGroup
            ( Recent
            , Remembered
            )
        , SongGroupLength
        , SongRecent
        , SongRecentBase
        , SongRecentMaybe
        , SongRecentOrRemembered
        , SongRemembered
        , SongRememberedMaybe
        , SongTimeBase
        , SongTimeExceptBase
        , SongTimeless
        , SongTimelessBase
        , SongsRecent
        , SongsRecentIndex
        , SongsRecentOrRemembered
        , SongsRecentOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , SongsRememberedIndexList
        , SongsRememberedIndexMaybe
        , SongsRememberedMaybe
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


type alias SongRecent =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongRecentBase a =
    { a
        | artist : Artist
        , time : Time
        , timestamp : Timestamp
        , title : Title
    }


type alias SongRecentMaybe =
    Maybe SongRecent


type alias SongRecentOrRemembered =
    SongRemembered


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongRememberedMaybe =
    Maybe SongRemembered


type alias SongTimeBase a =
    { a
        | time : Time
        , timestamp : Timestamp
    }


type alias SongTimeExceptBase a =
    { a
        | artist : Artist
        , likedOrCommented : LikedOrCommented
        , title : Title
    }


type alias SongTimeless =
    { artist : Artist
    , title : Title
    }


type alias SongTimelessBase a =
    { a
        | artist : Artist
        , title : Title
    }


type alias SongsRecent =
    List SongRecent


type alias SongsRecentIndex =
    Int


type alias SongsRecentOrRemembered =
    SongsRemembered


type alias SongsRemembered =
    List SongRemembered


type alias SongsRememberedIndex =
    Int


type alias SongsRememberedIndexList =
    List SongsRememberedIndex


type alias SongsRememberedIndexMaybe =
    Maybe SongsRememberedIndex


type alias SongsRememberedMaybe =
    Maybe SongsRemembered


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
    = Recent
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsRecentOrRememberedIndex =
    Int
