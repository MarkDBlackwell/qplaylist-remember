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


module SongInitialize
    exposing
        ( likedOrCommentedInit
        , songCommentingMaybeInit
        , songLikingMaybeInit
        , songLikingOrCommentingInit
        , songsLatestInit
        )

import Dom
    exposing
        ( Id
        )
import SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongCommentingMaybe
        , SongLikingMaybe
        , SongLikingOrCommenting
        , SongRemembered
        , SongsLatest
        , Time
        , Timestamp
        , Title
        )


-- MODEL


songCommentingMaybeInit : SongCommentingMaybe
songCommentingMaybeInit =
    Nothing


songLikingMaybeInit : SongLikingMaybe
songLikingMaybeInit =
    Nothing


songLikingOrCommentingInit : SongLikingOrCommenting
songLikingOrCommentingInit =
    SongRemembered artistInit likedOrCommentedInit timeInit timestampInit titleInit


songsLatestInit : SongsLatest
songsLatestInit =
    []



-- UPDATE


artistInit : Artist
artistInit =
    ""


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


timeInit : Time
timeInit =
    ""


timestampInit : Timestamp
timestampInit =
    ""


titleInit : Title
titleInit =
    ""
