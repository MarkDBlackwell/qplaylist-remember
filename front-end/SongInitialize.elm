{- Copyright (C) 2017 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module SongInitialize
    exposing
        ( likedOrCommentedCountInit
        , songCommentingMaybeInit
        , songLikingMaybeInit
        , songLikingOrCommentingInit
        , songsRecentInit
        )

import SongType
    exposing
        ( Artist
        , LikedOrCommentedCount
        , SongRemembered
        , SongRememberedMaybe
        , SongsRecent
        , Time
        , Timestamp
        , Title
        )


-- MODEL


songCommentingMaybeInit : SongRememberedMaybe
songCommentingMaybeInit =
    Nothing


songLikingMaybeInit : SongRememberedMaybe
songLikingMaybeInit =
    Nothing


songLikingOrCommentingInit : SongRemembered
songLikingOrCommentingInit =
    SongRemembered artistInit likedOrCommentedCountInit timeInit timestampInit titleInit


songsRecentInit : SongsRecent
songsRecentInit =
    []



-- UPDATE


artistInit : Artist
artistInit =
    ""


likedOrCommentedCountInit : LikedOrCommentedCount
likedOrCommentedCountInit =
    0


timeInit : Time
timeInit =
    ""


timestampInit : Timestamp
timestampInit =
    ""


titleInit : Title
titleInit =
    ""
