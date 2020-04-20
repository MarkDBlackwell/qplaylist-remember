{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module SongInitialize exposing
    ( likeOrCommentCountInit
    , songCommentingMaybeInit
    , songLikingMaybeInit
    , songLikingOrCommentingInit
    , songsRecentInit
    )

import SongType
    exposing
        ( Artist
        , LikeOrCommentCount
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
    SongRemembered artistInit likeOrCommentCountInit timeInit timestampInit titleInit


songsRecentInit : SongsRecent
songsRecentInit =
    []



-- UPDATE


artistInit : Artist
artistInit =
    ""


likeOrCommentCountInit : LikeOrCommentCount
likeOrCommentCountInit =
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
