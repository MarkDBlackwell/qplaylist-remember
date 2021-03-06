{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module SongInitialize exposing
    ( likeOrCommentCountInit
    , songCommentingOnNowMaybeInit
    , songLikingNowMaybeInit
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


likeOrCommentCountInit : LikeOrCommentCount
likeOrCommentCountInit =
    0



-- MODEL


artistInit : Artist
artistInit =
    ""


songCommentingOnNowMaybeInit : SongRememberedMaybe
songCommentingOnNowMaybeInit =
    Nothing


songLikingNowMaybeInit : SongRememberedMaybe
songLikingNowMaybeInit =
    Nothing


songLikingOrCommentingInit : SongRemembered
songLikingOrCommentingInit =
    SongRemembered artistInit likeOrCommentCountInit timeInit timestampInit titleInit


songsRecentInit : SongsRecent
songsRecentInit =
    []


timeInit : Time
timeInit =
    ""


timestampInit : Timestamp
timestampInit =
    ""


titleInit : Title
titleInit =
    ""
