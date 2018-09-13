{- Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Song
    exposing
        ( likedOrCommentedShow
        )

import SongHelper
    exposing
        ( song2SongRecent
        , song2SongTimeless
        , songRememberedUpdate
        , songsTimelessMatches
        )
import SongType
    exposing
        ( LikeOrCommentCount
        , SongGroupLength
        , SongRecent
        , SongRemembered
        , SongRememberedMaybe
        , SongTimeless
        , SongsRecent
        , SongsRecentIndex
        , SongsRecentOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , SongsRememberedMaybe
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        , startingWithFromIndex
        )


-- UPDATE


likedOrCommentedShow : SongRememberedMaybe -> SongsRemembered -> SongsRemembered
likedOrCommentedShow songLikingOrCommentingMaybe songsRemembered =
    let
        process : SongRemembered -> SongsRemembered
        process songLikingOrCommenting =
            let
                tweakPossibly : SongRemembered -> SongRemembered
                tweakPossibly songRemembered =
                    let
                        likeOrCommentCountNew : LikeOrCommentCount
                        likeOrCommentCountNew =
                            (+)
                                songRemembered.likeOrCommentCount
                                1

                        songDesired : SongRecent
                        songDesired =
                            song2SongRecent songLikingOrCommenting
                    in
                    if song2SongRecent songRemembered /= songDesired then
                        songRemembered
                    else
                        { songRemembered
                            | likeOrCommentCount = likeOrCommentCountNew
                        }
            in
            List.map tweakPossibly songsRemembered
    in
    maybeMapWithDefault songsRemembered process songLikingOrCommentingMaybe


songsRememberedSwapOneRecentFromIndexMaybe : SongsRemembered -> SongsRecent -> SongRemembered -> SongsRememberedIndex -> SongRecent -> SongsRememberedMaybe
songsRememberedSwapOneRecentFromIndexMaybe songsRemembered songsRecent songRemembered songsRememberedIndex songRecent =
    if
        songsTimelessMatches songsRecent songRemembered
            |> List.isEmpty
    then
        Nothing
    else
        (songsRememberedIndex + 1)
            |> startingWithFromIndex songsRemembered
            |> (++)
                (songRememberedUpdate songRemembered songRecent
                    |> List.singleton
                )
            |> (++)
                (List.take songsRememberedIndex songsRemembered)
            |> Just
