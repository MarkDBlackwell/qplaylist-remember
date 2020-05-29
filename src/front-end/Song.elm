{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Song exposing (likedOrCommentedShow)

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
        , succ
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
                            songRemembered.likeOrCommentCount
                                |> succ

                        songDesired : SongRecent
                        songDesired =
                            songLikingOrCommenting
                                |> song2SongRecent
                    in
                    if song2SongRecent songRemembered /= songDesired then
                        songRemembered

                    else
                        { songRemembered
                            | likeOrCommentCount = likeOrCommentCountNew
                        }
            in
            songsRemembered
                |> List.map tweakPossibly
    in
    songLikingOrCommentingMaybe
        |> maybeMapWithDefault songsRemembered process


songsRememberedSwapOneRecentFromIndexMaybe : SongsRemembered -> SongsRecent -> SongRemembered -> SongsRememberedIndex -> SongRecent -> SongsRememberedMaybe
songsRememberedSwapOneRecentFromIndexMaybe songsRemembered songsRecent songRemembered songsRememberedIndex songRecent =
    if
        songRemembered
            |> songsTimelessMatches songsRecent
            |> List.isEmpty
    then
        Nothing

    else
        songsRememberedIndex
            |> succ
            |> startingWithFromIndex songsRemembered
            |> (++)
                (songRecent
                    |> songRememberedUpdate songRemembered
                    |> List.singleton
                )
            |> (++)
                (songsRemembered
                    |> List.take songsRememberedIndex
                )
            |> Just
