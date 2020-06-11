{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Song exposing (likedOrCommentedShow)

import SongHelper
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
        ( startingWithFromIndex
        , succ
        )



-- UPDATE


likedOrCommentedShow : SongRememberedMaybe -> SongsRemembered -> SongsRemembered
likedOrCommentedShow songLikingOrCommentingOnNowMaybe songsRemembered =
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
                                |> SongHelper.song2SongRecent
                    in
                    if SongHelper.song2SongRecent songRemembered /= songDesired then
                        songRemembered

                    else
                        { songRemembered
                            | likeOrCommentCount = likeOrCommentCountNew
                        }
            in
            songsRemembered
                |> List.map tweakPossibly
    in
    songLikingOrCommentingOnNowMaybe
        |> Maybe.map process
        |> Maybe.withDefault
            songsRemembered


songsRememberedSwapOneRecentFromIndexMaybe : SongsRemembered -> SongsRecent -> SongRemembered -> SongsRememberedIndex -> SongRecent -> SongsRememberedMaybe
songsRememberedSwapOneRecentFromIndexMaybe songsRemembered songsRecent songRemembered songsRememberedIndex songRecent =
    if
        songRemembered
            |> SongHelper.songsTimelessMatches songsRecent
            |> List.isEmpty
    then
        Nothing

    else
        songsRememberedIndex
            |> succ
            |> startingWithFromIndex songsRemembered
            |> List.append
                (songRecent
                    |> SongHelper.songRememberedUpdate songRemembered
                    |> List.singleton
                )
            |> List.append
                (songsRemembered
                    |> List.take songsRememberedIndex
                )
            |> Just
