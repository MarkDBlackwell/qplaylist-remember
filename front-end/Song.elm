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


module Song
    exposing
        ( likedOrCommentedShow
        , songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
        )

import SongHelper
    exposing
        ( song2SongLatest
        , song2SongRemembered
        , song2SongTimeless
        , songRememberedUpdate
        , songs2SongsLatest
        , songs2SongsTimeless
        )
import SongType
    exposing
        ( SongGroupLength
        , SongLatest
        , SongLatestMaybe
        , SongLikingMaybe
        , SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , SongRemembered
        , SongRememberedMaybe
        , SongsLatest
        , SongsLatestIndex
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , SongsRememberedMaybe
        )
import Utilities
    exposing
        ( indexes
        , maybeDefaultNothing
        , maybeMapWithDefault
        , selectOneMaybe
        , startingWith
        , withIndexes
        , withoutOne
        )


-- UPDATE


likedOrCommentedShow : SongLikingOrCommentingMaybe -> SongsRemembered -> SongsRemembered
likedOrCommentedShow songLikingOrCommentingMaybe songsRemembered =
    let
        process : SongLikingOrCommenting -> SongsRemembered
        process songLikingOrCommenting =
            let
                tweakPossibly : SongRemembered -> SongRemembered
                tweakPossibly songRemembered =
                    let
                        songDesired : SongLatest
                        songDesired =
                            song2SongLatest songLikingOrCommenting
                    in
                    if song2SongLatest songRemembered /= songDesired then
                        songRemembered
                    else
                        { songRemembered
                            | likedOrCommented = True
                        }
            in
            List.map tweakPossibly songsRemembered
    in
    maybeMapWithDefault songsRemembered process songLikingOrCommentingMaybe


songsRememberedAppendOneUnique : SongsLatest -> SongsLatestIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUnique songsLatest songsLatestIndex songsRemembered =
    let
        appendUnlessRemembered : SongLatest -> SongsRemembered
        appendUnlessRemembered songLatest =
            let
                remembered : Bool
                remembered =
                    List.member
                        (song2SongTimeless songLatest)
                        (songs2SongsTimeless songsRemembered)
            in
            if remembered then
                songsRemembered
            else
                song2SongRemembered songLatest
                    |> List.singleton
                    |> (++) songsRemembered
    in
    selectOneMaybe songsLatest songsLatestIndex
        |> maybeMapWithDefault songsRemembered appendUnlessRemembered


songsRememberedUpdateTimestamp : SongsLatest -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsLatest songsRemembered songsRememberedIndex =
    let
        swapUnlessListHeadEmptyMaybe : SongRemembered -> SongsRememberedMaybe
        swapUnlessListHeadEmptyMaybe songRememberedSwapUnlessListHeadEmpty =
            let
                songsLatestSongRememberedMatches : SongRemembered -> SongsLatest
                songsLatestSongRememberedMatches songRememberedSongsLatestSongRememberedMatches =
                    let
                        compare : SongLatest -> Bool
                        compare songLatest =
                            song2SongTimeless songRememberedSongsLatestSongRememberedMatches
                                |> (==) (song2SongTimeless songLatest)
                    in
                    List.filter compare songsLatest

                swapOneLatestMaybe : SongLatest -> SongsRememberedMaybe
                swapOneLatestMaybe songLatest =
                    let
                        songsRememberedSwapOneLatestMaybe : SongRemembered -> SongLatest -> SongsRememberedMaybe
                        songsRememberedSwapOneLatestMaybe songRememberedSongsRememberedSwapOneLatest songLatest =
                            if
                                songsLatestSongRememberedMatches songRememberedSongsRememberedSwapOneLatest
                                    |> List.isEmpty
                            then
                                Nothing
                            else
                                (songsRememberedIndex + 1)
                                    |> startingWith songsRemembered
                                    |> (++)
                                        (songRememberedUpdate songRememberedSongsRememberedSwapOneLatest songLatest
                                            |> List.singleton
                                        )
                                    |> (++) (List.take songsRememberedIndex songsRemembered)
                                    |> Just
                    in
                    songsRememberedSwapOneLatestMaybe
                        songRememberedSwapUnlessListHeadEmpty
                        songLatest
            in
            songsLatestSongRememberedMatches songRememberedSwapUnlessListHeadEmpty
                |> List.head
                |> maybeDefaultNothing swapOneLatestMaybe
    in
    selectOneMaybe songsRemembered songsRememberedIndex
        |> maybeDefaultNothing swapUnlessListHeadEmptyMaybe
        |> Maybe.withDefault songsRemembered
