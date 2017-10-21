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
        , songsRememberedAppendOneUniqueFromIndex
        , songsRememberedUpdateTimestampFromIndex
        )

import SongHelper
    exposing
        ( song2SongRecent
        , song2SongRemembered
        , song2SongTimeless
        , songAlready
        , songRememberedUpdate
        , songTimelessCompare
        , songs2SongsRecent
        , songs2SongsRemembered
        , songs2SongsTimeless
        , songsTimelessMatches
        )
import SongType
    exposing
        ( SongGroupLength
        , SongLikingMaybe
        , SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , SongRecent
        , SongRecentMaybe
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
                        songDesired : SongRecent
                        songDesired =
                            song2SongRecent songLikingOrCommenting
                    in
                    if song2SongRecent songRemembered /= songDesired then
                        songRemembered
                    else
                        { songRemembered
                            | likedOrCommented = True
                        }
            in
            List.map tweakPossibly songsRemembered
    in
    maybeMapWithDefault songsRemembered process songLikingOrCommentingMaybe


songsRememberedAppendOneUnique : SongsRemembered -> SongsRecent -> SongRecent -> SongsRemembered
songsRememberedAppendOneUnique songsRemembered songsRecent songRecent =
    if songAlready songsRemembered songRecent then
        songsRemembered
    else
        song2SongRemembered songRecent
            |> List.singleton
            |> (++) songsRemembered


songsRememberedAppendOneUniqueFromIndex : SongsRemembered -> SongsRecent -> SongsRecentIndex -> SongsRemembered
songsRememberedAppendOneUniqueFromIndex songsRemembered songsRecent songsRecentIndex =
    let
        append : SongRecent -> SongsRemembered
        append =
            songsRememberedAppendOneUnique songsRemembered songsRecent
    in
    selectOneMaybe songsRecent songsRecentIndex
        |> maybeMapWithDefault songsRemembered append


songsRememberedSwapOneRecentMaybe : SongsRemembered -> SongsRecent -> SongsRememberedIndex -> SongRemembered -> SongRecent -> SongsRememberedMaybe
songsRememberedSwapOneRecentMaybe songsRemembered songsRecent songsRememberedIndex songRemembered songRecent =
    if
        songsTimelessMatches songsRecent songRemembered
            |> List.isEmpty
    then
        Nothing
    else
        (songsRememberedIndex + 1)
            |> startingWith songsRemembered
            |> (++)
                (songRememberedUpdate songRemembered songRecent
                    |> List.singleton
                )
            |> (++)
                (List.take songsRememberedIndex songsRemembered)
            |> Just


songsRememberedUpdateTimestampFromIndex : SongsRecent -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestampFromIndex songsRecent songsRemembered songsRememberedIndex =
    let
        swapMaybe : SongRemembered -> SongsRememberedMaybe
        swapMaybe =
            swapUnlessListHeadEmptyMaybe
                songsRecent
                songsRemembered
                songsRememberedIndex
    in
    selectOneMaybe songsRemembered songsRememberedIndex
        |> maybeDefaultNothing swapMaybe
        |> Maybe.withDefault songsRemembered


swapUnlessListHeadEmptyMaybe : SongsRecent -> SongsRemembered -> SongsRememberedIndex -> SongRemembered -> SongsRememberedMaybe
swapUnlessListHeadEmptyMaybe songsRecent songsRemembered songsRememberedIndex songRemembered =
    let
        swapMaybe : SongRecent -> SongsRememberedMaybe
        swapMaybe =
            songsRememberedSwapOneRecentMaybe
                songsRemembered
                songsRecent
                songsRememberedIndex
                songRemembered
    in
    songsTimelessMatches songsRecent songRemembered
        |> List.head
        |> maybeDefaultNothing swapMaybe
