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
        , songsRememberedUpdateTimestamp
        )

import SongHelper
    exposing
        ( song2SongRecent
        , song2SongRemembered
        , song2SongTimeless
        , songRememberedUpdate
        , songs2SongsRecent
        , songs2SongsRemembered
        , songs2SongsTimeless
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


songsRememberedAppendOneUniqueFromIndex : SongsRecent -> SongsRecentIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUniqueFromIndex songsRecent songsRecentIndex songsRemembered =
    let
        appendUnlessRemembered : SongRecent -> SongsRemembered
        appendUnlessRemembered songRecent =
            let
                remembered : Bool
                remembered =
                    List.member
                        (song2SongTimeless songRecent)
                        (songs2SongsTimeless songsRemembered)
            in
            if remembered then
                songsRemembered
            else
                song2SongRemembered songRecent
                    |> List.singleton
                    |> (++) songsRemembered
    in
    selectOneMaybe songsRecent songsRecentIndex
        |> maybeMapWithDefault songsRemembered appendUnlessRemembered



{-
   songRemembered2songsRememberedUpdateTimestamp : SongsRecent -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
   songRemembered2songsRememberedUpdateTimestamp songsRecent songsRemembered songsRememberedIndex =

   songRememberedMaybeUpdateMaybe : SongRememberedMaybe -> SongRememberedMaybe
   songRememberedMaybeUpdateMaybe songRememberedMaybe =

   songRememberedUpdateMaybe : SongRemembered -> SongRememberedMaybe
   songRememberedUpdateMaybe songRemembered =

   songsRememberedUpdateSingle : SongsRemembered -> SongRemembered -> SongsRemembered
   songsRememberedUpdateSingle songsRemembered songRemembered =

   songsRememberedSongsRememberedFromUpdateTimestamp : SongsRemembered -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
   songsRememberedSongsRememberedFromUpdateTimestamp songsRememberedFrom songsRemembered songsRememberedIndex =
       let
           songsRecent =
               songsRecent2SongsRemembered songsRememberedFrom
       in
       songsRememberedUpdateTimestamp songsRecent songsRemembered songsRememberedIndex
-}


songsRememberedUpdateTimestamp : SongsRecent -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsRecentSongsRememberedUpdateTimestamp songsRemembered songsRememberedIndex =
    let
        songsRememberedFrom : SongsRemembered
        songsRememberedFrom =
            songs2SongsRemembered songsRecentSongsRememberedUpdateTimestamp

        swapUnlessListHeadEmptyMaybe : SongRemembered -> SongsRememberedMaybe
        swapUnlessListHeadEmptyMaybe songRememberedSwapUnlessListHeadEmpty =
            let
                songsRecentSongRememberedMatches : SongRemembered -> SongsRecent
                songsRecentSongRememberedMatches songRememberedSongsRecentSongRememberedMatches =
                    let
                        compare : SongRecent -> Bool
                        compare songRecentCompare =
                            song2SongTimeless songRememberedSongsRecentSongRememberedMatches
                                |> (==) (song2SongTimeless songRecentCompare)
                    in
                    List.filter compare songsRecentSongsRememberedUpdateTimestamp

                swapOneRecentMaybe : SongRecent -> SongsRememberedMaybe
                swapOneRecentMaybe songRecentSwapOneRecentMaybe =
                    let
                        songsRememberedSwapOneRecentMaybe : SongRemembered -> SongRecent -> SongsRememberedMaybe
                        songsRememberedSwapOneRecentMaybe songRememberedSongsRememberedSwapOneRecent songRecentSongsRememberedSwapOneRecentMaybe =
                            if
                                songsRecentSongRememberedMatches songRememberedSongsRememberedSwapOneRecent
                                    |> List.isEmpty
                            then
                                Nothing
                            else
                                (songsRememberedIndex + 1)
                                    |> startingWith songsRemembered
                                    |> (++)
                                        (songRememberedUpdate songRememberedSongsRememberedSwapOneRecent songRecentSongsRememberedSwapOneRecentMaybe
                                            |> List.singleton
                                        )
                                    |> (++)
                                        (List.take songsRememberedIndex songsRemembered)
                                    |> Just
                    in
                    songsRememberedSwapOneRecentMaybe
                        songRememberedSwapUnlessListHeadEmpty
                        songRecentSwapOneRecentMaybe
            in
            songsRecentSongRememberedMatches songRememberedSwapUnlessListHeadEmpty
                |> List.head
                |> maybeDefaultNothing swapOneRecentMaybe
    in
    selectOneMaybe songsRemembered songsRememberedIndex
        |> maybeDefaultNothing swapUnlessListHeadEmptyMaybe
        |> Maybe.withDefault songsRemembered
