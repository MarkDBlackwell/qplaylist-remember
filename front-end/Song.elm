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


songsRememberedAppendOneUniqueFromIndex : SongsRemembered -> SongsRecent -> SongsRecentIndex -> SongsRemembered
songsRememberedAppendOneUniqueFromIndex songsRemembered songsRecent songsRecentIndex =
    selectOneMaybe songsRecent songsRecentIndex
        |> maybeMapWithDefault songsRemembered (songsRememberedAppendOneUnique songsRemembered songsRecent)



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
       songsRememberedUpdateTimestampFromIndex songsRecent songsRemembered songsRememberedIndex


    let
        songsRememberedFrom : SongsRemembered
        songsRememberedFrom =
            songs2SongsRemembered songsRecentSongsRememberedUpdateTimestamp

    in
-}


compareSongRememberedToSongRecent : SongRemembered -> SongRecent -> Bool
compareSongRememberedToSongRecent songRemembered songRecent =
    song2SongTimeless songRemembered
        |> (==) (song2SongTimeless songRecent)


compareSongTimelessToSongRecent : SongTimeless -> SongRecent -> Bool
compareSongTimelessToSongRecent songTimeless songRecent =
    songTimeless
        |> (==) (song2SongTimeless songRecent)


songsRecentSongRememberedMatches : SongsRecent -> SongRemembered -> SongsRecent
songsRecentSongRememberedMatches songsRecent songRemembered =
    List.filter (compareSongRememberedToSongRecent songRemembered) songsRecent


songsRecentSongTimelessMatches : SongsRecent -> SongTimeless -> SongsRecent
songsRecentSongTimelessMatches songsRecent songTimeless =
    List.filter (compareSongTimelessToSongRecent songTimeless) songsRecent


swapUnlessListHeadEmptyMaybe :
    SongsRememberedIndex
    -> SongsRecent
    -> SongsRemembered
    -> SongRemembered
    -> SongsRememberedMaybe
swapUnlessListHeadEmptyMaybe songsRememberedIndex songsRecentSongsRememberedUpdateTimestamp songsRemembered songRememberedSwapUnlessListHeadEmpty =
    let
        swapOneRecentMaybe : SongRecent -> SongsRememberedMaybe
        swapOneRecentMaybe songRecentSwapOneRecentMaybe =
            let
                songsRememberedSwapOneRecentMaybe : SongRemembered -> SongRecent -> SongsRememberedMaybe
                songsRememberedSwapOneRecentMaybe songRememberedSongsRememberedSwapOneRecent songRecentSongsRememberedSwapOneRecentMaybe =
                    if
                        songsRecentSongRememberedMatches
                            songsRecentSongsRememberedUpdateTimestamp
                            songRememberedSongsRememberedSwapOneRecent
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
    songsRecentSongRememberedMatches
        songsRecentSongsRememberedUpdateTimestamp
        songRememberedSwapUnlessListHeadEmpty
        |> List.head
        |> maybeDefaultNothing swapOneRecentMaybe


songsRememberedUpdateTimestampFromIndex : SongsRecent -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestampFromIndex songsRecentSongsRememberedUpdateTimestamp songsRemembered songsRememberedIndex =
    selectOneMaybe songsRemembered songsRememberedIndex
        |> maybeDefaultNothing (swapUnlessListHeadEmptyMaybe songsRememberedIndex songsRecentSongsRememberedUpdateTimestamp songsRemembered)
        |> Maybe.withDefault songsRemembered
