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
        , songsRememberedAppendOneUniqueFromMaybe
        , songsRememberedLikeOrCommentNewFromMaybe
        , songsRememberedUpdateTimestampFromMaybe
        )

import SongHelper
    exposing
        ( song2SongRecent
        , song2SongRemembered
        , song2SongTimeless
        , songAlready
        , songRememberedUpdate
        , songsTimelessMatches
        )
import SongType
    exposing
        ( SongGroupLength
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
        ( maybeDefaultNothing
        , maybeMapWithDefault
        , selectOneFromIndexMaybe
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


songsRememberedAppendOneUniqueFromMaybe : SongsRemembered -> SongsRecent -> SongRecentMaybe -> SongsRemembered
songsRememberedAppendOneUniqueFromMaybe songsRemembered songsRecent songRecentMaybe =
    maybeMapWithDefault
        songsRemembered
        (songsRememberedAppendOneUnique songsRemembered songsRecent)
        songRecentMaybe


songsRememberedLikeOrCommentNewFromMaybe : SongsRemembered -> SongsRecent -> SongRememberedMaybe -> SongsRemembered
songsRememberedLikeOrCommentNewFromMaybe songsRemembered songsRecent songRememberedMaybe =
    Maybe.map song2SongRecent songRememberedMaybe
        |> songsRememberedUpdateTimestampFromMaybe
            songsRemembered
            songsRecent


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


songsRememberedUpdateTimestampFromMaybe : SongsRemembered -> SongsRecent -> SongRecentMaybe -> SongsRemembered
songsRememberedUpdateTimestampFromMaybe songsRemembered songsRecent songRecentMaybe =
    let
        updateSometimes : SongRecent -> SongRemembered -> SongRemembered
        updateSometimes songRecent songRemembered =
            if song2SongTimeless (song2SongRecent songRemembered) /= song2SongTimeless songRecent then
                songRemembered
            else
                { songRemembered
                    | time = songRecent.time
                    , timestamp = songRecent.timestamp
                }
    in
    Maybe.map
        (\x -> List.map (updateSometimes x) songsRemembered)
        songRecentMaybe
        |> Maybe.withDefault songsRemembered
