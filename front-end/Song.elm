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
        , songLikingOrCommentingMaybe
        , songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
        )

import SongHelper
    exposing
        ( song2SongLatest
        , song2SongRemembered
        , song2SongTimeless
        , songs2SongsLatest
        , songs2SongsTimeless
        )
import SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongGroupLength
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
        , Time
        , Timestamp
        , Title
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
        songsRememberedTweak : SongLikingOrCommenting -> SongsRemembered
        songsRememberedTweak songLikingOrCommenting =
            let
                songRememberedTweak : SongRemembered -> SongRemembered
                songRememberedTweak songRemembered =
                    if song2SongLatest songLikingOrCommenting /= song2SongLatest songRemembered then
                        songRemembered
                    else
                        { songRemembered
                            | likedOrCommented = True
                        }
            in
            List.map songRememberedTweak songsRemembered
    in
    maybeMapWithDefault songsRemembered songsRememberedTweak songLikingOrCommentingMaybe


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommentingMaybe
songLikingOrCommentingMaybe songsRemembered songsRememberedIndex =
    selectOneMaybe songsRemembered songsRememberedIndex


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
        songsRememberedSelectOneMaybe : SongRememberedMaybe
        songsRememberedSelectOneMaybe =
            selectOneMaybe songsRemembered songsRememberedIndex

        swapUnlessListHeadEmptyMaybe : SongRemembered -> SongsRememberedMaybe
        swapUnlessListHeadEmptyMaybe songRememberedSwapUnlessListHeadEmpty =
            let
                listHeadMaybe : SongLatestMaybe
                listHeadMaybe =
                    songsLatestSongRememberedMatches songRememberedSwapUnlessListHeadEmpty
                        |> List.head

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
                            let
                                songUpdated :
                                    { a
                                        | artist : Artist
                                        , likedOrCommented : LikedOrCommented
                                        , title : Title
                                    }
                                    ->
                                        { b
                                            | time : Time
                                            , timestamp : Timestamp
                                        }
                                    -> SongRemembered
                                songUpdated { artist, likedOrCommented, title } { time, timestamp } =
                                    SongRemembered artist likedOrCommented time timestamp title
                            in
                            if
                                List.isEmpty
                                    (songsLatestSongRememberedMatches
                                        songRememberedSongsRememberedSwapOneLatest
                                    )
                            then
                                Nothing
                            else
                                (songsRememberedIndex + 1)
                                    |> startingWith songsRemembered
                                    |> (++)
                                        (songUpdated songRememberedSongsRememberedSwapOneLatest songLatest
                                            |> List.singleton
                                        )
                                    |> (++) (List.take songsRememberedIndex songsRemembered)
                                    |> Just
                    in
                    songsRememberedSwapOneLatestMaybe
                        songRememberedSwapUnlessListHeadEmpty
                        songLatest
            in
            maybeDefaultNothing swapOneLatestMaybe listHeadMaybe
    in
    Maybe.withDefault
        songsRemembered
        (maybeDefaultNothing swapUnlessListHeadEmptyMaybe songsRememberedSelectOneMaybe)
