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
        ( buttonIdReconstruct
        , commentingIndexMaybe
        , likedOrCommentedInit
        , likedOrCommentedShow
        , songCommentingMaybeInit
        , songLikingMaybeInit
        , songLikingOrCommentingInit
        , songLikingOrCommentingMaybe
        , songs2SongsRemembered
        , songsLatestInit
        , songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
        )

import Dom
    exposing
        ( Id
        )
import SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongCommenting
        , SongCommentingMaybe
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongLatest
        , SongLatestMaybe
        , SongLikingMaybe
        , SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , SongRemembered
        , SongRememberedMaybe
        , SongTimeless
        , SongsLatest
        , SongsLatestIndex
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , SongsRememberedIndexMaybe
        , SongsRememberedMaybe
        , SongsTimeless
        , Time
        , Timestamp
        , Title
        )
import Utilities
    exposing
        ( indexes
        , matchingIndexes
        , maybeDefaultNothing
        , maybeMapWithDefault
        , selectOneMaybe
        , startingWith
        , withIndexes
        , withoutOne
        )


-- MODEL


songCommentingMaybeInit : SongCommentingMaybe
songCommentingMaybeInit =
    Nothing


songLikingMaybeInit : SongLikingMaybe
songLikingMaybeInit =
    Nothing


songLikingOrCommentingInit : SongLikingOrCommenting
songLikingOrCommentingInit =
    SongRemembered artistInit likedOrCommentedInit timeInit timestampInit titleInit


songsLatestInit : SongsLatest
songsLatestInit =
    []



-- UPDATE


artistInit : Artist
artistInit =
    ""


buttonIdCreate : Id -> Int -> Id
buttonIdCreate idFragment index =
    String.concat
        [ "button"
        , idFragment
        , toString index
        ]


buttonIdReconstruct : SongsRemembered -> SongCommentingMaybe -> Id -> Id
buttonIdReconstruct songsRemembered songCommentingMaybe idFragment =
    songCommentingMaybe
        |> Maybe.andThen (commentingIndexMaybe songsRemembered)
        |> Maybe.map (buttonIdCreate idFragment)
        |> Maybe.withDefault "refresh"


commentingIndexMaybe : SongsRemembered -> SongCommenting -> SongsRememberedIndexMaybe
commentingIndexMaybe songsRemembered songCommenting =
    let
        songsRememberedTimeless : SongsTimeless
        songsRememberedTimeless =
            songs2SongsTimeless songsRemembered
    in
    song2SongTimeless songCommenting
        |> matchingIndexes songsRememberedTimeless
        |> List.head


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


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


song2SongLatest :
    { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title }
    -> SongLatest
song2SongLatest { artist, time, timestamp, title } =
    SongLatest artist time timestamp title


song2SongRemembered :
    { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title }
    -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    SongRemembered artist likedOrCommentedInit time timestamp title


song2SongTimeless : { a | artist : Artist, title : Title } -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommentingMaybe
songLikingOrCommentingMaybe songsRemembered songsRememberedIndex =
    selectOneMaybe songsRemembered songsRememberedIndex


songs2SongsLatest :
    List { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title }
    -> SongsLatest
songs2SongsLatest listComplex =
    List.map song2SongLatest listComplex


songs2SongsRemembered :
    List { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title }
    -> SongsRemembered
songs2SongsRemembered listComplex =
    List.map song2SongRemembered listComplex


songs2SongsTimeless : List { a | artist : Artist, title : Title } -> SongsTimeless
songs2SongsTimeless listComplex =
    List.map song2SongTimeless listComplex


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
                                    { a | artist : Artist, likedOrCommented : LikedOrCommented, title : Title }
                                    -> { b | time : Time, timestamp : Timestamp }
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


timeInit : Time
timeInit =
    ""


timestampInit : Timestamp
timestampInit =
    ""


titleInit : Title
titleInit =
    ""
