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
        ( SongCommenting
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongLatest
        , SongLiking
        , SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , SongRemembered
        , SongsLatest
        , SongsLatestIndex
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , Time
        , likedOrCommentedShow
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingMaybe
        , songLikingOrCommentingNew
        , songs2SongsRemembered
        , songsLatestInit
        , songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
        )

import Utilities
    exposing
        ( indexes
        , matchingIndexes
        , selectOne
        , startingWith
        , withIndexes
        , withoutOne
        )


-- MODEL


type alias Artist =
    String


type alias LikedOrCommented =
    Bool


type alias SongCommenting =
    Maybe SongLatest


type alias SongLatest =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongLiking =
    Maybe SongLatest


type alias SongLikingOrCommenting =
    SongLatest


songLikingOrCommentingNew : Artist -> Time -> Timestamp -> Title -> SongLikingOrCommenting
songLikingOrCommentingNew artist time timestamp title =
    SongLatest artist time timestamp title


type alias SongLikingOrCommentingMaybe =
    Maybe SongLatest


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongTimeless =
    { artist : Artist
    , title : Title
    }


type alias SongsLatest =
    List SongLatest


type alias SongsLatestIndex =
    Int


type alias SongsRemembered =
    List SongRemembered


type alias SongsRememberedIndex =
    Int


type alias SongsTimeless =
    List SongTimeless


type alias SongsTimelessIndex =
    Int


type alias Time =
    String


type alias Timestamp =
    String


type alias Title =
    String


songCommentingInit : SongCommenting
songCommentingInit =
    Nothing


songLikingInit : SongLiking
songLikingInit =
    Nothing


songsLatestInit : SongsLatest
songsLatestInit =
    []



-- UPDATE


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


likedOrCommentedShow : SongLikingOrCommentingMaybe -> SongsRemembered -> SongsRemembered
likedOrCommentedShow songLikingOrCommenting songsRemembered =
    List.map (likedOrCommentedShowSong songLikingOrCommenting) songsRemembered


likedOrCommentedShowSong : SongLikingOrCommentingMaybe -> SongRemembered -> SongRemembered
likedOrCommentedShowSong songLikingOrCommenting songRemembered =
    case songLikingOrCommenting of
        Nothing ->
            songRemembered

        Just songLikingOrCommenting ->
            if songLikingOrCommenting /= song2SongLatest songRemembered then
                songRemembered
            else
                { songRemembered
                    | likedOrCommented = True
                }


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
    case selectOne songsRemembered songsRememberedIndex of
        Nothing ->
            Nothing

        Just songRemembered ->
            Just (song2SongLatest songRemembered)


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
    case selectOne songsLatest songsLatestIndex of
        Nothing ->
            songsRemembered

        Just songLatest ->
            if
                List.member
                    (song2SongTimeless songLatest)
                    (songs2SongsTimeless songsRemembered)
            then
                songsRemembered
            else
                songsRemembered
                    ++ [ song2SongRemembered songLatest ]


songsRememberedUpdateTimestamp : SongsLatest -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsLatest songsRemembered songsRememberedIndex =
    let
        songsLatestSongRememberedMatches : SongsLatest -> SongRemembered -> SongsLatest
        songsLatestSongRememberedMatches songsLatest songRemembered =
            let
                compare : SongLatest -> Bool
                compare songLatest =
                    song2SongTimeless songRemembered == song2SongTimeless songLatest
            in
            List.filter compare songsLatest

        songsRememberedSwapOneLatest : SongRemembered -> SongLatest -> SongsRemembered
        songsRememberedSwapOneLatest songRemembered songLatest =
            let
                songUpdated :
                    { a | artist : Artist, likedOrCommented : LikedOrCommented, title : Title }
                    -> { b | time : Time, timestamp : Timestamp }
                    -> SongRemembered
                songUpdated { artist, likedOrCommented, title } { time, timestamp } =
                    SongRemembered artist likedOrCommented time timestamp title
            in
            if List.isEmpty (songsLatestSongRememberedMatches songsLatest songRemembered) then
                songsRemembered
            else
                List.take songsRememberedIndex songsRemembered
                    ++ [ songUpdated songRemembered songLatest ]
                    ++ startingWith songsRemembered (songsRememberedIndex + 1)
    in
    case selectOne songsRemembered songsRememberedIndex of
        Nothing ->
            songsRemembered

        Just songRemembered ->
            case List.head (songsLatestSongRememberedMatches songsLatest songRemembered) of
                Nothing ->
                    songsRemembered

                Just songLatest ->
                    songsRememberedSwapOneLatest songRemembered songLatest



-- VIEW


type SongGroup
    = Played
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsLatestOrRememberedIndex =
    Int
