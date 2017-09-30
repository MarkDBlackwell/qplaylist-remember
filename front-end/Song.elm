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
        , SongRemembered
        , SongsLatest
        , SongsLatestIndex
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , SongsRememberedIndex
        , likedOrCommentedShow
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingMaybe
        , songs2SongsRemembered
        , songsLatestInit
        , songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
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


indexes : List a -> List Int
indexes listA =
    List.range 0 (List.length listA - 1)


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


likedOrCommentedShow : SongLikingOrCommenting -> SongsRemembered -> SongsRemembered
likedOrCommentedShow songLikingOrCommenting songsRemembered =
    List.map (likedOrCommentedShowSong songLikingOrCommenting) songsRemembered


likedOrCommentedShowSong : SongLikingOrCommenting -> SongRemembered -> SongRemembered
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


matchingIndexes : List a -> a -> List Int
matchingIndexes listA a =
    let
        matchWithIndex : ( Int, a ) -> Maybe Int
        matchWithIndex ( index, another ) =
            if another == a then
                Just index
            else
                Nothing
    in
    List.filterMap matchWithIndex (withIndexes listA)


selectOne : List a -> Int -> Maybe a
selectOne listA index =
    List.head (startingWith listA index)


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


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommenting
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
        songsLatestIndexFilterMapIndexMaybe : SongsLatest -> SongRemembered -> Maybe SongsLatestIndex
        songsLatestIndexFilterMapIndexMaybe songsLatest songRemembered =
            List.head (matchingIndexes (songs2SongsTimeless songsLatest) (song2SongTimeless songRemembered))

        songsRememberedSwapOneLatest : SongRemembered -> SongLatest -> SongsRemembered
        songsRememberedSwapOneLatest songRememberedSelected songLatestSelected =
            let
                songUpdated :
                    { a | artist : Artist, likedOrCommented : LikedOrCommented, title : Title }
                    -> { b | time : Time, timestamp : Timestamp }
                    -> SongRemembered
                songUpdated { artist, likedOrCommented, title } { time, timestamp } =
                    SongRemembered artist likedOrCommented time timestamp title
            in
            case songsLatestIndexFilterMapIndexMaybe songsLatest songRememberedSelected of
                Nothing ->
                    songsRemembered

                Just _ ->
                    List.take songsRememberedIndex songsRemembered
                        ++ [ songUpdated songRememberedSelected songLatestSelected ]
                        ++ startingWith songsRemembered (songsRememberedIndex + 1)
    in
    case selectOne songsRemembered songsRememberedIndex of
        Nothing ->
            songsRemembered

        Just songRemembered ->
            case songsLatestIndexFilterMapIndexMaybe songsLatest songRemembered of
                Nothing ->
                    songsRemembered

                Just songsLatestIndex ->
                    case selectOne songsLatest songsLatestIndex of
                        Nothing ->
                            songsRemembered

                        Just songLatest ->
                            songsRememberedSwapOneLatest songRemembered songLatest


startingWith : List a -> Int -> List a
startingWith listA index =
    List.drop index listA


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    List.map2 (,) (indexes listA) listA


withoutOne : List a -> Int -> List a
withoutOne listA index =
    List.take index listA
        ++ startingWith listA (index + 1)



-- VIEW


type SongGroup
    = Played
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsLatestOrRememberedIndex =
    Int
