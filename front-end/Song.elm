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
        , songsRememberedWithoutOne
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


songsLatestInit : SongsLatest
songsLatestInit =
    []


songLikingInit : SongLiking
songLikingInit =
    Nothing



-- UPDATE


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


song2SongRemembered : { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title } -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    SongRemembered artist likedOrCommentedInit time timestamp title


song2SongTimeless : { a | artist : Artist, title : Title } -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommenting
songLikingOrCommentingMaybe songsRemembered songsRememberedIndex =
    case songsRememberedSelectOne songsRemembered songsRememberedIndex of
        Nothing ->
            Nothing

        Just song ->
            Just (song2SongLatest song)


song2SongLatest : { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title } -> SongLatest
song2SongLatest { artist, time, timestamp, title } =
    SongLatest artist time timestamp title


songs2SongsRemembered : List { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title } -> SongsRemembered
songs2SongsRemembered listComplex =
    List.map song2SongRemembered listComplex


songs2SongsTimeless : List { a | artist : Artist, title : Title } -> SongsTimeless
songs2SongsTimeless listComplex =
    List.map song2SongTimeless listComplex


songsLatestIndexes : SongsLatest -> List SongsLatestIndex
songsLatestIndexes songsLatest =
    indexes songsLatest


indexes : List a -> List Int
indexes listA =
    List.range 0 (List.length listA - 1)


songsLatestSelectOne : SongsLatest -> SongsLatestIndex -> Maybe SongLatest
songsLatestSelectOne songsLatest songsLatestIndex =
    selectOne songsLatest songsLatestIndex


selectOne : List a -> Int -> Maybe a
selectOne listA index =
    List.head (startingWith listA index)


songsLatestStartingWith : SongsLatest -> SongsLatestIndex -> SongsLatest
songsLatestStartingWith songsLatest songsLatestIndex =
    startingWith songsLatest songsLatestIndex


startingWith : List a -> Int -> List a
startingWith listA index =
    List.drop index listA


songsRemembered2SongsLatest : SongsRemembered -> SongsLatest
songsRemembered2SongsLatest songsRemembered =
    songs2SongsLatest songsRemembered


songs2SongsLatest : List { a | artist : Artist, time : Time, timestamp : Timestamp, title : Title } -> SongsLatest
songs2SongsLatest listComplex =
    List.map song2SongLatest listComplex


songsRemembered2SongsTimeless : SongsRemembered -> SongsTimeless
songsRemembered2SongsTimeless songsRemembered =
    songs2SongsTimeless songsRemembered


songsRememberedAppendOneUnique : SongsLatest -> SongsLatestIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUnique songsLatest songsLatestIndex songsRemembered =
    case songsLatestSelectOne songsLatest songsLatestIndex of
        Nothing ->
            songsRemembered

        Just song ->
            if
                List.member
                    (song2SongTimeless song)
                    (songsRemembered2SongsTimeless songsRemembered)
            then
                songsRemembered
            else
                songsRemembered
                    ++ [ song2SongRemembered song ]


songsRememberedSelectOne : SongsRemembered -> SongsRememberedIndex -> Maybe SongRemembered
songsRememberedSelectOne songsRemembered songsRememberedIndex =
    selectOne songsRemembered songsRememberedIndex


songsRememberedStartingWith : SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedStartingWith songsRemembered songsRememberedIndex =
    startingWith songsRemembered songsRememberedIndex


songsRememberedUpdateTimestamp : SongsLatest -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsLatest songsRemembered songsRememberedIndex =
    let
        songLatestSelectedMaybe : Maybe SongsLatestIndex -> Maybe SongLatest
        songLatestSelectedMaybe songsLatestIndex =
            case songsLatestIndex of
                Nothing ->
                    Nothing

                Just songsLatestIndex ->
                    songsLatestSelectOne songsLatest songsLatestIndex

        songRememberedSelectedMaybe : Maybe SongRemembered
        songRememberedSelectedMaybe =
            songsRememberedSelectOne songsRemembered songsRememberedIndex

        songsLatestIndexFilterMapIndexMaybe : SongsLatest -> SongRemembered -> Maybe SongsLatestIndex
        songsLatestIndexFilterMapIndexMaybe songsLatest songRemembered =
            List.head (songsTimelessMatchIndexes (songs2SongsTimeless songsLatest) (song2SongTimeless songRemembered))

        songsRememberedSwapOne : SongRemembered -> SongLatest -> SongsRemembered
        songsRememberedSwapOne songRememberedSelected songLatestSelected =
            let
                songUpdated : SongRemembered
                songUpdated =
                    SongRemembered
                        songRememberedSelected.artist
                        songRememberedSelected.likedOrCommented
                        songLatestSelected.time
                        songLatestSelected.timestamp
                        songRememberedSelected.title
            in
            case songsLatestIndexFilterMapIndexMaybe songsLatest songRememberedSelected of
                Nothing ->
                    songsRemembered

                Just songsLatestIndexFilterMapIndex ->
                    List.take songsRememberedIndex songsRemembered
                        ++ [ songUpdated ]
                        ++ songsRememberedStartingWith songsRemembered (songsRememberedIndex + 1)
    in
    case songRememberedSelectedMaybe of
        Nothing ->
            songsRemembered

        Just songRememberedSelected ->
            let
                songsLatestIndexMaybe : Maybe SongsLatestIndex
                songsLatestIndexMaybe =
                    songsLatestIndexFilterMapIndexMaybe songsLatest songRememberedSelected
            in
            case songLatestSelectedMaybe songsLatestIndexMaybe of
                Nothing ->
                    songsRemembered

                Just songLatestSelected ->
                    songsRememberedSwapOne songRememberedSelected songLatestSelected


songsRememberedWithoutOne : SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedWithoutOne songsRemembered songsRememberedIndex =
    withoutOne songsRemembered songsRememberedIndex


withoutOne : List a -> Int -> List a
withoutOne listA index =
    List.take index listA
        ++ startingWith listA (index + 1)


songsTimelessIndexes : SongsTimeless -> List SongsTimelessIndex
songsTimelessIndexes songsTimeless =
    indexes songsTimeless


songsTimelessMatchIndexes : SongsTimeless -> SongTimeless -> List SongsTimelessIndex
songsTimelessMatchIndexes songsTimeless songTimeless =
    matchIndexes songsTimeless songTimeless


matchIndexes : List a -> a -> List Int
matchIndexes listA a =
    let
        matchWithIndex : ( Int, a ) -> Maybe Int
        matchWithIndex ( index, another ) =
            if another == a then
                Just index
            else
                Nothing
    in
    List.filterMap matchWithIndex (withIndexes listA)


songsTimelessWithIndexes : SongsTimeless -> List ( SongsTimelessIndex, SongTimeless )
songsTimelessWithIndexes songsTimeless =
    withIndexes songsTimeless


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    List.map2 (,) (indexes listA) listA



-- VIEW


type SongGroup
    = Played
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsLatestOrRememberedIndex =
    Int
