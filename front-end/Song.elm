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
        , songsLatest2SongsRemembered
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
            if songLikingOrCommenting /= songRemembered2SongLatest songRemembered then
                songRemembered
            else
                { songRemembered
                    | likedOrCommented = True
                }


songLatest2SongRemembered : SongLatest -> SongRemembered
songLatest2SongRemembered song =
    SongRemembered
        song.artist
        likedOrCommentedInit
        song.time
        song.timestamp
        song.title


songLatest2SongTimeless : SongLatest -> SongTimeless
songLatest2SongTimeless songLatest =
    SongTimeless
        songLatest.artist
        songLatest.title


songLatestRememberedMatchTimeless : SongLatest -> SongRemembered -> Bool
songLatestRememberedMatchTimeless songLatest songRemembered =
    songLatest2SongTimeless songLatest == songRemembered2SongTimeless songRemembered


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommenting
songLikingOrCommentingMaybe songsRemembered songsRememberedIndex =
    case songsRememberedSelectOne songsRemembered songsRememberedIndex of
        Nothing ->
            Nothing

        Just song ->
            Just (songRemembered2SongLatest song)


songRemembered2SongLatest : SongRemembered -> SongLatest
songRemembered2SongLatest song =
    SongLatest
        song.artist
        song.time
        song.timestamp
        song.title


songRemembered2SongTimeless : SongRemembered -> SongTimeless
songRemembered2SongTimeless songRemembered =
    SongTimeless
        songRemembered.artist
        songRemembered.title


songsLatest2SongsRemembered : SongsLatest -> SongsRemembered
songsLatest2SongsRemembered songsLatest =
    List.map songLatest2SongRemembered songsLatest


songsLatestSelectOne : SongsLatest -> SongsLatestIndex -> Maybe SongLatest
songsLatestSelectOne songsLatest songsLatestIndex =
    List.head (songsLatestStartingWith songsLatest songsLatestIndex)


songsLatestStartingWith : SongsLatest -> SongsLatestIndex -> SongsLatest
songsLatestStartingWith songsLatest songsLatestIndex =
    List.drop songsLatestIndex songsLatest


songsRemembered2SongsLatest : SongsRemembered -> SongsLatest
songsRemembered2SongsLatest songsRemembered =
    List.map songRemembered2SongLatest songsRemembered


songsRemembered2SongsTimeless : SongsRemembered -> SongsTimeless
songsRemembered2SongsTimeless songsRemembered =
    List.map songRemembered2SongTimeless songsRemembered


songsRememberedAppendOneUnique : SongsLatest -> SongsLatestIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUnique songsLatest songsLatestIndex songsRemembered =
    case songsLatestSelectOne songsLatest songsLatestIndex of
        Nothing ->
            songsRemembered

        Just song ->
            if
                List.member
                    (songLatest2SongTimeless song)
                    (songsRemembered2SongsTimeless songsRemembered)
            then
                songsRemembered
            else
                songsRemembered
                    ++ [ songLatest2SongRemembered song ]


songsRememberedSelectOne : SongsRemembered -> SongsRememberedIndex -> Maybe SongRemembered
songsRememberedSelectOne songsRemembered songsRememberedIndex =
    List.head (songsRememberedStartingWith songsRemembered songsRememberedIndex)


songsRememberedStartingWith : SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedStartingWith songsRemembered songsRememberedIndex =
    List.drop songsRememberedIndex songsRemembered


songsRememberedUpdateTimestamp : SongsLatest -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsLatest songsRemembered songsRememberedIndex =
    let
        songLatestSelected : Maybe SongsLatestIndex -> Maybe SongLatest
        songLatestSelected songsLatestIndex =
            case songsLatestIndex of
                Nothing ->
                    Nothing

                Just songsLatestIndex ->
                    songsLatestSelectOne songsLatest songsLatestIndex

        songRememberedSelectedMaybe : Maybe SongRemembered
        songRememberedSelectedMaybe =
            songsRememberedSelectOne songsRemembered songsRememberedIndex

        songsLatestIndexFilterMapIndex : SongRemembered -> Maybe SongsLatestIndex
        songsLatestIndexFilterMapIndex songRemembered =
            let
                songsLatestIndexFilterMap : SongRemembered -> List SongsLatestIndex
                songsLatestIndexFilterMap songRemembered =
                    let
                        songsLatestWithIndexes : List ( SongsLatestIndex, SongLatest )
                        songsLatestWithIndexes =
                            let
                                songsLatestIndexes : List SongsLatestIndex
                                songsLatestIndexes =
                                    List.range 0 (List.length songsLatest - 1)
                            in
                            List.map2 (,) songsLatestIndexes songsLatest

                        songsMatchTimelessWithIndex : ( SongsLatestIndex, SongLatest ) -> Maybe SongsLatestIndex
                        songsMatchTimelessWithIndex ( songLatestIndex, songLatest ) =
                            if songLatestRememberedMatchTimeless songLatest songRemembered then
                                Just songLatestIndex
                            else
                                Nothing
                    in
                    List.filterMap songsMatchTimelessWithIndex songsLatestWithIndexes
            in
            List.head (songsLatestIndexFilterMap songRemembered)

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
            case songsLatestIndexFilterMapIndex songRememberedSelected of
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
                    songsLatestIndexFilterMapIndex songRememberedSelected
            in
            case songLatestSelected songsLatestIndexMaybe of
                Nothing ->
                    songsRemembered

                Just songLatestSelected ->
                    songsRememberedSwapOne songRememberedSelected songLatestSelected


songsRememberedWithoutOne : SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedWithoutOne songsRemembered songsRememberedIndex =
    List.take songsRememberedIndex songsRemembered
        ++ songsRememberedStartingWith songsRemembered (songsRememberedIndex + 1)



-- VIEW


type SongGroup
    = Played
    | Remembered


type alias SongGroupLength =
    Int


type alias SongsLatestOrRememberedIndex =
    Int
