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
        , songsRememberedInit
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


type alias SongsLatest =
    List SongLatest


type alias SongsLatestIndex =
    Int


type alias SongsRemembered =
    List SongRemembered


type alias SongsRememberedIndex =
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


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []



-- UPDATE


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


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


songLatest2SongRemembered : SongLatest -> SongRemembered
songLatest2SongRemembered song =
    SongRemembered
        song.artist
        likedOrCommentedInit
        song.time
        song.timestamp
        song.title


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


songsRememberedAppendOneUnique : SongsLatest -> SongsLatestIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUnique songsLatest songsLatestIndex songsRemembered =
    case songsLatestSelectOne songsLatest songsLatestIndex of
        Nothing ->
            songsRemembered

        Just song ->
            if
                List.member
                    song
                    (songsRemembered2SongsLatest songsRemembered)
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
