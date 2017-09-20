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
        ( SongBasic
        , SongCommenting
        , SongLiking
        , SongLikingOrCommenting
        , SongRemembered
        , SongsBasic
        , SongsLatestFew
        , SongsLatestFewIndex
        , SongsRemembered
        , SongsRememberedIndex
        , likedOrCommentedShow
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingMaybe
        , songsBasic2SongsRemembered
        , songsLatestFewInit
        , songsRememberedAppendOneUnique
        , songsRememberedInit
        , songsRememberedWithoutOne
        )

-- MODEL


type alias Artist =
    String


type alias LikedOrCommented =
    Bool


type alias SongBasic =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongCommenting =
    Maybe SongBasic


type alias SongLatestFew =
    SongBasic


type alias SongLiking =
    Maybe SongBasic


type alias SongLikingOrCommenting =
    Maybe SongBasic


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timestamp : Timestamp
    , title : Title
    }


type alias SongsBasic =
    List SongBasic


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsLatestFewIndex =
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
            if songLikingOrCommenting /= songRemembered2SongBasic songRemembered then
                songRemembered
            else
                { songRemembered
                    | likedOrCommented = True
                }


songBasic2SongRemembered : SongBasic -> SongRemembered
songBasic2SongRemembered song =
    SongRemembered
        song.artist
        likedOrCommentedInit
        song.time
        song.timestamp
        song.title


songCommentingInit : SongCommenting
songCommentingInit =
    Nothing


songLikingInit : SongLiking
songLikingInit =
    Nothing


songLikingOrCommentingMaybe : SongsRemembered -> SongsRememberedIndex -> SongLikingOrCommenting
songLikingOrCommentingMaybe songsRemembered songsRememberedIndex =
    case songsRememberedSelectOne songsRemembered songsRememberedIndex of
        Nothing ->
            Nothing

        Just song ->
            Just (songRemembered2SongBasic song)


songRemembered2SongBasic : SongRemembered -> SongBasic
songRemembered2SongBasic song =
    SongBasic
        song.artist
        song.time
        song.timestamp
        song.title


songsBasic2SongsRemembered : SongsBasic -> SongsRemembered
songsBasic2SongsRemembered songsBasic =
    List.map songBasic2SongRemembered songsBasic


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsLatestFewSelectOne : SongsLatestFew -> SongsLatestFewIndex -> Maybe SongLatestFew
songsLatestFewSelectOne songsLatestFew songsLatestFewIndex =
    List.head (songsLatestFewStartingWith songsLatestFew songsLatestFewIndex)


songsLatestFewStartingWith : SongsLatestFew -> SongsLatestFewIndex -> SongsLatestFew
songsLatestFewStartingWith songsLatestFew songsLatestFewIndex =
    List.drop songsLatestFewIndex songsLatestFew


songsRemembered2SongsBasic : SongsRemembered -> SongsBasic
songsRemembered2SongsBasic songsRemembered =
    List.map songRemembered2SongBasic songsRemembered


songsRememberedAppendOneUnique : SongsLatestFew -> SongsLatestFewIndex -> SongsRemembered -> SongsRemembered
songsRememberedAppendOneUnique songsLatestFew songsLatestFewIndex songsRemembered =
    case songsLatestFewSelectOne songsLatestFew songsLatestFewIndex of
        Nothing ->
            songsRemembered

        Just song ->
            if
                List.member
                    song
                    (songsRemembered2SongsBasic songsRemembered)
            then
                songsRemembered
            else
                songsRemembered
                    ++ [ songBasic2SongRemembered song ]


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []


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
