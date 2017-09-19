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
        ( Artist
        , LikedOrCommented
        , SongBasic
        , SongCommenting
        , SongLatestFew
        , SongLiking
        , SongLikingOrCommenting
        , SongRemembered
        , SongRememberedIndex
        , SongsBasic
        , SongsLatestFew
        , SongsRemembered
        , Time
        , Timestamp
        , Title
        , likedOrCommentedInit
        , songBasic2SongRemembered
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingNew
        , songRemembered2SongBasic
        , songsLatestFewInit
        , songsRememberedInit
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


type alias SongRememberedIndex =
    Int


type alias SongsBasic =
    List SongBasic


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsRemembered =
    List SongRemembered


type alias Time =
    String


type alias Timestamp =
    String


type alias Title =
    String


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


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


songLikingOrCommentingNew : SongsRemembered -> SongRememberedIndex -> SongLikingOrCommenting
songLikingOrCommentingNew songsRemembered songRememberedIndex =
    let
        songRememberedSelected : Maybe SongRemembered
        songRememberedSelected =
            List.head (List.drop songRememberedIndex songsRemembered)
    in
    case songRememberedSelected of
        Nothing ->
            Nothing

        Just songSelected ->
            Just (songRemembered2SongBasic songSelected)


songRemembered2SongBasic : SongRemembered -> SongBasic
songRemembered2SongBasic song =
    SongBasic
        song.artist
        song.time
        song.timestamp
        song.title


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []
