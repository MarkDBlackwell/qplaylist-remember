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


module SongHelper
    exposing
        ( buttonIdReconstruct
        , song2SongLatest
        , song2SongRemembered
        , song2SongTimeless
        , songGroup2String
        , songRememberedUpdate
        , songs2SongsLatest
        , songs2SongsRemembered
        , songs2SongsTimeless
        )

import Dom
    exposing
        ( Id
        )
import SongInitialize
    exposing
        ( likedOrCommentedInit
        )
import SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongCommenting
        , SongCommentingMaybe
        , SongGroup
            ( Latest
            , Remembered
            )
        , SongLatest
        , SongLatestBase
        , SongRemembered
        , SongTimeless
        , SongTimelessBase
        , SongsLatest
        , SongsRemembered
        , SongsRememberedIndexMaybe
        , SongsTimeless
        , Time
        , Timestamp
        , Title
        )
import Utilities
    exposing
        ( matchingIndexes
        )


-- UPDATE


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


song2SongLatest : SongLatestBase a -> SongLatest
song2SongLatest { artist, time, timestamp, title } =
    SongLatest artist time timestamp title


song2SongRemembered : SongLatestBase a -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    SongRemembered artist likedOrCommentedInit time timestamp title


song2SongTimeless : SongTimelessBase a -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songRememberedUpdate :
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
songRememberedUpdate { artist, likedOrCommented, title } { time, timestamp } =
    SongRemembered artist likedOrCommented time timestamp title


songs2SongsLatest : List (SongLatestBase a) -> SongsLatest
songs2SongsLatest songLatestBaseList =
    List.map song2SongLatest songLatestBaseList


songs2SongsRemembered : List (SongLatestBase a) -> SongsRemembered
songs2SongsRemembered songLatestBaseList =
    List.map song2SongRemembered songLatestBaseList


songs2SongsTimeless : List (SongTimelessBase a) -> SongsTimeless
songs2SongsTimeless songTimelessBaseList =
    List.map song2SongTimeless songTimelessBaseList



-- VIEW


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Latest ->
            "latest"

        Remembered ->
            "remembered"
