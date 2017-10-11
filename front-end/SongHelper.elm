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
        , SongCommenting
        , SongCommentingMaybe
        , SongGroup
            ( Played
            , Remembered
            )
        , SongLatest
        , SongRemembered
        , SongTimeless
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


song2SongLatest :
    { a
        | artist : Artist
        , time : Time
        , timestamp : Timestamp
        , title : Title
    }
    -> SongLatest
song2SongLatest { artist, time, timestamp, title } =
    SongLatest artist time timestamp title


song2SongRemembered :
    { a
        | artist : Artist
        , time : Time
        , timestamp : Timestamp
        , title : Title
    }
    -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    SongRemembered artist likedOrCommentedInit time timestamp title


song2SongTimeless :
    { a
        | artist : Artist
        , title : Title
    }
    -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songs2SongsLatest :
    List
        { a
            | artist : Artist
            , time : Time
            , timestamp : Timestamp
            , title : Title
        }
    -> SongsLatest
songs2SongsLatest listComplex =
    List.map song2SongLatest listComplex


songs2SongsRemembered :
    List
        { a
            | artist : Artist
            , time : Time
            , timestamp : Timestamp
            , title : Title
        }
    -> SongsRemembered
songs2SongsRemembered listComplex =
    List.map song2SongRemembered listComplex


songs2SongsTimeless :
    List
        { a
            | artist : Artist
            , title : Title
        }
    -> SongsTimeless
songs2SongsTimeless listComplex =
    List.map song2SongTimeless listComplex



-- VIEW


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"
