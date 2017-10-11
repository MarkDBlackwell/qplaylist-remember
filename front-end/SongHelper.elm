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
        , song2SongTimeless
        , songGroup2String
        , songs2SongsTimeless
        )

import Dom
    exposing
        ( Id
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
        , SongTimeless
        , SongsRemembered
        , SongsRememberedIndexMaybe
        , SongsTimeless
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


song2SongTimeless : { a | artist : Artist, title : Title } -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songs2SongsTimeless : List { a | artist : Artist, title : Title } -> SongsTimeless
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
