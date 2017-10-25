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
        , song2SongRecent
        , song2SongRemembered
        , song2SongTimeless
        , songAlready
        , songGroup2String
        , songRememberedUpdate
        , songs2SongsRemembered
        , songsRememberedAppendOneUniqueFromMaybe
        , songsRememberedLikeOrCommentNewFromMaybe
        , songsRememberedNewFunction
        , songsRememberedUpdateTimestampFromMaybe
        , songsTimelessMatches
        )

import Dom
    exposing
        ( Id
        )
import ModelType
    exposing
        ( Model
        )
import SongInitialize
    exposing
        ( likedOrCommentedInit
        )
import SongType
    exposing
        ( Artist
        , LikedOrCommented
        , SongGroup
            ( Recent
            , Remembered
            )
        , SongRecent
        , SongRecentBase
        , SongRecentMaybe
        , SongRemembered
        , SongRememberedMaybe
        , SongTimeBase
        , SongTimeExceptBase
        , SongTimeless
        , SongTimelessBase
        , SongsRecent
        , SongsRemembered
        , SongsRememberedIndex
        , SongsRememberedIndexMaybe
        , SongsTimeless
        , Time
        , Timestamp
        , Title
        )
import Utilities
    exposing
        ( matchingIndexes
        , maybeMapWithDefault
        , selectOneFromIndexMaybe
        )


-- UPDATE


buttonIdReconstruct : SongsRemembered -> SongRememberedMaybe -> Id -> Id
buttonIdReconstruct songsRemembered songCommentingMaybe idFragment =
    let
        songRememberedIndexMaybe : SongRemembered -> SongsRememberedIndexMaybe
        songRememberedIndexMaybe songCommenting =
            let
                timeless : SongsTimeless
                timeless =
                    songs2SongsTimeless songsRemembered
            in
            song2SongTimeless songCommenting
                |> matchingIndexes timeless
                |> List.head

        create : Int -> Id
        create index =
            String.concat
                [ "button"
                , idFragment
                , toString index
                ]
    in
    Maybe.andThen songRememberedIndexMaybe songCommentingMaybe
        |> Maybe.map create
        |> Maybe.withDefault "refresh"


song2SongRecent : SongRecentBase a -> SongRecent
song2SongRecent { artist, time, timestamp, title } =
    SongRecent artist time timestamp title


song2SongRemembered : SongRecentBase a -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    SongRemembered artist likedOrCommentedInit time timestamp title


song2SongTimeless : SongTimelessBase a -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songAlready : List (SongTimelessBase a) -> SongTimelessBase b -> Bool
songAlready listA songB =
    List.member
        (song2SongTimeless songB)
        (songs2SongsTimeless listA)


songRememberedUpdate : SongTimeExceptBase a -> SongTimeBase b -> SongRemembered
songRememberedUpdate { artist, likedOrCommented, title } { time, timestamp } =
    SongRemembered artist likedOrCommented time timestamp title


songTimelessCompare : SongTimelessBase a -> SongTimelessBase b -> Bool
songTimelessCompare x y =
    (==)
        ( x.artist, x.title )
        ( y.artist, y.title )


songs2SongsRemembered : List (SongRecentBase a) -> SongsRemembered
songs2SongsRemembered songRecentBaseList =
    List.map song2SongRemembered songRecentBaseList


songs2SongsTimeless : List (SongTimelessBase a) -> SongsTimeless
songs2SongsTimeless songTimelessBaseList =
    List.map song2SongTimeless songTimelessBaseList


songsRememberedAppendOneUnique : SongsRemembered -> SongsRecent -> SongRecent -> SongsRemembered
songsRememberedAppendOneUnique songsRemembered songsRecent songRecent =
    if songAlready songsRemembered songRecent then
        songsRemembered
    else
        song2SongRemembered songRecent
            |> List.singleton
            |> (++) songsRemembered


songsRememberedAppendOneUniqueFromMaybe : SongsRemembered -> SongsRecent -> SongRecentMaybe -> SongsRemembered
songsRememberedAppendOneUniqueFromMaybe songsRemembered songsRecent songRecentMaybe =
    maybeMapWithDefault
        songsRemembered
        (songsRememberedAppendOneUnique songsRemembered songsRecent)
        songRecentMaybe


songsRememberedLikeOrCommentNewFromMaybe : SongsRemembered -> SongsRecent -> SongRememberedMaybe -> SongsRemembered
songsRememberedLikeOrCommentNewFromMaybe songsRemembered songsRecent songRememberedMaybe =
    Maybe.map song2SongRecent songRememberedMaybe
        |> songsRememberedUpdateTimestampFromMaybe
            songsRemembered
            songsRecent


songsRememberedNewFunction : Model -> SongsRememberedIndex -> SongsRemembered
songsRememberedNewFunction model songsRememberedIndex =
    let
        matchFirstMaybe : SongRemembered -> SongRecentMaybe
        matchFirstMaybe songRemembered =
            (\x -> song2SongTimeless x == song2SongTimeless songRemembered)
                |> flip List.filter model.songsRecent
                |> List.head

        selectOne : SongRememberedMaybe
        selectOne =
            selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex

        update : SongRemembered -> SongRecent -> SongRemembered
        update songRemembered songRecent =
            { songRemembered
                | time = songRecent.time
                , timestamp = songRecent.timestamp
            }
    in
    Maybe.andThen matchFirstMaybe selectOne
        |> Maybe.map2 update selectOne
        |> songsRememberedLikeOrCommentNewFromMaybe
            model.songsRemembered
            model.songsRecent


songsRememberedUpdateTimestampFromMaybe : SongsRemembered -> SongsRecent -> SongRecentMaybe -> SongsRemembered
songsRememberedUpdateTimestampFromMaybe songsRemembered songsRecent songRecentMaybe =
    let
        updateSometimes : SongRecent -> SongRemembered -> SongRemembered
        updateSometimes songRecent songRemembered =
            if song2SongTimeless (song2SongRecent songRemembered) /= song2SongTimeless songRecent then
                songRemembered
            else
                { songRemembered
                    | time = songRecent.time
                    , timestamp = songRecent.timestamp
                }
    in
    Maybe.map
        (\x -> List.map (updateSometimes x) songsRemembered)
        songRecentMaybe
        |> Maybe.withDefault songsRemembered


songsTimelessMatches : List (SongTimelessBase a) -> SongTimelessBase b -> List (SongTimelessBase a)
songsTimelessMatches listA songB =
    let
        compare : SongTimelessBase a -> Bool
        compare songA =
            songTimelessCompare songB songA
    in
    List.filter compare listA



-- VIEW


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Recent ->
            "latest"

        Remembered ->
            "remembered"
