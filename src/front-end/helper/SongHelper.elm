{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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
        , songsRememberedNewFromMaybeWithUpdate
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
        ( likeOrCommentCountInit
        )
import SongType
    exposing
        ( Artist
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
    SongRemembered artist likeOrCommentCountInit time timestamp title


song2SongTimeless : SongTimelessBase a -> SongTimeless
song2SongTimeless { artist, title } =
    SongTimeless artist title


songAlready : List (SongTimelessBase a) -> SongTimelessBase b -> Bool
songAlready listA songB =
    List.member
        (song2SongTimeless songB)
        (songs2SongsTimeless listA)


songRememberedUpdate : SongTimeExceptBase a -> SongTimeBase b -> SongRemembered
songRememberedUpdate { artist, likeOrCommentCount, title } { time, timestamp } =
    SongRemembered artist likeOrCommentCount time timestamp title


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


songsRememberedNewFromMaybeWithUpdate : Model -> SongRememberedMaybe -> SongsRemembered
songsRememberedNewFromMaybeWithUpdate model songRememberedMaybe =
    songsTimelessMatches model.songsRecent
        |> flip Maybe.map songRememberedMaybe
        |> Maybe.andThen List.head
        |> Maybe.map2 songRememberedUpdate songRememberedMaybe
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
