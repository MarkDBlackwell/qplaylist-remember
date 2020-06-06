{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module SongHelper exposing
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

import ModelType
    exposing
        ( Model
        )
import SongInitialize
import SongType
    exposing
        ( Artist
        , SongGroup(..)
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
import ViewType
    exposing
        ( Id
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
                    songsRemembered
                        |> songs2SongsTimeless
            in
            songCommenting
                |> song2SongTimeless
                |> matchingIndexes timeless
                |> List.head

        create : Int -> Id
        create index =
            [ "button"
            , idFragment
            , String.fromInt index
            ]
                |> String.concat
    in
    songCommentingMaybe
        |> Maybe.andThen songRememberedIndexMaybe
        |> Maybe.map create
        |> Maybe.withDefault "refresh"


song2SongRecent : SongRecentBase a -> SongRecent
song2SongRecent { artist, time, timestamp, title } =
    --TODO: Try constructor.
    SongRecent artist time timestamp title


song2SongRemembered : SongRecentBase a -> SongRemembered
song2SongRemembered { artist, time, timestamp, title } =
    --TODO: Try constructor.
    SongRemembered artist SongInitialize.likeOrCommentCountInit time timestamp title


song2SongTimeless : SongTimelessBase a -> SongTimeless
song2SongTimeless { artist, title } =
    --TODO: Try constructor.
    SongTimeless artist title


songAlready : List (SongTimelessBase a) -> SongTimelessBase b -> Bool
songAlready listA songB =
    songs2SongsTimeless listA
        |> List.member
            (song2SongTimeless songB)


songRememberedUpdate : SongTimeExceptBase a -> SongTimeBase b -> SongRemembered
songRememberedUpdate { artist, likeOrCommentCount, title } { time, timestamp } =
    --TODO: Try constructor.
    SongRemembered artist likeOrCommentCount time timestamp title


songTimelessCompare : SongTimelessBase a -> SongTimelessBase b -> Bool
songTimelessCompare x y =
    --TODO: Try constructor.
    (==)
        ( x.artist, x.title )
        ( y.artist, y.title )


songs2SongsRemembered : List (SongRecentBase a) -> SongsRemembered
songs2SongsRemembered songRecentBaseList =
    songRecentBaseList
        |> List.map
            song2SongRemembered


songs2SongsTimeless : List (SongTimelessBase a) -> SongsTimeless
songs2SongsTimeless songTimelessBaseList =
    songTimelessBaseList
        |> List.map
            song2SongTimeless


songsRememberedAppendOneUnique : SongsRemembered -> SongsRecent -> SongRecent -> SongsRemembered
songsRememberedAppendOneUnique songsRemembered songsRecent songRecent =
    if songAlready songsRemembered songRecent then
        songsRemembered

    else
        songRecent
            |> song2SongRemembered
            |> List.singleton
            |> List.append
                songsRemembered


songsRememberedAppendOneUniqueFromMaybe : SongsRemembered -> SongsRecent -> SongRecentMaybe -> SongsRemembered
songsRememberedAppendOneUniqueFromMaybe songsRemembered songsRecent songRecentMaybe =
    songRecentMaybe
        |> maybeMapWithDefault
            songsRemembered
            (songsRecent
                |> songsRememberedAppendOneUnique
                    songsRemembered
            )


songsRememberedLikeOrCommentNewFromMaybe : SongsRemembered -> SongsRecent -> SongRememberedMaybe -> SongsRemembered
songsRememberedLikeOrCommentNewFromMaybe songsRemembered songsRecent songRememberedMaybe =
    songRememberedMaybe
        |> Maybe.map song2SongRecent
        |> songsRememberedUpdateTimestampFromMaybe
            songsRemembered
            songsRecent


songsRememberedNewFromMaybeWithUpdate : Model -> SongRememberedMaybe -> SongsRemembered
songsRememberedNewFromMaybeWithUpdate model songRememberedMaybe =
    model.songsRecent
        |> songsTimelessMatches
        |> (\a -> Maybe.map a songRememberedMaybe)
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
        |> Maybe.withDefault
            songsRemembered


songsTimelessMatches : List (SongTimelessBase a) -> SongTimelessBase b -> List (SongTimelessBase a)
songsTimelessMatches listA songB =
    let
        compare : SongTimelessBase a -> Bool
        compare songA =
            songA
                |> songTimelessCompare
                    songB
    in
    listA
        |> List.filter
            compare



-- VIEW


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Recent ->
            "latest"

        Remembered ->
            "remembered"
