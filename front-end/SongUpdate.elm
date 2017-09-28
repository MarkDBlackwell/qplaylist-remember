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


module SongUpdate
    exposing
        ( songsRememberedUpdateTimestamp
        )

import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( SongLatest
        , SongRemembered
        , SongsLatestIndex
        , SongsRemembered
        , SongsRememberedIndex
        , songsLatestSelectOne
        , songsRememberedSelectOne
        , songsRememberedStartingWith
        )


-- UPDATE


songsRememberedUpdateTimestamp : Model -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp model songsRememberedIndex =
    let
        {-
                 songsLatestStripped : SongLatest
                 songsLatestStripped =
                     List.map songLatestStrip model.songsLatest

                 songsMatchIndex : SongRemembered -> SongLatest -> Bool
                 songsMatchIndex songRemembered songLatest =

           List.indexedMap songsMatchIndex songRememberedSelected songsLatestStripped
        -}
        songLatestSelected : Maybe SongsLatestIndex -> Maybe SongLatest
        songLatestSelected songsLatestIndex =
            case songsLatestIndex of
                Nothing ->
                    Nothing

                Just songsLatestIndex ->
                    songsLatestSelectOne model.songsLatest songsLatestIndex

        songLatestStrip : SongLatest -> SongLatest
        songLatestStrip songLatest =
            SongLatest
                songLatest.artist
                ""
                ""
                songLatest.title

        songRememberedSelected : Maybe SongRemembered
        songRememberedSelected =
            songsRememberedSelectOne model.songsRemembered songsRememberedIndex

        songRememberedStrip : SongRemembered -> SongLatest
        songRememberedStrip songRemembered =
            SongLatest
                songRemembered.artist
                ""
                ""
                songRemembered.title

        songRememberedUpdated : SongRemembered -> SongLatest -> SongRemembered
        songRememberedUpdated songRemembered songLatest =
            SongRemembered
                songRemembered.artist
                songRemembered.likedOrCommented
                songLatest.time
                songLatest.timestamp
                songRemembered.title

        songsLatestIndexes : List SongsLatestIndex
        songsLatestIndexes =
            List.range 0 (List.length model.songsLatest - 1)

        songsLatestIndexFilterMap : SongRemembered -> List SongsLatestIndex
        songsLatestIndexFilterMap songRemembered =
            List.filterMap (songsMatch songRemembered) songsLatestWithIndexes

        songsLatestIndexFilterMapIndex : SongRemembered -> Maybe SongsLatestIndex
        songsLatestIndexFilterMapIndex songRemembered =
            List.head (songsLatestIndexFilterMap songRemembered)

        songsLatestWithIndexes : List ( SongsLatestIndex, SongLatest )
        songsLatestWithIndexes =
            List.map2 (,) songsLatestIndexes model.songsLatest

        songsMatch : SongRemembered -> ( SongsLatestIndex, SongLatest ) -> Maybe SongsLatestIndex
        songsMatch songRemembered ( songLatestIndex, songLatest ) =
            if songRememberedStrip songRemembered == songLatestStrip songLatest then
                Just songLatestIndex
            else
                Nothing

        songsRememberedSwapOne : SongRemembered -> SongLatest -> SongsRemembered
        songsRememberedSwapOne songRemembered songLatest =
            case songsLatestIndexFilterMapIndex songRemembered of
                Nothing ->
                    model.songsRemembered

                Just songsLatestIndexFilterMapIndex ->
                    List.take songsRememberedIndex model.songsRemembered
                        ++ [ songRememberedUpdated songRemembered songLatest ]
                        ++ songsRememberedStartingWith model.songsRemembered (songsRememberedIndex + 1)
    in
    case songRememberedSelected of
        Nothing ->
            model.songsRemembered

        Just songRememberedSelected ->
            case songLatestSelected (songsLatestIndexFilterMapIndex songRememberedSelected) of
                Nothing ->
                    model.songsRemembered

                Just songLatestSelected ->
                    songsRememberedSwapOne songRememberedSelected songLatestSelected
