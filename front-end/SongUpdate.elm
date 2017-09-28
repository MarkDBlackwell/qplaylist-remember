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

import Song
    exposing
        ( SongLatest
        , SongRemembered
        , SongsLatest
        , SongsLatestIndex
        , SongsRemembered
        , SongsRememberedIndex
        , songsLatestSelectOne
        , songsRememberedSelectOne
        , songsRememberedStartingWith
        )


-- UPDATE


songsRememberedUpdateTimestamp : SongsLatest -> SongsRemembered -> SongsRememberedIndex -> SongsRemembered
songsRememberedUpdateTimestamp songsLatest songsRemembered songsRememberedIndex =
    let
        songsLatestIndexFilterMapIndex : SongRemembered -> Maybe SongsLatestIndex
        songsLatestIndexFilterMapIndex songRemembered =
            let
                songsLatestIndexFilterMap : SongRemembered -> List SongsLatestIndex
                songsLatestIndexFilterMap songRemembered =
                    let
                        songsLatestWithIndexes : List ( SongsLatestIndex, SongLatest )
                        songsLatestWithIndexes =
                            let
                                songsLatestIndexes : List SongsLatestIndex
                                songsLatestIndexes =
                                    List.range 0 (List.length songsLatest - 1)
                            in
                            List.map2 (,) songsLatestIndexes songsLatest

                        songsMatch : SongRemembered -> ( SongsLatestIndex, SongLatest ) -> Maybe SongsLatestIndex
                        songsMatch songRemembered ( songLatestIndex, songLatest ) =
                            let
                                songLatestStrip : SongLatest -> SongLatest
                                songLatestStrip songLatest =
                                    SongLatest
                                        songLatest.artist
                                        ""
                                        ""
                                        songLatest.title

                                songRememberedStrip : SongRemembered -> SongLatest
                                songRememberedStrip songRemembered =
                                    SongLatest
                                        songRemembered.artist
                                        ""
                                        ""
                                        songRemembered.title
                            in
                            if songRememberedStrip songRemembered == songLatestStrip songLatest then
                                Just songLatestIndex
                            else
                                Nothing
                    in
                    List.filterMap (songsMatch songRemembered) songsLatestWithIndexes
            in
            List.head (songsLatestIndexFilterMap songRemembered)

        songLatestSelected : Maybe SongsLatestIndex -> Maybe SongLatest
        songLatestSelected songsLatestIndex =
            case songsLatestIndex of
                Nothing ->
                    Nothing

                Just songsLatestIndex ->
                    songsLatestSelectOne songsLatest songsLatestIndex

        songRememberedSelected : Maybe SongRemembered
        songRememberedSelected =
            songsRememberedSelectOne songsRemembered songsRememberedIndex

        songsRememberedSwapOne : SongRemembered -> SongLatest -> SongsRemembered
        songsRememberedSwapOne songRemembered songLatest =
            let
                songRememberedUpdated : SongRemembered -> SongLatest -> SongRemembered
                songRememberedUpdated songRemembered songLatest =
                    SongRemembered
                        songRemembered.artist
                        songRemembered.likedOrCommented
                        songLatest.time
                        songLatest.timestamp
                        songRemembered.title
            in
            case songsLatestIndexFilterMapIndex songRemembered of
                Nothing ->
                    songsRemembered

                Just songsLatestIndexFilterMapIndex ->
                    List.take songsRememberedIndex songsRemembered
                        ++ [ songRememberedUpdated songRemembered songLatest ]
                        ++ songsRememberedStartingWith songsRemembered (songsRememberedIndex + 1)
    in
    case songRememberedSelected of
        Nothing ->
            songsRemembered

        Just songRememberedSelected ->
            case songLatestSelected (songsLatestIndexFilterMapIndex songRememberedSelected) of
                Nothing ->
                    songsRemembered

                Just songLatestSelected ->
                    songsRememberedSwapOne songRememberedSelected songLatestSelected
