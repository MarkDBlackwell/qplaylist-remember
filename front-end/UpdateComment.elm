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


module UpdateComment
    exposing
        ( commentAreaInputTextChangeCaptureHand
        , commentAreaOpenHand
        , commentCancelHand
        )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaitingElmCycle
        )
import ElmCycle
    exposing
        ( ElmCycle
        )
import ModelInitialize
    exposing
        ( commentTextInit
        )
import ModelType
    exposing
        ( Model
        , Optional
            ( Open
            )
        )
import Song
    exposing
        ( songsRememberedLikeOrCommentNewFromMaybe
        )
import SongHelper
    exposing
        ( buttonIdReconstruct
        , song2SongTimeless
        )
import SongInitialize
    exposing
        ( songCommentingMaybeInit
        )
import SongType
    exposing
        ( SongRecentMaybe
        , SongRemembered
        , SongRememberedMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        )
import UpdateHelper
    exposing
        ( stateVector
        )
import Utilities
    exposing
        ( selectOneFromIndexMaybe
        )


-- UPDATE


commentAreaInputTextChangeCaptureHand : Model -> String -> ElmCycle
commentAreaInputTextChangeCaptureHand model text =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            ( { model
                | commentText = text
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , commentText = text
              }
            , Cmd.none
            )


commentAreaOpenHand : Model -> SongsRememberedIndex -> ElmCycle
commentAreaOpenHand model songsRememberedIndex =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            alertMessageTextServerAwaitingElmCycle model

        ( _, Open ) ->
            ( { model
                | alertMessageText = alertMessageTextInit
              }
            , focusInputPossibly model
            )

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    let
                        selectOneMaybe =
                            selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex

                        songsRecentMatchFirstMaybe : SongRecentMaybe
                        songsRecentMatchFirstMaybe =
                            let
                                selectOneMaybe =
                                    selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex
                            in
                            case selectOneMaybe of
                                Nothing ->
                                    Nothing

                                Just songRemembered ->
                                    List.filter (\x -> song2SongTimeless songRemembered == song2SongTimeless x) model.songsRecent
                                        |> List.head
                    in
                    case songsRecentMatchFirstMaybe of
                        Nothing ->
                            selectOneMaybe
                                |> songsRememberedLikeOrCommentNewFromMaybe
                                    model.songsRemembered
                                    model.songsRecent

                        Just songRecent ->
                            let
                                update : SongRemembered -> SongRemembered
                                update songRemembered =
                                    { songRemembered
                                        | time = songRecent.time
                                        , timestamp = songRecent.timestamp
                                    }
                            in
                            Maybe.map update selectOneMaybe
                                |> songsRememberedLikeOrCommentNewFromMaybe
                                    model.songsRemembered
                                    model.songsRecent

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    selectOneFromIndexMaybe songsRememberedNew songsRememberedIndex
            in
            case songsRememberedSelectOneMaybe of
                Nothing ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentText = commentTextInit
                        , songCommentingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                      --'focusInputPossibly' doesn't work, here:
                    , focusSetId "input"
                    )


commentCancelHand : Model -> ElmCycle
commentCancelHand model =
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            alertMessageTextServerAwaitingElmCycle model

        _ ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , commentText = commentTextInit
                , songCommentingMaybe = songCommentingMaybeInit
              }
            , buttonIdReconstruct model.songsRemembered model.songCommentingMaybe "Comment"
                |> focusSetId
            )
