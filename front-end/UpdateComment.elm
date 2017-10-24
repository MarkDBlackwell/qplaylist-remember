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
        ( songsRememberedUpdateTimestampFromMaybe
        )
import SongHelper
    exposing
        ( buttonIdReconstruct
        , song2SongRecent
        )
import SongInitialize
    exposing
        ( songCommentingMaybeInit
        )
import SongType
    exposing
        ( SongsRemembered
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
                    songsRememberedLikeOrCommentNew

                songsRememberedLikeOrCommentNew : SongsRemembered
                songsRememberedLikeOrCommentNew =
                    selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex
                        |> Maybe.map song2SongRecent
                        |> songsRememberedUpdateTimestampFromMaybe
                            model.songsRemembered
                            model.songsRecent
            in
            case selectOneFromIndexMaybe songsRememberedNew songsRememberedIndex of
                Nothing ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentText = commentTextInit
                        , songCommentingMaybe =
                            selectOneFromIndexMaybe
                                songsRememberedNew
                                songsRememberedIndex
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
