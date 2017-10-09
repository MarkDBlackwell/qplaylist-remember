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


module UpdateCommentArea
    exposing
        ( commentAreaInputTextChangeCaptureHand
        , commentAreaOpenHand
        , commentCancelHand
        )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaiting
        )
import Initialize
    exposing
        ( commentTextInit
        )
import MessageType
    exposing
        ( ElmCycle
        )
import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( songCommentingMaybeInit
        , songLikingOrCommentingMaybe
        , songsRememberedUpdateTimestamp
        )
import SongType
    exposing
        ( SongLikingOrCommentingMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        )
import UpdateStateVector
    exposing
        ( stateVector
        )
import UpdateType
    exposing
        ( Optional
            ( Open
            )
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
    let
        songCommentingMaybeNew : SongLikingOrCommentingMaybe
        songCommentingMaybeNew =
            songLikingOrCommentingMaybe songsRememberedNew songsRememberedIndex

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            songsRememberedUpdateTimestamp model.songsLatest model.songsRemembered songsRememberedIndex
    in
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            ( { model
                | alertMessageText = alertMessageTextServerAwaiting
              }
            , focusInputPossibly model
            )

        ( _, Open ) ->
            ( { model
                | alertMessageText = alertMessageTextInit
              }
            , focusInputPossibly model
            )

        _ ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , commentText = commentTextInit
                , songCommentingMaybe = songCommentingMaybeNew
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
            ( { model
                | alertMessageText = alertMessageTextServerAwaiting
              }
            , focusInputPossibly model
            )

        _ ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , commentText = commentTextInit
                , songCommentingMaybe = songCommentingMaybeInit
              }
            , focusSetId "refresh"
            )
