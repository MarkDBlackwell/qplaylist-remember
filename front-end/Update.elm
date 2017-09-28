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


module Update
    exposing
        ( update
        )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaiting
        )
import MessageType
    exposing
        ( Msg(..)
        )
import ModelType
    exposing
        ( Model
        , PageIsExpanded
        )
import Song
    exposing
        ( SongCommenting
        , SongsRemembered
        , songLikingOrCommentingMaybe
        , songsRememberedAppendOneUnique
        , songsRememberedWithoutOne
        )
import SongPort
    exposing
        ( songsRememberedUpdate
        )
import UpdateCommentArea
    exposing
        ( commentAreaInputTextChangeCaptureHand
        , commentAreaOpenHand
        , commentCancelHand
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusResult
        , focusSet
        )
import UpdateLog
    exposing
        ( httpRequestOrResponseTextLog
        )
import UpdateRequest
    exposing
        ( commentSendHand
        , likeButtonProcessHand
        , songsLatestRefreshHand
        )
import UpdateResponse
    exposing
        ( commentResponseErr
        , commentResponseOk
        , likeResponseErr
        , likeResponseOk
        , songsLatestResponseErr
        , songsLatestResponseOk
        )
import UpdateStateVector
    exposing
        ( stateVector
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )
import UserIdentifier
    exposing
        ( updateInitialSetUp
        )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommentAreaInputTextChangeCaptureHand text ->
            commentAreaInputTextChangeCaptureHand model text

        CommentAreaOpenHand songsRememberedIndex ->
            commentAreaOpenHand model songsRememberedIndex

        CommentCancelHand ->
            commentCancelHand model

        CommentResponse (Err httpError) ->
            commentResponseErr model httpError

        CommentResponse (Ok httpResponseText) ->
            commentResponseOk model httpResponseText

        CommentSendHand ->
            commentSendHand model

        FocusResult _ ->
            focusResult model

        FocusSet id ->
            focusSet model id

        HttpRequestOrResponseTextLog requestOrResponseLabelText httpRequestOrResponseText ->
            httpRequestOrResponseTextLog model requestOrResponseLabelText httpRequestOrResponseText

        InitialSetUp threeLetterSpaceInt ->
            ( { model
                | userIdentifier = updateInitialSetUp threeLetterSpaceInt
              }
            , Cmd.none
            )

        LikeButtonProcessHand songsRememberedIndex ->
            likeButtonProcessHand model songsRememberedIndex

        LikeResponse (Err httpError) ->
            likeResponseErr model httpError

        LikeResponse (Ok httpResponseText) ->
            likeResponseOk model httpResponseText

        PageMorphHand ->
            let
                pageIsExpandedNew : PageIsExpanded
                pageIsExpandedNew =
                    --Here, can't use List.all.
                    if
                        List.isEmpty model.songsLatest
                            && List.isEmpty model.songsRemembered
                    then
                        model.pageIsExpanded
                    else
                        not model.pageIsExpanded
            in
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
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , focusInputPossibly model
                    )

        SongBuyAnchorProcessHand ->
            ( model
            , focusInputPossibly model
            )

        SongForgetHand songsRememberedIndex ->
            let
                songRememberedCompare : SongCommenting
                songRememberedCompare =
                    songLikingOrCommentingMaybe model.songsRemembered songsRememberedIndex

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedWithoutOne model.songsRemembered songsRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector model of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    if model.songCommenting == songRememberedCompare then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , songsRemembered = songsRememberedNew
                          }
                        , Cmd.batch
                            [ msg2Cmd SongsRememberedUpdate
                            , focusInputPossibly model
                            ]
                        )

        SongRememberHand songsLatestIndex ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedAppendOneUnique model.songsLatest songsLatestIndex model.songsRemembered
            in
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
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ msg2Cmd SongsRememberedUpdate
                        , focusInputPossibly model
                        ]
                    )

        SongsLatestRefreshHand ->
            songsLatestRefreshHand model

        SongsLatestResponse (Err httpError) ->
            songsLatestResponseErr model httpError

        SongsLatestResponse (Ok httpResponseText) ->
            songsLatestResponseOk model httpResponseText

        SongsRememberedUpdate ->
            songsRememberedUpdate model
