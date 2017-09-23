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
import UpdateCommentArea
    exposing
        ( updateCommentAreaInputTextChangeCaptureHand
        , updateCommentAreaOpenHand
        , updateCommentCancelHand
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , updateFocusResult
        , updateFocusSet
        )
import UpdateLog
    exposing
        ( updateHttpRequestOrResponseTextLog
        )
import UpdateRequest
    exposing
        ( updateCommentSendHand
        , updateLikeButtonProcessHand
        , updateSongsLatestRefreshHand
        )
import UpdateResponse
    exposing
        ( updateCommentResponseErr
        , updateCommentResponseOk
        , updateLikeResponseErr
        , updateLikeResponseOk
        , updateSongsLatestResponseErr
        , updateSongsLatestResponseOk
        )
import UpdateStateVector
    exposing
        ( stateVector
        )
import UserIdentifier
    exposing
        ( UserIdentifier
        , keyCode2Char
        , threeDigits
        )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommentAreaInputTextChangeCaptureHand text ->
            updateCommentAreaInputTextChangeCaptureHand model text

        CommentAreaOpenHand songsRememberedIndex ->
            updateCommentAreaOpenHand model songsRememberedIndex

        CommentCancelHand ->
            updateCommentCancelHand model

        CommentResponse (Err httpError) ->
            updateCommentResponseErr model httpError

        CommentResponse (Ok httpResponseText) ->
            updateCommentResponseOk model httpResponseText

        CommentSendHand ->
            updateCommentSendHand model

        FocusResult _ ->
            updateFocusResult model

        FocusSet id ->
            updateFocusSet model id

        HttpRequestOrResponseTextLog requestOrResponseLabelText httpRequestOrResponseText ->
            updateHttpRequestOrResponseTextLog model requestOrResponseLabelText httpRequestOrResponseText

        InitialSetUp threeLetterSpace ->
            let
                userIdentifierNew : UserIdentifier
                userIdentifierNew =
                    String.fromList (List.map keyCode2Char (threeDigits threeLetterSpace))
            in
            ( { model
                | userIdentifier = userIdentifierNew
              }
            , Cmd.none
            )

        LikeButtonProcessHand songsRememberedIndex ->
            updateLikeButtonProcessHand model songsRememberedIndex

        LikeResponse (Err httpError) ->
            updateLikeResponseErr model httpError

        LikeResponse (Ok httpResponseText) ->
            updateLikeResponseOk model httpResponseText

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
                        , focusInputPossibly model
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
                    , focusInputPossibly model
                    )

        SongsLatestRefreshHand ->
            updateSongsLatestRefreshHand model

        SongsLatestResponse (Err httpError) ->
            updateSongsLatestResponseErr model httpError

        SongsLatestResponse (Ok httpResponseText) ->
            updateSongsLatestResponseOk model httpResponseText
