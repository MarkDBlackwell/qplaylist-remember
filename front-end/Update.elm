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
        , alertMessageTextServerAwaitingElmCycle
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import ModelType
    exposing
        ( Model
        , PageIsExpanded
        )
import Song
    exposing
        ( songsRememberedAppendOneUniqueFromIndex
        , songsRememberedUpdateTimestampFromIndex
        )
import SongHelper
    exposing
        ( song2SongTimeless
        , songs2SongsTimeless
        )
import SongPort
    exposing
        ( songsRememberedStore
        )
import SongType
    exposing
        ( SongCommentingMaybe
        , SongRecentMaybe
        , SongTimeless
        , SongsRemembered
        , SongsRememberedIndexList
        , SongsRememberedIndexMaybe
        )
import UpdateComment
    exposing
        ( commentAreaInputTextChangeCaptureHand
        , commentAreaOpenHand
        , commentCancelHand
        )
import UpdateFocus
    exposing
        ( focusAttempt
        , focusInputPossibly
        , focusSetId
        )
import UpdateHelper
    exposing
        ( stateVector
        )
import UpdateKeyboard
    exposing
        ( keystrokeHand
        )
import UpdateRequest
    exposing
        ( commentSendHand
        , likeButtonProcessHand
        , songsRecentRefreshHand
        )
import UpdateResponse
    exposing
        ( commentResponseErr
        , commentResponseOk
        , likeResponseErr
        , likeResponseOk
        , songsRecentResponseErr
        , songsRecentResponseOk
        )
import UserIdentifier
    exposing
        ( userIdentifierCalc
        )
import Utilities
    exposing
        ( matchingIndexes
        , msg2Cmd
        , selectOneFromIndexMaybe
        , withoutOneFromMaybe
        )


-- UPDATE


update : Msg -> Model -> ElmCycle
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

        FocusAttempt id ->
            focusAttempt model id

        KeystrokeHand keyCode ->
            keystrokeHand model keyCode

        LikeButtonProcessHand songsRememberedIndex ->
            likeButtonProcessHand model songsRememberedIndex

        LikeResponse (Err httpError) ->
            likeResponseErr model httpError

        LikeResponse (Ok httpResponseText) ->
            likeResponseOk model httpResponseText

        None ->
            ( model
            , focusInputPossibly model
            )

        PageMorphHand ->
            --(awaitingServer, commentArea)
            case stateVector model of
                ( True, _ ) ->
                    alertMessageTextServerAwaitingElmCycle model

                _ ->
                    let
                        pageIsExpandedNew : PageIsExpanded
                        pageIsExpandedNew =
                            let
                                bothListsAreEmpty : Bool
                                bothListsAreEmpty =
                                    List.all
                                        identity
                                        --Here, can't use List.map.
                                        [ List.isEmpty model.songsRecent
                                        , List.isEmpty model.songsRemembered
                                        ]
                            in
                            if bothListsAreEmpty then
                                model.pageIsExpanded
                            else
                                not model.pageIsExpanded
                    in
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , focusInputPossibly model
                    )

        SongForgetHand songsRememberedIndex ->
            --(awaitingServer, commentArea)
            case stateVector model of
                ( True, _ ) ->
                    alertMessageTextServerAwaitingElmCycle model

                _ ->
                    let
                        songRememberedSelectOneMaybe : SongCommentingMaybe
                        songRememberedSelectOneMaybe =
                            selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex
                    in
                    if model.songCommentingMaybe == songRememberedSelectOneMaybe then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        let
                            songsRememberedNew : SongsRemembered
                            songsRememberedNew =
                                withoutOneFromMaybe model.songsRemembered songRememberedSelectOneMaybe
                        in
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , songsRemembered = songsRememberedNew
                          }
                        , Cmd.batch
                            [ msg2Cmd SongsRememberedStore
                            , focusSetId "refresh"
                            , focusInputPossibly model
                            ]
                        )

        SongRememberHand songsRecentIndex ->
            --(awaitingServer, commentArea)
            case stateVector model of
                ( True, _ ) ->
                    alertMessageTextServerAwaitingElmCycle model

                _ ->
                    let
                        songsRememberedNew : SongsRemembered
                        songsRememberedNew =
                            let
                                songsRememberedAppended : SongsRemembered
                                songsRememberedAppended =
                                    songsRememberedAppendOneUniqueFromIndex
                                        model.songsRemembered
                                        model.songsRecent
                                        songsRecentIndex

                                songsRememberedIndexMaybe : SongsRememberedIndexMaybe
                                songsRememberedIndexMaybe =
                                    let
                                        songRecentSelectOneMaybe : SongRecentMaybe
                                        songRecentSelectOneMaybe =
                                            selectOneFromIndexMaybe model.songsRecent songsRecentIndex

                                        songsRememberedIndexes : SongTimeless -> SongsRememberedIndexList
                                        songsRememberedIndexes =
                                            songs2SongsTimeless songsRememberedAppended
                                                |> matchingIndexes
                                    in
                                    Maybe.map song2SongTimeless songRecentSelectOneMaybe
                                        |> Maybe.map songsRememberedIndexes
                                        |> Maybe.andThen List.head
                            in
                            songsRememberedUpdateTimestampFromIndex songsRememberedAppended model.songsRecent
                                |> flip Maybe.map songsRememberedIndexMaybe
                                |> Maybe.withDefault songsRememberedAppended
                    in
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ msg2Cmd SongsRememberedStore
                        , focusInputPossibly model
                        ]
                    )

        SongsRecentRefreshHand ->
            songsRecentRefreshHand model

        SongsRecentResponse (Err httpError) ->
            songsRecentResponseErr model httpError

        SongsRecentResponse (Ok httpResponseText) ->
            songsRecentResponseOk model httpResponseText

        SongsRememberedStore ->
            songsRememberedStore model

        UserIdentifierEstablish randomInt ->
            ( { model
                | userIdentifier = userIdentifierCalc randomInt
              }
            , Cmd.batch
                [ focusSetId "refresh"
                , focusInputPossibly model
                ]
            )
