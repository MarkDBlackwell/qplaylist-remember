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
        ( songsRememberedAppendOneUnique
        , songsRememberedUpdateTimestamp
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
        ( initialSetUp
        )
import Utilities
    exposing
        ( matchingIndexes
        , msg2Cmd
        , selectOneMaybe
        , withoutOne
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

        InitialSetUp threeLetterNumberSpaceInt ->
            ( initialSetUp model threeLetterNumberSpaceInt
            , focusSetId "refresh"
            )

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
            let
                pageIsExpandedNew : PageIsExpanded
                pageIsExpandedNew =
                    let
                        bothListsAreEmpty : Bool
                        bothListsAreEmpty =
                            List.any
                                identity
                                --Here, can't use List.all.
                                [ List.isEmpty model.songsRecent
                                , List.isEmpty model.songsRemembered
                                ]
                    in
                    if bothListsAreEmpty then
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

        SongForgetHand songsRememberedIndex ->
            let
                songRememberedCompareMaybe : SongCommentingMaybe
                songRememberedCompareMaybe =
                    selectOneMaybe model.songsRemembered songsRememberedIndex

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    withoutOne model.songsRemembered songsRememberedIndex
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
                    if model.songCommentingMaybe == songRememberedCompareMaybe then
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
                            [ msg2Cmd SongsRememberedStore
                            , focusSetId "refresh"
                            , focusInputPossibly model
                            ]
                        )

        SongRememberHand songsRecentIndex ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    let
                        songsRememberedAppended : SongsRemembered
                        songsRememberedAppended =
                            songsRememberedAppendOneUnique
                                model.songsRecent
                                songsRecentIndex
                                model.songsRemembered

                        songsRememberedIndexMaybe : SongsRememberedIndexMaybe
                        songsRememberedIndexMaybe =
                            let
                                songsRememberedIndexes : SongTimeless -> SongsRememberedIndexList
                                songsRememberedIndexes =
                                    songs2SongsTimeless songsRememberedAppended
                                        |> matchingIndexes
                            in
                            selectOneMaybe model.songsRecent songsRecentIndex
                                |> Maybe.map song2SongTimeless
                                |> Maybe.map songsRememberedIndexes
                                |> Maybe.andThen List.head
                    in
                    songsRememberedUpdateTimestamp model.songsRecent songsRememberedAppended
                        |> flip Maybe.map songsRememberedIndexMaybe
                        |> Maybe.withDefault songsRememberedAppended
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
