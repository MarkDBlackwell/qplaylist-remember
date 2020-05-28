{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Update exposing (update)

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
import SongHelper
    exposing
        ( songsRememberedAppendOneUniqueFromMaybe
        , songsRememberedUpdateTimestampFromMaybe
        )
import SongPort
    exposing
        ( songsRememberedStore
        )
import SongType
    exposing
        ( SongRecentMaybe
        , SongRememberedMaybe
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
        ( elmCycleDefault
        , stateVector
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
import UpdateRequestType
    exposing
        ( ActionLikeOrComment(..)
        )
import UpdateResponse
    exposing
        ( likeOrCommentResponseErr
        , likeOrCommentResponseOk
        , songsRecentResponseErr
        , songsRecentResponseOk
        )
import UserIdentifier
    exposing
        ( userIdentifierCalc
        )
import Utilities
    exposing
        ( msg2Cmd
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
            likeOrCommentResponseErr model httpError Comment

        CommentResponse (Ok httpResponseText) ->
            likeOrCommentResponseOk model httpResponseText Comment

        CommentSendHand ->
            commentSendHand model

        FocusAttempt id ->
            focusAttempt model id

        KeystrokeHand keyChar ->
            keystrokeHand model keyChar

        LikeButtonProcessHand songsRememberedIndex ->
            likeButtonProcessHand model songsRememberedIndex

        LikeResponse (Err httpError) ->
            likeOrCommentResponseErr model httpError Like

        LikeResponse (Ok httpResponseText) ->
            likeOrCommentResponseOk model httpResponseText Like

        None ->
            elmCycleDefault model

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
                        songsRememberedSelectOneMaybe : SongRememberedMaybe
                        songsRememberedSelectOneMaybe =
                            selectOneFromIndexMaybe model.songsRemembered songsRememberedIndex
                    in
                    if model.songCommentingMaybe == songsRememberedSelectOneMaybe then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )

                    else
                        let
                            songsRememberedNew : SongsRemembered
                            songsRememberedNew =
                                withoutOneFromMaybe model.songsRemembered songsRememberedSelectOneMaybe
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
                                songsRecentSelectOneMaybe : SongRecentMaybe
                                songsRecentSelectOneMaybe =
                                    selectOneFromIndexMaybe model.songsRecent songsRecentIndex

                                songsRememberedAppended : SongsRemembered
                                songsRememberedAppended =
                                    songsRememberedAppendOneUniqueFromMaybe
                                        model.songsRemembered
                                        model.songsRecent
                                        songsRecentSelectOneMaybe
                            in
                            songsRememberedUpdateTimestampFromMaybe
                                songsRememberedAppended
                                model.songsRecent
                                songsRecentSelectOneMaybe
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
