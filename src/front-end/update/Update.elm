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
import UpdateRequest
import UpdateRequestType
    exposing
        ( ActionLikeOrComment(..)
        )
import UpdateResponse
import UserIdentifier
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
            text
                |> commentAreaInputTextChangeCaptureHand model

        CommentAreaOpenHand songsRememberedIndex ->
            songsRememberedIndex
                |> commentAreaOpenHand model

        CommentCancelHand ->
            commentCancelHand model

        CommentResponse (Err httpError) ->
            UpdateResponse.likeOrCommentResponseErr model httpError Comment

        CommentResponse (Ok httpResponseText) ->
            Comment
                |> UpdateResponse.likeOrCommentResponseOk model httpResponseText

        CommentSendHand ->
            UpdateRequest.commentSendHand model

        FocusAttempt id ->
            id
                |> focusAttempt model

        KeystrokeHand keyChar ->
            keyChar
                |> UpdateKeyboard.keystrokeHand model

        LikeButtonProcessHand songsRememberedIndex ->
            songsRememberedIndex
                |> UpdateRequest.likeButtonProcessHand model

        LikeResponse (Err httpError) ->
            UpdateResponse.likeOrCommentResponseErr model httpError Like

        LikeResponse (Ok httpResponseText) ->
            UpdateResponse.likeOrCommentResponseOk model httpResponseText Like

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
                            songsRememberedIndex
                                |> selectOneFromIndexMaybe model.songsRemembered
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
                                songsRememberedSelectOneMaybe
                                    |> withoutOneFromMaybe model.songsRemembered
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
                                    songsRecentIndex
                                        |> selectOneFromIndexMaybe model.songsRecent

                                songsRememberedAppended : SongsRemembered
                                songsRememberedAppended =
                                    songsRecentSelectOneMaybe
                                        |> songsRememberedAppendOneUniqueFromMaybe
                                            model.songsRemembered
                                            model.songsRecent
                            in
                            songsRecentSelectOneMaybe
                                |> songsRememberedUpdateTimestampFromMaybe
                                    songsRememberedAppended
                                    model.songsRecent
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
            UpdateRequest.songsRecentRefreshHand model

        SongsRecentResponse (Err httpError) ->
            httpError
                |> UpdateResponse.songsRecentResponseErr model

        SongsRecentResponse (Ok httpResponseText) ->
            httpResponseText
                |> UpdateResponse.songsRecentResponseOk model

        SongsRememberedStore ->
            SongPort.songsRememberedStore model

        UserIdentifierEstablish randomInt ->
            ( { model
                | userIdentifier =
                    randomInt
                        |> UserIdentifier.userIdentifierCalc
              }
            , Cmd.batch
                [ focusSetId "refresh"
                , focusInputPossibly model
                ]
            )
