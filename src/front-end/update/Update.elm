{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Update exposing (update)

import Alert
import ElmCycle
    exposing
        ( Msg(..)
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
import UpdateFocus
import UpdateHelper
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
        ( cmdMsg2Cmd
        , selectOneFromIndexMaybe
        , withoutOneFromMaybe
        )



-- UPDATE


update : ElmCycle.Msg -> Model -> ElmCycle.ElmCycle
update msg model =
    case msg of
        MsgCommentAreaInputTextChangeCaptureHand text ->
            text
                |> UpdateComment.commentAreaInputTextChangeCaptureHand model

        MsgCommentAreaOpenHand songsRememberedIndex ->
            songsRememberedIndex
                |> UpdateComment.commentAreaOpenHand model

        MsgCommentCancelHand ->
            UpdateComment.commentCancelHand model

        MsgCommentResponse (Err httpError) ->
            UpdateResponse.likeOrCommentResponseErr model httpError Comment

        MsgCommentResponse (Ok httpResponseText) ->
            Comment
                |> UpdateResponse.likeOrCommentResponseOk model httpResponseText

        MsgCommentSendHand ->
            UpdateRequest.commentSendHand model

        MsgFocusAttempt id ->
            id
                |> UpdateFocus.focusAttempt model

        MsgKeystrokeHand keyChar ->
            keyChar
                |> UpdateKeyboard.keystrokeHand model

        MsgLikeButtonProcessHand songsRememberedIndex ->
            songsRememberedIndex
                |> UpdateRequest.likeButtonProcessHand model

        MsgLikeResponse (Err httpError) ->
            UpdateResponse.likeOrCommentResponseErr model httpError Like

        MsgLikeResponse (Ok httpResponseText) ->
            UpdateResponse.likeOrCommentResponseOk model httpResponseText Like

        MsgNone ->
            UpdateHelper.elmCycleDefault model

        MsgPageMorphHand ->
            case UpdateHelper.stateVector model of
                --(awaitingServer, commentArea)
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

                _ ->
                    let
                        pageIsExpandedNew : PageIsExpanded
                        pageIsExpandedNew =
                            let
                                bothListsAreEmpty : Bool
                                bothListsAreEmpty =
                                    --Can't combine, in a list, lists of different types:
                                    [ List.isEmpty model.songsRecent
                                    , List.isEmpty model.songsRemembered
                                    ]
                                        |> List.foldl (&&) True
                            in
                            if bothListsAreEmpty then
                                model.pageIsExpanded

                            else
                                not model.pageIsExpanded
                    in
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , UpdateFocus.cmdFocusInputPossibly model
                    )

        MsgSongForgetHand songsRememberedIndex ->
            case UpdateHelper.stateVector model of
                --(awaitingServer, commentArea)
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

                _ ->
                    let
                        songsRememberedSelectOneMaybe : SongRememberedMaybe
                        songsRememberedSelectOneMaybe =
                            songsRememberedIndex
                                |> selectOneFromIndexMaybe model.songsRemembered
                    in
                    if model.songCommentingMaybe == songsRememberedSelectOneMaybe then
                        ( { model
                            | alertMessageText = Alert.messageTextInit
                          }
                        , UpdateFocus.cmdFocusInputPossibly model
                        )

                    else
                        let
                            songsRememberedNew : SongsRemembered
                            songsRememberedNew =
                                songsRememberedSelectOneMaybe
                                    |> withoutOneFromMaybe model.songsRemembered
                        in
                        ( { model
                            | alertMessageText = Alert.messageTextInit
                            , songsRemembered = songsRememberedNew
                          }
                        , Cmd.batch
                            [ cmdMsg2Cmd MsgSongsRememberedStore
                            , UpdateFocus.cmdFocusSetId "refresh"
                            , UpdateFocus.cmdFocusInputPossibly model
                            ]
                        )

        MsgSongRememberHand songsRecentIndex ->
            case UpdateHelper.stateVector model of
                --(awaitingServer, commentArea)
                ( True, _ ) ->
                    Alert.messageTextServerAwaitingElmCycle model

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
                        | alertMessageText = Alert.messageTextInit
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ cmdMsg2Cmd MsgSongsRememberedStore
                        , UpdateFocus.cmdFocusInputPossibly model
                        ]
                    )

        MsgSongsRecentRefreshHand ->
            UpdateRequest.songsRecentRefreshHand model

        MsgSongsRecentResponse (Err httpError) ->
            httpError
                |> UpdateResponse.songsRecentResponseErr model

        MsgSongsRecentResponse (Ok httpResponseText) ->
            httpResponseText
                |> UpdateResponse.songsRecentResponseOk model

        MsgSongsRememberedStore ->
            SongPort.songsRememberedStore model

        MsgUserIdentifierEstablish randomInt ->
            ( { model
                | userIdentifier =
                    randomInt
                        |> UserIdentifier.userIdentifierCalc
              }
            , Cmd.batch
                [ UpdateFocus.cmdFocusSetId "refresh"
                , UpdateFocus.cmdFocusInputPossibly model
                ]
            )
