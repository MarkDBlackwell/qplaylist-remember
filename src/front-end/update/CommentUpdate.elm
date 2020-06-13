{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module CommentUpdate exposing
    ( commentAreaInputTextChangeCaptureHand
    , commentAreaOpenHand
    , commentCancelHand
    )

import Alert
import ElmCycle
import FocusUpdate
import ModelInitialize
import ModelType
    exposing
        ( CommentAreaOptional(..)
        , Model
        )
import SongHelper
import SongInitialize
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateHelper



-- UPDATE


commentAreaInputTextChangeCaptureHand : Model -> String -> ElmCycle.ElmCycle
commentAreaInputTextChangeCaptureHand model text =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            ( { model
                | commentText = text
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | alertMessageText = Alert.messageTextInit
                , commentText = text
              }
            , Cmd.none
            )


commentAreaOpenHand : Model -> SongsRememberedIndex -> ElmCycle.ElmCycle
commentAreaOpenHand model songsRememberedIndex =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        ( _, CommentAreaOpen ) ->
            ( { model
                | alertMessageText = Alert.messageTextInit
              }
            , FocusUpdate.cmdFocusInputPossibly model
            )

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    model.songsRemembered
                        |> List.drop songsRememberedIndex
                        |> List.head
                        |> SongHelper.songsRememberedNewFromMaybeWithUpdate model

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    songsRememberedNew
                        |> List.drop songsRememberedIndex
                        |> List.head
            in
            case songsRememberedSelectOneMaybe of
                Nothing ->
                    UpdateHelper.elmCycleDefault model

                _ ->
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , commentText = ModelInitialize.commentTextInit
                        , songCommentingOnNowMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                      --'FocusUpdate.cmdFocusInputPossibly' doesn't work, here:
                    , FocusUpdate.cmdFocusSetId "input"
                    )


commentCancelHand : Model -> ElmCycle.ElmCycle
commentCancelHand model =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            ( { model
                | alertMessageText = Alert.messageTextInit
                , commentText = ModelInitialize.commentTextInit
                , songCommentingOnNowMaybe = SongInitialize.songCommentingOnNowMaybeInit
              }
            , "Comment"
                |> SongHelper.buttonIdReconstruct
                    model.songsRemembered
                    model.songCommentingOnNowMaybe
                |> FocusUpdate.cmdFocusSetId
            )
