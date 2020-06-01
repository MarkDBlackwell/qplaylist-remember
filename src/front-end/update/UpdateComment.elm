{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateComment exposing
    ( commentAreaInputTextChangeCaptureHand
    , commentAreaOpenHand
    , commentCancelHand
    )

import Alert
import ElmCycle
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
import UpdateFocus
import UpdateHelper
import Utilities
    exposing
        ( selectOneFromIndexMaybe
        )



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
            , UpdateFocus.cmdFocusInputPossibly model
            )

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedIndex
                        |> selectOneFromIndexMaybe model.songsRemembered
                        |> SongHelper.songsRememberedNewFromMaybeWithUpdate model

                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    songsRememberedIndex
                        |> selectOneFromIndexMaybe songsRememberedNew
            in
            case songsRememberedSelectOneMaybe of
                Nothing ->
                    UpdateHelper.elmCycleDefault model

                _ ->
                    ( { model
                        | alertMessageText = Alert.messageTextInit
                        , commentText = ModelInitialize.commentTextInit
                        , songCommentingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                      --'UpdateFocus.cmdFocusInputPossibly' doesn't work, here:
                    , UpdateFocus.cmdFocusSetId "input"
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
                , songCommentingMaybe = SongInitialize.songCommentingMaybeInit
              }
            , "Comment"
                |> SongHelper.buttonIdReconstruct model.songsRemembered model.songCommentingMaybe
                |> UpdateFocus.cmdFocusSetId
            )
