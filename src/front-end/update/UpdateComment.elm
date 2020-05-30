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
        ( Model
        , Optional(..)
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
    --(awaitingServer, commentArea)
    case UpdateHelper.stateVector model of
        ( True, _ ) ->
            ( { model
                | commentText = text
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | alertMessageText = Alert.alertMessageTextInit
                , commentText = text
              }
            , Cmd.none
            )


commentAreaOpenHand : Model -> SongsRememberedIndex -> ElmCycle.ElmCycle
commentAreaOpenHand model songsRememberedIndex =
    --(awaitingServer, commentArea)
    case UpdateHelper.stateVector model of
        ( True, _ ) ->
            Alert.alertMessageTextServerAwaitingElmCycle model

        ( _, Open ) ->
            ( { model
                | alertMessageText = Alert.alertMessageTextInit
              }
            , UpdateFocus.focusInputPossibly model
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
                        | alertMessageText = Alert.alertMessageTextInit
                        , commentText = ModelInitialize.commentTextInit
                        , songCommentingMaybe = songsRememberedSelectOneMaybe
                        , songsRemembered = songsRememberedNew
                      }
                      --'UpdateFocus.focusInputPossibly' doesn't work, here:
                    , UpdateFocus.focusSetId "input"
                    )


commentCancelHand : Model -> ElmCycle.ElmCycle
commentCancelHand model =
    --(awaitingServer, commentArea)
    case UpdateHelper.stateVector model of
        ( True, _ ) ->
            Alert.alertMessageTextServerAwaitingElmCycle model

        _ ->
            ( { model
                | alertMessageText = Alert.alertMessageTextInit
                , commentText = ModelInitialize.commentTextInit
                , songCommentingMaybe = SongInitialize.songCommentingMaybeInit
              }
            , "Comment"
                |> SongHelper.buttonIdReconstruct model.songsRemembered model.songCommentingMaybe
                |> UpdateFocus.focusSetId
            )
