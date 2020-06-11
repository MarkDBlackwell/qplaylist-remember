{- Copyright (C) 2020 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module SongListUpdate exposing
    ( pageMorphHand
    , songForgetHand
    , songRememberHand
    )

import Alert
import ElmCycle
    exposing
        ( Msg(..)
        )
import FocusUpdate
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
import SongType
    exposing
        ( SongRecentMaybe
        , SongRememberedMaybe
        , SongsRecentIndex
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateHelper
import Utilities
    exposing
        ( cmdMsg2Cmd
        , selectOneFromIndexMaybe
        , withoutOneFromMaybe
        )



-- UPDATE


pageMorphHand : Model -> ElmCycle.ElmCycle
pageMorphHand model =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
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
                        model.pageIsExpanded
                            |> not
            in
            ( { model
                | alertMessageText = Alert.messageTextInit
                , pageIsExpanded = pageIsExpandedNew
              }
            , FocusUpdate.cmdFocusInputPossibly model
            )


songForgetHand : Model -> SongsRememberedIndex -> ElmCycle.ElmCycle
songForgetHand model songsRememberedIndex =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            let
                songsRememberedSelectOneMaybe : SongRememberedMaybe
                songsRememberedSelectOneMaybe =
                    model.songsRemembered
                        |> selectOneFromIndexMaybe songsRememberedIndex
            in
            if model.songCommentingOnNowMaybe == songsRememberedSelectOneMaybe then
                ( { model
                    | alertMessageText = Alert.messageTextInit
                  }
                , FocusUpdate.cmdFocusInputPossibly model
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
                    [ MsgSongsRememberedStore
                        |> cmdMsg2Cmd
                    , FocusUpdate.cmdFocusSetId "refresh"
                    , FocusUpdate.cmdFocusInputPossibly model
                    ]
                )


songRememberHand : Model -> SongsRecentIndex -> ElmCycle.ElmCycle
songRememberHand model songsRecentIndex =
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( True, _ ) ->
            Alert.messageTextServerAwaitingElmCycle model

        _ ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    let
                        songsRecentSelectOneMaybe : SongRecentMaybe
                        songsRecentSelectOneMaybe =
                            model.songsRecent
                                |> selectOneFromIndexMaybe songsRecentIndex

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
                [ MsgSongsRememberedStore
                    |> cmdMsg2Cmd
                , FocusUpdate.cmdFocusInputPossibly model
                ]
            )
