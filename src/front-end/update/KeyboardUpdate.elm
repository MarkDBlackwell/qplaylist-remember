{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module KeyboardUpdate exposing (keystrokeHand)

import AlertType
    exposing
        ( AlertMessageText
        )
import ElmCycle
    exposing
        ( Msg(..)
        )
import FocusUpdate
import ModelType
    exposing
        ( CommentAreaOptional(..)
        , Model
        )
import SongType
    exposing
        ( SongsRememberedIndex
        )
import UpdateHelper
import Utilities
    exposing
        ( cmdMsg2Cmd
        , idMorphString
        , idRefreshString
        , pred
        )
import ViewType
    exposing
        ( Id
        , KeyChar
        )



-- UPDATE


keyProcessH : Model -> ElmCycle.ElmCycle
keyProcessH model =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            let
                entry : Char -> String -> Maybe String
                entry letter string =
                    let
                        letterParenthesized : String
                        letterParenthesized =
                            [ '(', ')' ]
                                |> List.intersperse letter
                                |> String.fromList
                    in
                    [ letterParenthesized
                    , string
                    ]
                        |> String.join " "
                        |> Just

                comment : Maybe String
                comment =
                    if not model.showCommentButtons then
                        Nothing

                    else
                        "Comment on latest remembered"
                            |> entry 'C'

                separator : String
                separator =
                    --The alert box compresses multiple blank characters.
                    "; "
            in
            --Keep alphabetized by entry letter:
            [ comment
            , "reFresh"
                |> entry 'F'
            , "this Help"
                |> entry 'H'
            , "Like latest remembered"
                |> entry 'L'
            , "Morph"
                |> entry 'M'
            , "Remember latest played"
                |> entry 'R'
            ]
                |> List.filterMap identity
                |> String.join separator
    in
    ( { model
        | alertMessageText = Just alertMessageTextNew
      }
    , model
        |> FocusUpdate.cmdFocusInputPossibly
    )


keystrokeHand : Model -> KeyChar -> ElmCycle.ElmCycle
keystrokeHand model keyCharRaw =
    let
        doNothing : ElmCycle.ElmCycle
        doNothing =
            model
                |> UpdateHelper.elmCycleDefault
    in
    case UpdateHelper.commentAreaStateVector model of
        --( AwaitingServerResponse, CommentAreaOptional )
        ( False, CommentAreaClosed ) ->
            let
                doMessage : Id -> ElmCycle.Msg -> ElmCycle.ElmCycle
                doMessage id msg =
                    ( model
                    , Cmd.batch
                        [ msg
                            |> cmdMsg2Cmd
                        , FocusUpdate.cmdFocusSetId id
                        , model
                            |> FocusUpdate.cmdFocusInputPossibly
                        ]
                    )

                keyChar : String
                keyChar =
                    String.toUpper keyCharRaw

                keyIs : Char -> Bool
                keyIs letter =
                    keyChar == String.fromChar letter

                songsRememberedIndex : SongsRememberedIndex
                songsRememberedIndex =
                    model.songsRemembered
                        |> List.length
                        |> pred
            in
            if keyIs 'H' then
                keyProcessH model

            else if keyIs 'C' then
                songsRememberedIndex
                    |> MsgCommentAreaOpenHand
                    |> doMessage idRefreshString

            else if keyIs 'F' then
                MsgSongsRecentRefreshHand
                    |> doMessage idRefreshString

            else if keyIs 'L' then
                songsRememberedIndex
                    |> MsgLikeSendHand
                    |> doMessage idRefreshString

            else if keyIs 'M' then
                MsgPageMorphHand
                    |> doMessage idMorphString

            else if keyIs 'R' then
                MsgSongRememberHand 0
                    |> doMessage idRefreshString

            else
                doNothing

        _ ->
            doNothing
