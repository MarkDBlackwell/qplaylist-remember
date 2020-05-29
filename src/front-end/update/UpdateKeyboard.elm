{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateKeyboard exposing (keystrokeHand)

import AlertType
    exposing
        ( AlertMessageText
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import ModelType
    exposing
        ( Model
        , Optional(..)
        )
import SongType
    exposing
        ( SongsRememberedIndex
        )
import UpdateFocus
import UpdateHelper
    exposing
        ( elmCycleDefault
        , stateVector
        )
import Utilities
    exposing
        ( msg2Cmd
        , pred
        )
import ViewType
    exposing
        ( Id
        , KeyChar
        )



-- UPDATE


keyProcessH : Model -> ElmCycle
keyProcessH model =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            let
                entry : Char -> String -> String
                entry letter string =
                    [ String.fromChar letter, "–", string ]
                        |> String.join " "

                likeComment : String
                likeComment =
                    let
                        comment : String
                        comment =
                            "Comment latest remembered"
                                |> entry 'C'

                        like : String
                        like =
                            "Like latest remembered"
                                |> entry 'L'
                    in
                    if not model.showCommentButtons then
                        like

                    else
                        [ like, comment ]
                            |> String.join separator

                separator : String
                separator =
                    --The alert box compresses multiple blank characters.
                    "; "
            in
            [ "reFresh"
                |> entry 'F'
            , "Remember latest played"
                |> entry 'R'
            , likeComment
            , "Morph"
                |> entry 'M'
            , "this Help"
                |> entry 'H'
            ]
                |> String.join separator
    in
    ( { model
        | alertMessageText = Just alertMessageTextNew
      }
    , model
        |> UpdateFocus.focusInputPossibly
    )


keystrokeHand : Model -> KeyChar -> ElmCycle
keystrokeHand model keyCharRaw =
    let
        doNothing : ElmCycle
        doNothing =
            model
                |> elmCycleDefault
    in
    --(awaitingServer, commentArea)
    case stateVector model of
        ( False, Closed ) ->
            let
                doMessage : Id -> Msg -> ElmCycle
                doMessage id msg =
                    ( model
                    , Cmd.batch
                        [ msg2Cmd msg
                        , UpdateFocus.focusSetId id
                        , model
                            |> UpdateFocus.focusInputPossibly
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
                    |> CommentAreaOpenHand
                    |> doMessage "refresh"

            else if keyIs 'F' then
                SongsRecentRefreshHand
                    |> doMessage "refresh"

            else if keyIs 'L' then
                songsRememberedIndex
                    |> LikeButtonProcessHand
                    |> doMessage "refresh"

            else if keyIs 'M' then
                PageMorphHand
                    |> doMessage "morph"

            else if keyIs 'R' then
                SongRememberHand 0
                    |> doMessage "refresh"

            else
                doNothing

        _ ->
            doNothing
