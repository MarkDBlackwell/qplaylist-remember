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
                            entry 'C' "Comment latest remembered"

                        like : String
                        like =
                            entry 'L' "Like latest remembered"
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
            [ entry 'F' "reFresh"
            , entry 'R' "Remember latest played"
            , likeComment
            , entry 'M' "Morph"
            , entry 'H' "this Help"
            ]
                |> String.join separator
    in
    ( { model
        | alertMessageText = Just alertMessageTextNew
      }
    , UpdateFocus.focusInputPossibly model
    )


keystrokeHand : Model -> KeyChar -> ElmCycle
keystrokeHand model keyCharRaw =
    let
        doNothing : ElmCycle
        doNothing =
            elmCycleDefault model
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
                        , UpdateFocus.focusInputPossibly model
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
                    List.length model.songsRemembered
                        |> (\a -> (-) a 1)
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
