{- Copyright (C) 2017 Mark D. Blackwell.
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module UpdateKeyboard
    exposing
        ( keystrokeHand
        )

import AlertType
    exposing
        ( AlertMessageText
        )
import Char
    exposing
        ( KeyCode
        , toCode
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( PageMorphHand
            , SongRememberHand
            , SongsRecentRefreshHand
            )
        )
import ModelType
    exposing
        ( Model
        , Optional
            ( Closed
            )
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateHelper
    exposing
        ( stateVector
        )
import Utilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


keystrokeHand : Model -> KeyCode -> ElmCycle
keystrokeHand model keyCode =
    let
        doMessage : Msg -> ElmCycle
        doMessage msg =
            ( model
            , Cmd.batch
                [ msg2Cmd msg
                , focusInputPossibly model
                ]
            )

        doNothing : ElmCycle
        doNothing =
            ( model
            , focusInputPossibly model
            )
    in
    --(awaitingServer, commentArea)
    case stateVector model of
        ( False, Closed ) ->
            if keyCode == Char.toCode 'H' then
                let
                    alertMessageTextNew : AlertMessageText
                    alertMessageTextNew =
                        let
                            entry : String -> String -> String
                            entry letter string =
                                [ letter, "–", string ]
                                    |> String.join " "

                            likeComment : String
                            likeComment =
                                let
                                    comment : String
                                    comment =
                                        entry "C" "Comment latest remembered"

                                    like : String
                                    like =
                                        entry "L" "Like latest remembered"
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
                        String.join
                            separator
                            [ entry "F" "reFresh"
                            , entry "R" "Remember latest played"
                            , likeComment
                            , entry "M" "Morph"
                            , entry "H" "this Help"
                            ]
                in
                ( { model
                    | alertMessageText = Just alertMessageTextNew
                  }
                , focusInputPossibly model
                )
            else if keyCode == Char.toCode 'C' then
                doNothing
            else if keyCode == Char.toCode 'F' then
                doMessage SongsRecentRefreshHand
            else if keyCode == Char.toCode 'L' then
                doNothing
            else if keyCode == Char.toCode 'M' then
                doMessage PageMorphHand
            else if keyCode == Char.toCode 'R' then
                SongRememberHand 0
                    |> doMessage
            else
                doNothing

        _ ->
            doNothing
