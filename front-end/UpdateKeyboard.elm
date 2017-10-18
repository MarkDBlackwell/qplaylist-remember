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
import Dom
    exposing
        ( Id
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( CommentAreaOpenHand
            , LikeButtonProcessHand
            , PageMorphHand
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
import SongType
    exposing
        ( SongsRememberedIndex
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
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
        doNothing : ElmCycle
        doNothing =
            ( model
            , focusInputPossibly model
            )
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
                        , focusSetId id
                        , focusInputPossibly model
                        ]
                    )

                keyIs : Char -> Bool
                keyIs char =
                    Char.toCode char == keyCode

                songsRememberedIndex : SongsRememberedIndex
                songsRememberedIndex =
                    List.length model.songsRemembered
                        |> flip (-) 1
            in
            if keyIs 'H' then
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
