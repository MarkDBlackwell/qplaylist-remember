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


module UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        , updateFocusResult
        , updateFocusSet
        )

import Dom
    exposing
        ( Id
        , focus
        )
import MessageType
    exposing
        ( Msg
            ( FocusResult
            , FocusSet
            )
        )
import ModelType
    exposing
        ( Model
        )
import Task
    exposing
        ( attempt
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    case model.songCommenting of
        Nothing ->
            Cmd.none

        _ ->
            focusSetId "input"


focusSetId : Id -> Cmd Msg
focusSetId id =
    msg2Cmd (FocusSet id)


updateFocusResult : Model -> ( Model, Cmd Msg )
updateFocusResult model =
    ( model
    , Cmd.none
    )


updateFocusSet : Model -> Id -> ( Model, Cmd Msg )
updateFocusSet model id =
    --See:
    --https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
    --https://stackoverflow.com/a/39419640/1136063
    ( model
    , attempt FocusResult (focus id)
    )
