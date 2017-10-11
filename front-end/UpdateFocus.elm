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
        ( focusAttempt
        , focusInputPossibly
        , focusSetId
        )

import Dom
    exposing
        ( Error
        , Id
        , focus
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( FocusAttempt
            , None
            )
        )
import ModelType
    exposing
        ( Model
        )
import Task
    exposing
        ( Task
        , attempt
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        , msg2Cmd
        )


-- UPDATE


focusAttempt : Model -> Id -> ElmCycle
focusAttempt model id =
    let
        focusOnId : Task Error ()
        focusOnId =
            focus id

        ignoreResult : Result x a -> Msg
        ignoreResult _ =
            None
    in
    --See:
    --https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
    --https://stackoverflow.com/a/39419640/1136063
    ( model
      --Here, unlike logging, executing the task requires the Elm runtime.
    , attempt ignoreResult focusOnId
    )


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    maybeMapWithDefault
        Cmd.none
        (\_ -> focusSetId "input")
        model.songCommentingMaybe


focusSetId : Id -> Cmd Msg
focusSetId id =
    FocusAttempt id
        |> msg2Cmd
