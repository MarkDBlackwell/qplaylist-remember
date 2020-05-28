{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateFocus exposing
    ( focusAttempt
    , focusInputPossibly
    , focusSetId
    )

import Browser.Dom as Dom
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import ModelType
    exposing
        ( Model
        )
import Task
    exposing
        ( Task
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        , msg2Cmd
        )
import ViewType
    exposing
        ( Id
        )



-- UPDATE


focusAttempt : Model -> Id -> ElmCycle
focusAttempt model id =
    let
        focusOnId : Task Dom.Error ()
        focusOnId =
            Dom.focus id

        ignoreResult : Result x a -> Msg
        ignoreResult _ =
            None
    in
    --See:
    --http://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
    --http://stackoverflow.com/a/39419640/1136063
    --http://stackoverflow.com/questions/31901397/how-to-set-focus-on-an-element-in-elm
    ( model
      --Here, unlike logging, executing the task requires the Elm runtime.
    , Task.attempt ignoreResult focusOnId
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
