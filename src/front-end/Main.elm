{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Main exposing (main)

import Browser
import ElmCycle
    exposing
        ( Msg
        )
import ModelInitialize
import ModelType
    exposing
        ( Flags
        , Model
        )
import Subscription
import Update
import View



-- MODEL


main : Program Flags Model Msg
main =
    { init = ModelInitialize.init
    , subscriptions = Subscription.subscriptions
    , update = Update.update
    , view = View.view
    }
        |> Browser.element
