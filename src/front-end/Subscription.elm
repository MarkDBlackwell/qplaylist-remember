{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Subscription exposing (subscriptions)

import ElmCycle
    exposing
        ( Msg(..)
        )
import Keyboard
    exposing
        ( ups
        )
import ModelType
    exposing
        ( Model
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ups KeystrokeHand
        ]
