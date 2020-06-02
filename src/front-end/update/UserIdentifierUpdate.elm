{- Copyright (C) 2020 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UserIdentifierUpdate exposing (userIdentifierEstablish)

import ElmCycle
import FocusUpdate
import ModelType
    exposing
        ( Model
        )
import UserIdentifier
import UserIdentifierType
    exposing
        ( UserIdentifierNumberSpaceInt
        )



-- UPDATE


userIdentifierEstablish : Model -> UserIdentifierNumberSpaceInt -> ElmCycle.ElmCycle
userIdentifierEstablish model randomInt =
    ( { model
        | userIdentifier =
            randomInt
                |> UserIdentifier.userIdentifierCalc
      }
    , Cmd.batch
        [ FocusUpdate.cmdFocusSetId "refresh"
        , FocusUpdate.cmdFocusInputPossibly model
        ]
    )
