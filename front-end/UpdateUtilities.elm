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


module UpdateUtilities
    exposing
        ( focusSet
        , msg2Cmd
        )

import Dom
    exposing
        ( Id
        )
import MessageType
    exposing
        ( Msg
            ( FocusSet
            )
        )
import Task
    exposing
        ( Task
        , perform
        , succeed
        )


-- UPDATE


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (FocusSet id)


msg2Cmd : Msg -> Cmd Msg
msg2Cmd msg =
    --See:
    --https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    perform identity (succeed msg)
