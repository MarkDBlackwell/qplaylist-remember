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


module UpdateStateVector
    exposing
        ( stateVector
        )

import ModelType
    exposing
        ( Model
        )
import UpdateRequestType
    exposing
        ( AwaitingServerResponse
        )
import UpdateType
    exposing
        ( Optional
            ( Closed
            , Open
            )
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        )


-- UPDATE


stateVector : Model -> ( AwaitingServerResponse, Optional )
stateVector model =
    let
        commentOptional : Optional
        commentOptional =
            maybeMapWithDefault Closed (\_ -> Open) model.songCommentingMaybe
    in
    ( model.awaitingServerResponse
    , commentOptional
    )
