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


port module SongPort
    exposing
        ( songsRememberedStore
        )

import MessageType
    exposing
        ( Msg
        )
import ModelType
    exposing
        ( Model
        )
import SongType
    exposing
        ( SongsRemembered
        )


-- UPDATE


port updateLocalStorage : SongsRemembered -> Cmd msg


songsRememberedStore : Model -> ( Model, Cmd Msg )
songsRememberedStore model =
    ( model
    , updateLocalStorage model.songsRemembered
    )
