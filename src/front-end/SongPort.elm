{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


port module SongPort exposing (songsRememberedStore)

import ElmCycle
import ModelType
    exposing
        ( Model
        )
import SongType
    exposing
        ( SongsRemembered
        )



-- UPDATE


port cmdUpdateLocalStorage : SongsRemembered -> Cmd msg


songsRememberedStore : Model -> ElmCycle.ElmCycle
songsRememberedStore model =
    ( model
    , model.songsRemembered
        |> cmdUpdateLocalStorage
    )
