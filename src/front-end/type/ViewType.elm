{- Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewType
    exposing
        ( Display
        , HoverText
        , IdMaybe
        )

import Dom
    exposing
        ( Id
        )


-- VIEW


type alias Display =
    String


type alias HoverText =
    String


type alias IdMaybe =
    Maybe Id
