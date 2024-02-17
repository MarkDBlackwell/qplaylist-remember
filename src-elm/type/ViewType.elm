{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewType exposing
    ( Display
    , HoverText
    , Id
    , IdMaybe
    , KeyChar
    , KeyCode
    )

-- VIEW


type alias Display =
    String


type alias HoverText =
    String


type alias Id =
    String


type alias IdMaybe =
    Maybe Id


type alias KeyChar =
    String


type alias KeyCode =
    Int
