{- Copyright (C) 2017 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module AlertType
    exposing
        ( ActionDescription
        , AlertMessageText
        , AlertMessageTextList
        , AlertMessageTextMaybe
        , DetailsText
        , LikeOrCommentName
        , PrefixSeparatorText
        )

-- MODEL


type alias ActionDescription =
    String


type alias AlertMessageText =
    String


type alias AlertMessageTextList =
    List AlertMessageText


type alias AlertMessageTextMaybe =
    Maybe AlertMessageText


type alias DetailsText =
    String


type alias LikeOrCommentName =
    String


type alias PrefixSeparatorText =
    String
