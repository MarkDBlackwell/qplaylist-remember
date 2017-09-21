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


module ModelInitialize
    exposing
        ( awaitingServerResponseInit
        , commentTextInit
        , init
        )

import Alert
    exposing
        ( alertMessageTextInit
        )
import Message
    exposing
        ( Msg
            ( InitialSetUp
            )
        )
import Model
    exposing
        ( AwaitingServerResponse
        , CommentText
        , Model
        , PageIsExpanded
        )
import Random
    exposing
        ( generate
        )
import Song
    exposing
        ( songCommentingInit
        , songLikingInit
        , songsLatestFewInit
        , songsRememberedInit
        )
import UserIdentifier
    exposing
        ( letterSpace
        , userIdentifierInit
        )


-- MODEL


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentTextInit : CommentText
commentTextInit =
    ""


init : ( Model, Cmd Msg )
init =
    let
        threeLetterSpaceHighest : Int
        threeLetterSpaceHighest =
            (letterSpace ^ 3) - 1
    in
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        songCommentingInit
        songLikingInit
        songsLatestFewInit
        songsRememberedInit
        userIdentifierInit
    , generate InitialSetUp (Random.int 0 threeLetterSpaceHighest)
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
