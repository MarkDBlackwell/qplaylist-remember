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


module Initialize
    exposing
        ( Flags
        , awaitingServerResponseInit
        , commentTextInit
        , init
        )

import Alert
    exposing
        ( alertMessageTextInit
        )
import MessageType
    exposing
        ( Msg
            ( InitialSetUp
            )
        )
import ModelType
    exposing
        ( CommentText
        , Model
        , PageIsExpanded
        , ShowCommentButtons
        )
import Random
    exposing
        ( generate
        )
import Request
    exposing
        ( AwaitingServerResponse
        )
import Song
    exposing
        ( SongsRemembered
        , songCommentingInit
        , songLikingInit
        , songsLatestInit
        )
import UserIdentifier
    exposing
        ( letterSpace
        , userIdentifierInit
        )


-- MODEL


type alias Flags =
    { showCommentButtons : ShowCommentButtons
    , songsRemembered : SongsRemembered
    }


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentTextInit : CommentText
commentTextInit =
    ""


init : Flags -> ( Model, Cmd Msg )
init flags =
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
        flags.showCommentButtons
        songCommentingInit
        songLikingInit
        songsLatestInit
        flags.songsRemembered
        userIdentifierInit
    , generate InitialSetUp (Random.int 0 threeLetterSpaceHighest)
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
