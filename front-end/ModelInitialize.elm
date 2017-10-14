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
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( InitialSetUp
            )
        )
import ModelType
    exposing
        ( CommentText
        , Flags
        , Model
        , PageIsExpanded
        )
import Random
    exposing
        ( generate
        )
import SongInitialize
    exposing
        ( songCommentingMaybeInit
        , songLikingMaybeInit
        , songsRecentInit
        )
import UpdateRequestType
    exposing
        ( AwaitingServerResponse
        )
import UserIdentifier
    exposing
        ( threeLetterNumberSpaceIntRandom
        , userIdentifierInit
        )


-- MODEL


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentTextInit : CommentText
commentTextInit =
    ""


init : Flags -> ElmCycle
init flags =
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        flags.showCommentButtons
        songCommentingMaybeInit
        songLikingMaybeInit
        songsRecentInit
        flags.songsRemembered
        userIdentifierInit
    , generate InitialSetUp threeLetterNumberSpaceIntRandom
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
