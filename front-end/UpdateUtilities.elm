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
        , httpErrorMessageLogging
        , httpErrorMessageScreen
        , msg2Cmd
        , songBasic2SongRemembered
        , songRemembered2SongBasic
        )

import Dom
    exposing
        ( Id
        )
import Http
    exposing
        ( Error
        )
import MessageDetails
    exposing
        ( Msg
            ( FocusSet
            )
        )
import ModelDetailsUpdate
    exposing
        ( HttpErrorMessageText
        )
import ModelInitialize
    exposing
        ( likedOrCommentedInit
        )
import Song
    exposing
        ( SongBasic
        , SongRemembered
        , SongsBasic
        )
import Task
    exposing
        ( Task
        , perform
        , succeed
        )
import Tuple
    exposing
        ( first
        , second
        )


-- UPDATE


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (succeed (FocusSet id))


httpErrorMessage : Error -> ( HttpErrorMessageText, HttpErrorMessageText )
httpErrorMessage httpError =
    let
        prefix : HttpErrorMessageText
        prefix =
            "HttpError"

        prefixColon : HttpErrorMessageText
        prefixColon =
            prefix ++ ": "
    in
    case httpError of
        Http.BadPayload debuggingText httpResponseText ->
            ( prefixColon ++ "BadPayload"
            , debuggingText
            )

        Http.BadStatus httpResponseText ->
            ( prefixColon ++ "BadStatus"
            , toString httpResponseText.status
            )

        Http.BadUrl uriText ->
            ( prefixColon ++ "BadUrl"
            , uriText
            )

        Http.NetworkError ->
            ( prefix
            , "NetworkError"
            )

        Http.Timeout ->
            ( prefix
            , "Timeout"
            )


httpErrorMessageLogging : Error -> HttpErrorMessageText
httpErrorMessageLogging httpError =
    first (httpErrorMessage httpError)


httpErrorMessageScreen : Error -> HttpErrorMessageText
httpErrorMessageScreen httpError =
    second (httpErrorMessage httpError)


msg2Cmd : Task Never msg -> Cmd msg
msg2Cmd msg =
    --TODO: All calls to this include 'succeed'; so, refactor that out.
    --See:
    --https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    perform identity msg


songBasic2SongRemembered : SongBasic -> SongRemembered
songBasic2SongRemembered song =
    SongRemembered
        song.artist
        likedOrCommentedInit
        song.time
        song.timestamp
        song.title


songRemembered2SongBasic : SongRemembered -> SongBasic
songRemembered2SongBasic song =
    SongBasic
        song.artist
        song.time
        song.timestamp
        song.title
