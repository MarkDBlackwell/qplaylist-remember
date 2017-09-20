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


module AlertMessage
    exposing
        ( AlertMessageText
        , DecodeErrorMessageText
        , alertMessageTextAwaitingServer
        , alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextLikeOrCommentRequest
        )

import Http
    exposing
        ( Error
        )
import Tuple
    exposing
        ( first
        , second
        )


-- MODEL


type alias AlertMessageText =
    String


type alias DecodeErrorMessageText =
    String


type alias HttpErrorMessageText =
    String


alertMessageTextAwaitingServer : AlertMessageText
alertMessageTextAwaitingServer =
    "Awaiting server"


alertMessageTextInit : AlertMessageText
alertMessageTextInit =
    ""


alertMessageTextLikeOrCommentRequest : Error -> String -> AlertMessageText
alertMessageTextLikeOrCommentRequest httpError likeOrCommentName =
    alertMessageTextErrorHttpScreen httpError
        ++ " (while attempting to send "
        ++ likeOrCommentName
        ++ " to server)"


alertMessageTextErrorUnexpected : AlertMessageText -> DecodeErrorMessageText -> AlertMessageText
alertMessageTextErrorUnexpected alertMessageText decodeErrorMessageText =
    "Unexpected error "
        ++ alertMessageText
        ++ ": "
        ++ decodeErrorMessageText


alertMessageTextErrorHttp : Error -> ( HttpErrorMessageText, HttpErrorMessageText )
alertMessageTextErrorHttp httpError =
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


alertMessageTextErrorHttpLogging : Error -> HttpErrorMessageText
alertMessageTextErrorHttpLogging httpError =
    first (alertMessageTextErrorHttp httpError)


alertMessageTextErrorHttpScreen : Error -> HttpErrorMessageText
alertMessageTextErrorHttpScreen httpError =
    second (alertMessageTextErrorHttp httpError)
