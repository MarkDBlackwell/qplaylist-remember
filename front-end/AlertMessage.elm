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
        , alertMessageErrorHttpLogging
        , alertMessageErrorHttpScreen
        , alertMessageTextAwaitingServer
        , alertMessageTextInit
        , alertMessageTextLikeOrCommentRequest
        , alertMessageTextUnexpectedError
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
    alertMessageErrorHttpScreen httpError
        ++ " (while attempting to send "
        ++ likeOrCommentName
        ++ " to server)"


alertMessageTextUnexpectedError : AlertMessageText -> DecodeErrorMessageText -> AlertMessageText
alertMessageTextUnexpectedError alertMessageText decodeErrorMessageText =
    "Unexpected error "
        ++ alertMessageText
        ++ ": "
        ++ decodeErrorMessageText


alertMessageErrorHttp : Error -> ( HttpErrorMessageText, HttpErrorMessageText )
alertMessageErrorHttp httpError =
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


alertMessageErrorHttpLogging : Error -> HttpErrorMessageText
alertMessageErrorHttpLogging httpError =
    first (alertMessageErrorHttp httpError)


alertMessageErrorHttpScreen : Error -> HttpErrorMessageText
alertMessageErrorHttpScreen httpError =
    second (alertMessageErrorHttp httpError)
