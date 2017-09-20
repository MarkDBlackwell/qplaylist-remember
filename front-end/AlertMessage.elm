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
        , alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextServerAwaiting
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


type alias AlertMessageTextErrorHttp =
    String


alertMessageTextInit : AlertMessageText
alertMessageTextInit =
    ""


alertMessageTextErrorHttp : Error -> ( AlertMessageTextErrorHttp, AlertMessageTextErrorHttp )
alertMessageTextErrorHttp httpError =
    let
        prefix : AlertMessageTextErrorHttp
        prefix =
            "HttpError"

        prefixColon : AlertMessageTextErrorHttp
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


alertMessageTextErrorHttpLogging : Error -> AlertMessageTextErrorHttp
alertMessageTextErrorHttpLogging httpError =
    first (alertMessageTextErrorHttp httpError)


alertMessageTextErrorHttpScreen : Error -> AlertMessageTextErrorHttp
alertMessageTextErrorHttpScreen httpError =
    second (alertMessageTextErrorHttp httpError)


alertMessageTextErrorUnexpected : AlertMessageText -> AlertMessageText -> AlertMessageText
alertMessageTextErrorUnexpected alertMessageText alertMessageTextDecode =
    "Unexpected error "
        ++ alertMessageText
        ++ ": "
        ++ alertMessageTextDecode


alertMessageTextRequestLikeOrComment : Error -> String -> AlertMessageText
alertMessageTextRequestLikeOrComment httpError likeOrCommentName =
    alertMessageTextErrorHttpScreen httpError
        ++ " (while attempting to send "
        ++ likeOrCommentName
        ++ " to server)"


alertMessageTextServerAwaiting : AlertMessageText
alertMessageTextServerAwaiting =
    "Awaiting server"
