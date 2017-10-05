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


module Alert
    exposing
        ( AlertMessageText
        , alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextSend
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


alertMessageTextInit : AlertMessageText
alertMessageTextInit =
    ""



-- UPDATE


alertMessageTextErrorHttp : Error -> ( AlertMessageText, AlertMessageText )
alertMessageTextErrorHttp httpError =
    let
        prefix : AlertMessageText
        prefix =
            "HttpError"
    in
    case httpError of
        Http.BadPayload debuggingText httpResponseText ->
            ( prefix ++ prefixColon ++ "BadPayload"
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


alertMessageTextErrorHttpLogging : Error -> AlertMessageText
alertMessageTextErrorHttpLogging httpError =
    alertMessageTextErrorHttp httpError
        |> first


alertMessageTextErrorHttpScreen : Error -> AlertMessageText
alertMessageTextErrorHttpScreen httpError =
    alertMessageTextErrorHttp httpError
        |> second


alertMessageTextErrorUnexpected : List AlertMessageText -> AlertMessageText
alertMessageTextErrorUnexpected alertMessageTextList =
    (++)
        "Unexpected error "
        (String.join
            prefixColon
            alertMessageTextList
        )


alertMessageTextRequestLikeOrComment : Error -> String -> AlertMessageText
alertMessageTextRequestLikeOrComment httpError likeOrCommentName =
    (++)
        (alertMessageTextErrorHttpScreen httpError)
        (String.concat
            [ " (while attempting to send your "
            , likeOrCommentName
            , ")"
            ]
        )


alertMessageTextSend : AlertMessageText -> AlertMessageText -> AlertMessageText
alertMessageTextSend action details =
    alertMessageTextErrorUnexpected
        [ "while attempting to " ++ action
        , details
        ]


alertMessageTextServerAwaiting : AlertMessageText
alertMessageTextServerAwaiting =
    "Awaiting server"


prefixColon : AlertMessageText
prefixColon =
    ": "
