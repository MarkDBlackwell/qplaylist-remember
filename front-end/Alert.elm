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
        ( alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextSend
        , alertMessageTextServerAwaiting
        )

import AlertType
    exposing
        ( ActionDescription
        , AlertMessageText
        , AlertMessageTextMaybe
        , DetailsText
        , LikeOrCommentName
        , PrefixSeparatorText
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


alertMessageTextInit : AlertMessageTextMaybe
alertMessageTextInit =
    Nothing



-- UPDATE


alertMessageTextErrorHttp : Error -> ( AlertMessageText, AlertMessageText )
alertMessageTextErrorHttp httpError =
    let
        prefix : PrefixSeparatorText
        prefix =
            "HttpError"
    in
    case httpError of
        Http.BadPayload debuggingText httpResponseText ->
            ( prefix ++ prefixSeparator ++ "BadPayload"
            , debuggingText
            )

        Http.BadStatus httpResponseText ->
            ( prefixSeparator ++ "BadStatus"
            , toString httpResponseText.status
            )

        Http.BadUrl uriText ->
            ( prefixSeparator ++ "BadUrl"
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
            prefixSeparator
            alertMessageTextList
        )


alertMessageTextRequestLikeOrComment : Error -> LikeOrCommentName -> AlertMessageText
alertMessageTextRequestLikeOrComment httpError likeOrCommentName =
    (++)
        (alertMessageTextErrorHttpScreen httpError)
        (String.concat
            [ " (while attempting to send your "
            , likeOrCommentName
            , ")"
            ]
        )


alertMessageTextSend : ActionDescription -> DetailsText -> AlertMessageText
alertMessageTextSend actionDescription detailsText =
    alertMessageTextErrorUnexpected
        [ "while attempting to " ++ actionDescription
        , detailsText
        ]


alertMessageTextServerAwaiting : AlertMessageTextMaybe
alertMessageTextServerAwaiting =
    Just "Awaiting server"


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "
