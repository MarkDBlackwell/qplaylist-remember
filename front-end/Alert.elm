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
        ( ActionName
        , AlertMessageText
        , AlertMessageTextMaybe
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextLogging
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


type alias ActionName =
    String


type alias AlertMessageText =
    String


type alias AlertMessageTextMaybe =
    Maybe AlertMessageText


type alias DetailsText =
    String


type alias LikeOrCommentName =
    String


type alias PrefixSeparatorText =
    String


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



--alertMessageTextErrorHttpLogging : Error -> AlertMessageTextMaybe
--alertMessageTextErrorHttpLogging httpError =
--alertMessageTextErrorHttp httpError
--|> first
--|> Just


alertMessageTextLogging : Error -> AlertMessageText
alertMessageTextLogging httpError =
    alertMessageTextErrorHttp httpError
        |> first
        |> Just
        |> Maybe.withDefault ""


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


alertMessageTextSend : ActionName -> DetailsText -> AlertMessageText
alertMessageTextSend actionText detailsText =
    alertMessageTextErrorUnexpected
        [ "while attempting to " ++ actionText
        , detailsText
        ]


alertMessageTextServerAwaiting : AlertMessageTextMaybe
alertMessageTextServerAwaiting =
    Just "Awaiting server"


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "
