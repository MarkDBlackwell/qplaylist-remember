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
        , relative
        )

import Dom exposing (Id)
import Http exposing (Error)
import MessageDetails exposing (Msg(FocusSet))
import ModelDetailsUpdate
    exposing
        ( HttpErrorMessageText
        , QueryBeforeList
        , QueryPair
        , QueryPairs
        , UriText
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


relative : QueryBeforeList -> QueryPairs -> UriText
relative queryBeforeList queryPairs =
    --See:
    --https://github.com/elm-lang/http/issues/10
    --https://github.com/elm-lang/url
    --https://github.com/evancz/elm-http
    --http://package.elm-lang.org/packages/elm-lang/http/latest
    --TODO: When elm-lang/url is updated to contain 'relative',
    --consider replacing this code:
    let
        escapeAll : UriText -> UriText
        escapeAll string =
            --See:
            --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
            --TODO: Possibly, use Http.encodeUri instead:
            escapeHashes (escapeEqualsSigns (escapeAmpersands string))

        escapeAmpersands : UriText -> UriText
        escapeAmpersands string =
            String.join
                "%26"
                (String.split "&" string)

        escapeEqualsSigns : UriText -> UriText
        escapeEqualsSigns string =
            String.join
                "%3D"
                (String.split "=" string)

        escapeHashes : UriText -> UriText
        escapeHashes string =
            String.join
                "%23"
                (String.split "#" string)

        query : UriText
        query =
            String.join
                "&"
                (List.map queryPairJoin queryPairs)

        queryBefore : UriText
        queryBefore =
            String.join
                "/"
                queryBeforeList

        queryPairJoin : QueryPair -> UriText
        queryPairJoin ( name, value ) =
            String.join
                "="
                [ name
                , escapeAll value
                ]
    in
    queryBefore ++ "?" ++ query
