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


module ModelDetailsUpdate
    exposing
        ( AlertMessageOptional
        , DecodeErrorMessageText
        , HttpErrorMessageText
        , HttpRequestOrResponseText
        , HttpRequestText
        , HttpResponseText
        , LikeText
        , QueryBeforeList
        , QueryPair
        , QueryPairs
        , RequestOrResponse
        , SongLatestFewIndex
        , SongRememberedIndex
        , UriText
        )

import ModelDetails
    exposing
        ( Optional
            ( Closed
            , Open
            )
        , SongsLatestFew
        )


-- UPDATE


type alias AlertMessageOptional =
    Optional


type alias DecodeErrorMessageText =
    String


type alias HttpErrorMessageText =
    String


type alias HttpRequestText =
    String


type alias HttpRequestOrResponseText =
    String


type alias HttpResponseText =
    String


type alias LikeText =
    String


type alias QueryBeforeList =
    --See:
    --https://github.com/elm-lang/url
    --https://tools.ietf.org/html/rfc3986
    --When joined, then comprises a URI's scheme, authority, and path:
    List UriText


type alias QueryPair =
    ( UriText, UriText )


type alias QueryPairs =
    List QueryPair


type alias RequestOrResponse =
    String


type alias SongLatestFewIndex =
    Int


type alias SongRememberedIndex =
    Int


type alias UriText =
    String
