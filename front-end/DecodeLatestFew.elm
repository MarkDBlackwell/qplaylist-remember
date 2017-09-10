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


module DecodeLatestFew exposing (decodeSongsLatestFew)

import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , list
        , map
        , map4
        , string
        )
import ModelDetails
    exposing
        ( SongLatestFew
        , SongsLatestFew
        )
import ModelDetailsUpdate
    exposing
        ( DecodeErrorMessageText
        , HttpResponseText
        , SongsLatestFewTagged
        )


-- UPDATE


decodeSongsLatestFew : HttpResponseText -> SongsLatestFew
decodeSongsLatestFew jsonRawText =
    --See:
    --https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    --For decoding JSON:
    let
        decodeSong : Decoder SongLatestFew
        decodeSong =
            map4 SongLatestFew
                (field "artist" string)
                (field "time" string)
                (field "timeStamp" string)
                (field "title" string)

        tagged2Record : Decoder SongsLatestFewTagged
        tagged2Record =
            map SongsLatestFewTagged
                (field "latestFive" (list decodeSong))

        tryRecord : Result DecodeErrorMessageText SongsLatestFewTagged
        tryRecord =
            decodeString tagged2Record jsonRawText
    in
    case tryRecord of
        Err _ ->
            []

        Ok record ->
            record.latestFew