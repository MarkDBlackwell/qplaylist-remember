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


module DecodeSongsBasic exposing (decodeSongsBasic)

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
        ( SongBasic
        , SongsBasic
        )
import ModelDetailsUpdate
    exposing
        ( DecodeErrorMessageText
        , HttpResponseText
        )


-- UPDATE


type alias SongsBasicWithDummyTag =
    --TODO: Why do we need a tag?
    { dummyTag : SongsBasic }


decodeSongsBasic : HttpResponseText -> SongsBasic
decodeSongsBasic jsonRawText =
    --See:
    --https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    --For decoding JSON:
    let
        asRecord : Result DecodeErrorMessageText SongsBasicWithDummyTag
        asRecord =
            decodeString decodeSongsBasicWithDummyTag jsonRawText

        decodeSongBasic : Decoder SongBasic
        decodeSongBasic =
            map4 SongBasic
                (field "artist" string)
                (field "time" string)
                (field "timeStamp" string)
                (field "title" string)

        decodeSongsBasicWithDummyTag : Decoder SongsBasicWithDummyTag
        decodeSongsBasicWithDummyTag =
            map SongsBasicWithDummyTag
                (field tag (list decodeSongBasic))

        tag : String
        tag =
            "latestFive"
    in
    case asRecord of
        Err _ ->
            []

        Ok record ->
            record.dummyTag