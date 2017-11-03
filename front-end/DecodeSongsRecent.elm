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


module DecodeSongsRecent
    exposing
        ( decodeSongsRecentResponse
        )

import AlertType
    exposing
        ( AlertMessageText
        )
import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , list
        , map
        , map4
        )
import SongType
    exposing
        ( SongRecent
        , SongsRecent
        )
import UpdateRequestType
    exposing
        ( HttpResponseText
        )
import Utilities
    exposing
        ( field2String
        )


-- UPDATE


type alias SongsRecentWithDummyTag =
    --TODO: Why do we need a tag?
    { dummyTag : SongsRecent }


decodeSongsRecentResponse : HttpResponseText -> Result AlertMessageText SongsRecent
decodeSongsRecentResponse jsonRawText =
    --See:
    --https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    --For decoding JSON:
    let
        asRecord : Result AlertMessageText SongsRecentWithDummyTag
        asRecord =
            let
                decodeSongsRecentWithDummyTag : Decoder SongsRecentWithDummyTag
                decodeSongsRecentWithDummyTag =
                    let
                        decodeSongRecent : Decoder SongRecent
                        decodeSongRecent =
                            map4
                                SongRecent
                                (field2String "artist")
                                (field2String "time")
                                (field2String "timeStamp")
                                (field2String "title")

                        tag : String
                        tag =
                            "latestFive"
                    in
                    list decodeSongRecent
                        |> field tag
                        |> map SongsRecentWithDummyTag
            in
            decodeString decodeSongsRecentWithDummyTag jsonRawText
    in
    case asRecord of
        Err text ->
            Err text

        Ok record ->
            Ok record.dummyTag
