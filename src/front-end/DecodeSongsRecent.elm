{- Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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
