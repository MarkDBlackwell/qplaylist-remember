{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module DecodeSongsRecent exposing (decodeSongsRecentResponse)

import AlertType
    exposing
        ( AlertMessageText
        )
import Json.Decode
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
    --http://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    --For decoding JSON:
    let
        asRecord : Result Json.Decode.Error SongsRecentWithDummyTag
        asRecord =
            let
                decodeSongsRecentWithDummyTag : Json.Decode.Decoder SongsRecentWithDummyTag
                decodeSongsRecentWithDummyTag =
                    let
                        decodeSongRecent : Json.Decode.Decoder SongRecent
                        decodeSongRecent =
                            Json.Decode.map4
                                SongRecent
                                (field2String "artist")
                                (field2String "time")
                                (field2String "timeStamp")
                                (field2String "title")

                        tag : String
                        tag =
                            "latestFive"
                    in
                    decodeSongRecent
                        |> Json.Decode.list
                        |> Json.Decode.field tag
                        |> Json.Decode.map SongsRecentWithDummyTag
            in
            jsonRawText
                |> Json.Decode.decodeString decodeSongsRecentWithDummyTag
    in
    case asRecord of
        Err error ->
            error
                |> Json.Decode.errorToString
                |> Err

        Ok record ->
            Ok record.dummyTag
