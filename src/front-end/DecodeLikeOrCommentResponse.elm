{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module DecodeLikeOrCommentResponse exposing (decodeLikeOrCommentResponse)

import AlertType
    exposing
        ( AlertMessageText
        )
import Json.Decode as Json
import UpdateRequestType
    exposing
        ( HttpResponseText
        , LikeOrCommentResponseText
        )
import Utilities
    exposing
        ( field2String
        )



-- UPDATE


type alias LikeOrCommentResponseWithDummyTag =
    --TODO: Why do we need a tag?
    { dummyTag : LikeOrCommentResponseText }


decodeLikeOrCommentResponse : HttpResponseText -> Result AlertMessageText LikeOrCommentResponseText
decodeLikeOrCommentResponse jsonRawText =
    --For decoding JSON:
    let
        asRecord : Result Json.Error LikeOrCommentResponseWithDummyTag
        asRecord =
            let
                decodeResponse : Json.Decoder LikeOrCommentResponseWithDummyTag
                decodeResponse =
                    let
                        tag : String
                        tag =
                            "response"
                    in
                    field2String tag
                        |> Json.map LikeOrCommentResponseWithDummyTag
            in
            Json.decodeString decodeResponse jsonRawText
    in
    case asRecord of
        Err error ->
            Err (Json.errorToString error)

        Ok record ->
            Ok record.dummyTag
