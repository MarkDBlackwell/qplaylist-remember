{- Copyright (C) 2017 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module DecodeLikeOrCommentResponse
    exposing
        ( decodeLikeOrCommentResponse
        )

import AlertType
    exposing
        ( AlertMessageText
        )
import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , map
        )
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
        asRecord : Result AlertMessageText LikeOrCommentResponseWithDummyTag
        asRecord =
            let
                decodeResponse : Decoder LikeOrCommentResponseWithDummyTag
                decodeResponse =
                    let
                        tag : String
                        tag =
                            "response"
                    in
                    field2String tag
                        |> map LikeOrCommentResponseWithDummyTag
            in
            decodeString decodeResponse jsonRawText
    in
    case asRecord of
        Err text ->
            Err text

        Ok record ->
            Ok record.dummyTag
