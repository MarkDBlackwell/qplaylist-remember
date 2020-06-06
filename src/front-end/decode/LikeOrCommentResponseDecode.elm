{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module LikeOrCommentResponseDecode exposing (decodeLikeOrCommentResponse)

import AlertType
    exposing
        ( AlertMessageText
        )
import Json.Decode
import RequestUpdateType
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
        asRecord : Result Json.Decode.Error LikeOrCommentResponseWithDummyTag
        asRecord =
            let
                decodeResponse : Json.Decode.Decoder LikeOrCommentResponseWithDummyTag
                decodeResponse =
                    let
                        tag : String
                        tag =
                            "response"
                    in
                    tag
                        |> field2String
                        |> Json.Decode.map
                            LikeOrCommentResponseWithDummyTag
            in
            jsonRawText
                |> Json.Decode.decodeString
                    decodeResponse
    in
    case asRecord of
        Err error ->
            error
                |> Json.Decode.errorToString
                |> Err

        Ok record ->
            Ok record.dummyTag
