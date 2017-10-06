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
        , field
        , map
        , string
        )
import UpdateRequestType
    exposing
        ( HttpResponseText
        , LikeOrCommentResponseText
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
                    map
                        LikeOrCommentResponseWithDummyTag
                        (field tag string)
            in
            decodeString decodeResponse jsonRawText
    in
    case asRecord of
        Err text ->
            Err text

        Ok record ->
            Ok record.dummyTag
