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


module Update exposing (..)

import Dom
    exposing
        ( Id
        )
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
import Msgs exposing (..)
import Task
    exposing
        ( perform
        , succeed
        )


-- UPDATE


type alias Artist =
    String


type alias DecodeErrorMessageText =
    String


type alias SongLatestFew =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsLatestFewTagged =
    { latestFew : SongsLatestFew }


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String


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


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (succeed (FocusSet id))


msg2Cmd : Task.Task Never msg -> Cmd msg
msg2Cmd msg =
    --See:
    --https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    Task.perform identity msg
