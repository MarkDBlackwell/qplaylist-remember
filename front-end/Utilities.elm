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


module Utilities
    exposing
        ( buttonIdCreate
        , goldenRatio
        , htmlNodeNull
        , indexes
        , matchingIndexes
        , maybeDefaultNothing
        , maybeMapWithDefault
        , msg2Cmd
        , selectOneMaybe
        , songGroup2String
        , startingWith
        , withIndexes
        , withoutOne
        )

import Dom
    exposing
        ( Id
        )
import Html
    exposing
        ( Html
        , text
        )
import MessageType
    exposing
        ( Msg
        )
import SongType
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        )
import Task
    exposing
        ( perform
        , succeed
        )


-- UPDATE


buttonIdCreate : Id -> Int -> Id
buttonIdCreate idFragment index =
    String.concat
        [ "button"
        , idFragment
        , toString index
        ]


indexes : List a -> List Int
indexes listA =
    List.length listA
        |> flip (-) 1
        |> List.range 0


matchingIndexes : List a -> a -> List Int
matchingIndexes listA a =
    let
        matchWithIndexMaybe : ( Int, a ) -> Maybe Int
        matchWithIndexMaybe ( index, another ) =
            if another == a then
                Just index
            else
                Nothing
    in
    withIndexes listA
        |> List.filterMap matchWithIndexMaybe


maybeDefaultNothing : (a -> Maybe b) -> Maybe a -> Maybe b
maybeDefaultNothing function value =
    maybeMapWithDefault Nothing function value


maybeMapWithDefault : a -> (b -> a) -> Maybe b -> a
maybeMapWithDefault default function value =
    Maybe.map function value
        |> Maybe.withDefault default


msg2Cmd : Msg -> Cmd Msg
msg2Cmd msg =
    --See:
    --https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    succeed msg
        |> perform identity


selectOneMaybe : List a -> Int -> Maybe a
selectOneMaybe listA index =
    startingWith listA index
        |> List.head


startingWith : List a -> Int -> List a
startingWith listA index =
    List.drop index listA


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    indexes listA
        |> List.map2 (flip (,)) listA


withoutOne : List a -> Int -> List a
withoutOne listA index =
    (index + 1)
        |> startingWith listA
        |> (++) (List.take index listA)



-- VIEW


goldenRatio : Float
goldenRatio =
    --See:
    --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"
