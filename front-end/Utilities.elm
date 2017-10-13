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
        ( attributeIdFromMaybe
        , attributesEmpty
        , field2String
        , goldenRatio
        , htmlNodeNull
        , indexes
        , innerHtmlEmpty
        , matchingIndexes
        , maybeDefaultNothing
        , maybeMapWithDefault
        , msg2Cmd
        , prefixSeparator
        , selectOneMaybe
        , startingWith
        , withIndexes
        , withoutOne
        )

import AlertType
    exposing
        ( PrefixSeparatorText
        )
import ElmCycle
    exposing
        ( Msg
        )
import Html
    exposing
        ( Attribute
        , Html
        , text
        )
import Html.Attributes
    exposing
        ( id
        )
import Json.Decode
    exposing
        ( Decoder
        , field
        , string
        )
import Task
    exposing
        ( perform
        , succeed
        )
import ViewType
    exposing
        ( IdMaybe
        )


-- UPDATE


field2String : String -> Decoder String
field2String text =
    field text string


indexes : List a -> List Int
indexes listA =
    List.length listA
        |> flip (-) 1
        |> List.range 0


matchingIndexes : List a -> a -> List Int
matchingIndexes listA thing =
    let
        matchWithIndexMaybe : ( Int, a ) -> Maybe Int
        matchWithIndexMaybe ( index, variable ) =
            if thing /= variable then
                Nothing
            else
                Just index
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


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "


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


attributeIdFromMaybe : IdMaybe -> List (Attribute msg)
attributeIdFromMaybe attributeIdMaybe =
    maybeMapWithDefault
        attributesEmpty
        (\x -> [ id x ])
        attributeIdMaybe


attributesEmpty : List (Attribute msg)
attributesEmpty =
    []


goldenRatio : Float
goldenRatio =
    --See:
    --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


innerHtmlEmpty : List (Html msg)
innerHtmlEmpty =
    []
