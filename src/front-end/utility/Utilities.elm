{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Utilities exposing
    ( attributeIdFromMaybe
    , attributesEmpty
    , field2String
    , goldenRatio
    , htmlNodeNull
    , innerHtmlEmpty
    , matchingIndexes
    , maybeMapWithDefault
    , msg2Cmd
    , pred
    , prefixSeparator
    , selectOneFromIndexMaybe
    , startingWithFromIndex
    , succ
    , withoutOneFromMaybe
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
import Json.Decode as Json
import Task
import ViewType
    exposing
        ( IdMaybe
        )



-- UPDATE


field2String : String -> Json.Decoder String
field2String text =
    Json.field text Json.string


indexes : List a -> List Int
indexes listA =
    listA
        |> List.length
        |> pred
        |> List.range 0



{- TODO: try List.indexedMap -}


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
    listA
        |> withIndexes
        |> List.filterMap matchWithIndexMaybe


maybeMapWithDefault : a -> (b -> a) -> Maybe b -> a
maybeMapWithDefault default function value =
    value
        |> Maybe.map function
        |> Maybe.withDefault default


msg2Cmd : Msg -> Cmd Msg
msg2Cmd msg =
    --See:
    --http://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    msg
        |> Task.succeed
        |> Task.perform identity


pred : Int -> Int
pred x =
    --Predecessor
    x - 1


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "


selectOneFromIndexMaybe : List a -> Int -> Maybe a
selectOneFromIndexMaybe listA index =
    index
        |> startingWithFromIndex listA
        |> List.head


startingWithFromIndex : List a -> Int -> List a
startingWithFromIndex listA index =
    listA
        |> List.drop index


succ : Int -> Int
succ x =
    --Successor
    x + 1


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    --TODO: try List.indexedMap
    listA
        |> indexes
        |> List.map2 (\thing index -> ( index, thing )) listA


withoutOne : List a -> a -> List a
withoutOne listA thing =
    listA
        |> List.filter ((/=) thing)


withoutOneFromMaybe : List a -> Maybe a -> List a
withoutOneFromMaybe listA xMaybe =
    listA
        |> withoutOne
        |> (\thing -> Maybe.map thing xMaybe)
        |> Maybe.withDefault listA



-- VIEW


attributeIdFromMaybe : IdMaybe -> List (Attribute msg)
attributeIdFromMaybe attributeIdMaybe =
    maybeMapWithDefault
        attributesEmpty
        (\thing -> [ id thing ])
        attributeIdMaybe


attributesEmpty : List (Attribute msg)
attributesEmpty =
    []


goldenRatio : Float
goldenRatio =
    --See:
    --http://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


innerHtmlEmpty : List (Html msg)
innerHtmlEmpty =
    []
