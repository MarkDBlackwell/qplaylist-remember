{- Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Utilities
    exposing
        ( attributeIdFromMaybe
        , attributesEmpty
        , field2String
        , goldenRatio
        , htmlNodeNull
        , innerHtmlEmpty
        , matchingIndexes
        , maybeMapWithDefault
        , msg2Cmd
        , prefixSeparator
        , selectOneFromIndexMaybe
        , startingWithFromIndex
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


selectOneFromIndexMaybe : List a -> Int -> Maybe a
selectOneFromIndexMaybe listA index =
    startingWithFromIndex listA index
        |> List.head


startingWithFromIndex : List a -> Int -> List a
startingWithFromIndex listA index =
    List.drop index listA


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    indexes listA
        |> List.map2 (flip (,)) listA


withoutOne : List a -> a -> List a
withoutOne listA x =
    List.filter ((/=) x) listA


withoutOneFromMaybe : List a -> Maybe a -> List a
withoutOneFromMaybe listA xMaybe =
    withoutOne listA
        |> flip Maybe.map xMaybe
        |> Maybe.withDefault listA



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
