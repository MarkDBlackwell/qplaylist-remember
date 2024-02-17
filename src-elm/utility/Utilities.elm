{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Utilities exposing
    ( attributeIdFromMaybe
    , attributesEmpty
    , cmdMsg2Cmd
    , field2String
    , goldenRatio
    , htmlNodeNull
    , idMorphString
    , idRefreshString
    , innerHtmlEmpty
    , matchingIndexes
    , pred
    , succ
    )

import ElmCycle
import Html
import Html.Attributes
import Json.Decode
import Task
import ViewType
    exposing
        ( Id
        , IdMaybe
        )


field2String : String -> Json.Decode.Decoder String
field2String text =
    Json.Decode.string
        |> Json.Decode.field
            text


matchingIndexes : List a -> a -> List Int
matchingIndexes listOfThings thing =
    let
        matchWithIndexMaybe : Int -> a -> Maybe Int
        matchWithIndexMaybe index variable =
            if thing /= variable then
                Nothing

            else
                Just index
    in
    listOfThings
        |> List.indexedMap matchWithIndexMaybe
        |> List.filterMap identity


pred : Int -> Int
pred x =
    --Predecessor
    x - 1


succ : Int -> Int
succ x =
    --Successor
    x + 1



-- UPDATE


cmdMsg2Cmd : ElmCycle.Msg -> Cmd ElmCycle.Msg
cmdMsg2Cmd msg =
    --See:
    --  http://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    msg
        |> Task.succeed
        |> Task.perform
            identity



-- VIEW


attributeIdFromMaybe : IdMaybe -> List (Html.Attribute ElmCycle.Msg)
attributeIdFromMaybe attributeIdMaybe =
    case attributeIdMaybe of
        Nothing ->
            attributesEmpty

        Just x ->
            [ Html.Attributes.id x ]


attributesEmpty : List (Html.Attribute ElmCycle.Msg)
attributesEmpty =
    []


goldenRatio : Float
goldenRatio =
    --See:
    --  http://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html.Html ElmCycle.Msg
htmlNodeNull =
    Html.text ""


idMorphString : Id
idMorphString =
    "morph"


idRefreshString : Id
idRefreshString =
    "refresh"


innerHtmlEmpty : List (Html.Html ElmCycle.Msg)
innerHtmlEmpty =
    []
