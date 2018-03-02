{- Copyright (C) 2017 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UserIdentifier
    exposing
        ( generateUserIdentifier
        , userIdentifierCalc
        , userIdentifierInit
        )

import Char
    exposing
        ( KeyCode
        , fromCode
        , toCode
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( UserIdentifierEstablish
            )
        )
import Random
    exposing
        ( Generator
        , generate
        , int
        )
import UserIdentifierType
    exposing
        ( UserIdentifier
        , UserIdentifierNumberSpaceInt
        )


-- MODEL


caseLength : Int
caseLength =
    1 + toCode 'Z' - toCode 'A'


charCount : Int
charCount =
    3


charNumberSpaceLength : Int
charNumberSpaceLength =
    let
        caseCount : Int
        caseCount =
            --Upper and lower case (letters).
            2
    in
    caseCount * caseLength


generateUserIdentifier : Cmd Msg
generateUserIdentifier =
    let
        highestCharNumber : UserIdentifierNumberSpaceInt
        highestCharNumber =
            (charNumberSpaceLength ^ charCount)
                |> flip (-) 1
    in
    int 0 highestCharNumber
        |> generate UserIdentifierEstablish


userIdentifierInit : UserIdentifier
userIdentifierInit =
    ""



-- UPDATE


userIdentifierCalc : UserIdentifierNumberSpaceInt -> UserIdentifier
userIdentifierCalc userIdentifierNumberSpaceInt =
    let
        keyCode2Char : KeyCode -> Char
        keyCode2Char keyCode =
            let
                keyCodeBase : KeyCode
                keyCodeBase =
                    if keyCode < caseLength then
                        toCode 'A'
                    else
                        toCode 'a'

                slotInLetterCase : KeyCode
                slotInLetterCase =
                    keyCode % caseLength
            in
            (keyCodeBase + slotInLetterCase)
                |> fromCode

        keyCodes : List KeyCode
        keyCodes =
            let
                keyCodeCalc : Int -> KeyCode
                keyCodeCalc index =
                    (charNumberSpaceLength ^ index)
                        |> (//) userIdentifierNumberSpaceInt
                        |> flip (%) charNumberSpaceLength
            in
            (charCount - 1)
                |> List.range 0
                |> List.map keyCodeCalc
    in
    List.map keyCode2Char keyCodes
        |> String.fromList
