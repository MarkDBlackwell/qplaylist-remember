{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UserIdentifier exposing
    ( generateUserIdentifier
    , userIdentifierCalc
    , userIdentifierInit
    )

import Char
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import Random
import UserIdentifierType
    exposing
        ( UserIdentifier
        , UserIdentifierNumberSpaceInt
        )
import ViewType
    exposing
        ( KeyCode
        )



-- MODEL


caseLength : Int
caseLength =
    1 + Char.toCode 'Z' - Char.toCode 'A'


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
                |> (\a -> (-) a 1)
    in
    Random.int 0 highestCharNumber
        |> Random.generate UserIdentifierEstablish


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
                        Char.toCode 'A'

                    else
                        Char.toCode 'a'

                slotInLetterCase : KeyCode
                slotInLetterCase =
                    modBy caseLength keyCode
            in
            (keyCodeBase + slotInLetterCase)
                |> Char.fromCode

        keyCodes : List KeyCode
        keyCodes =
            let
                keyCodeCalc : Int -> KeyCode
                keyCodeCalc index =
                    (charNumberSpaceLength ^ index)
                        |> (//) userIdentifierNumberSpaceInt
                        |> (\a -> (\dividend modulus -> modBy modulus dividend) a charNumberSpaceLength)
            in
            (charCount - 1)
                |> List.range 0
                |> List.map keyCodeCalc
    in
    List.map keyCode2Char keyCodes
        |> String.fromList
