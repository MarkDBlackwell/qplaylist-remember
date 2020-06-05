{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UserIdentifier exposing
    ( cmdGenerateUserIdentifier
    , userIdentifierCalc
    , userIdentifierInit
    )

import Char
import ElmCycle
    exposing
        ( Msg(..)
        )
import Random
import UserIdentifierType
    exposing
        ( UserIdentifier
        , UserIdentifierNumberSpaceInt
        )
import Utilities
    exposing
        ( pred
        )
import ViewType
    exposing
        ( KeyCode
        )



-- MODEL


caseLength : Int
caseLength =
    Char.toCode 'A'
        |> pred
        |> (-) (Char.toCode 'Z')


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


cmdGenerateUserIdentifier : Cmd ElmCycle.Msg
cmdGenerateUserIdentifier =
    let
        highestCharNumber : UserIdentifierNumberSpaceInt
        highestCharNumber =
            charNumberSpaceLength
                ^ charCount
                |> pred
    in
    Random.int 0 highestCharNumber
        |> Random.generate MsgUserIdentifierEstablish


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
                    keyCode
                        |> modBy caseLength
            in
            keyCodeBase
                + slotInLetterCase
                |> Char.fromCode

        keyCodes : List KeyCode
        keyCodes =
            let
                keyCodeCalc : Int -> KeyCode
                keyCodeCalc index =
                    charNumberSpaceLength
                        ^ index
                        |> (//) userIdentifierNumberSpaceInt
                        |> modBy charNumberSpaceLength
            in
            charCount
                |> pred
                |> List.range 0
                |> List.map keyCodeCalc
    in
    keyCodes
        |> List.map keyCode2Char
        |> String.fromList
