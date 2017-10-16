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
