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
        ( threeLetterNumberSpaceIntRandom
        , updateInitialSetUp
        , userIdentifierInit
        )

import Char
    exposing
        ( KeyCode
        , fromCode
        , toCode
        )
import Random
    exposing
        ( Generator
        , int
        )
import UserIdentifierType
    exposing
        ( ThreeLetterNumberSpaceInt
        , UserIdentifier
        )


-- MODEL


caseLength : Int
caseLength =
    1 + toCode 'Z' - toCode 'A'


digitCount : Int
digitCount =
    3


letterSpace : Int
letterSpace =
    let
        caseCount : Int
        caseCount =
            --Upper and lower case (letters).
            2
    in
    caseCount * caseLength


threeLetterNumberSpaceIntRandom : Generator ThreeLetterNumberSpaceInt
threeLetterNumberSpaceIntRandom =
    let
        highest : ThreeLetterNumberSpaceInt
        highest =
            (letterSpace ^ digitCount) - 1
    in
    int 0 highest


userIdentifierInit : UserIdentifier
userIdentifierInit =
    ""



-- UPDATE


updateInitialSetUp : ThreeLetterNumberSpaceInt -> UserIdentifier
updateInitialSetUp threeLetterNumberSpaceInt =
    let
        keyCode2Char : KeyCode -> Char
        keyCode2Char digit =
            let
                charBase : KeyCode
                charBase =
                    if digit < caseLength then
                        toCode 'A'
                    else
                        toCode 'a'
            in
            (digit % caseLength)
                |> (+) charBase
                |> fromCode

        threeDigits : List Int
        threeDigits =
            (digitCount - 1)
                |> flip List.repeat letterSpace
                |> List.scanl (//) threeLetterNumberSpaceInt
                |> List.map (\x -> x % letterSpace)
    in
    threeDigits
        |> List.map keyCode2Char
        |> String.fromList
