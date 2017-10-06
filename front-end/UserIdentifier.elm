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
        ( threeLetterSpaceIntRandom
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
        ( ThreeLetterSpaceInt
        , UserIdentifier
        )


-- MODEL


caseLength : Int
caseLength =
    1 + toCode 'Z' - toCode 'A'


letterSpace : Int
letterSpace =
    let
        caseCount : Int
        caseCount =
            2
    in
    caseCount * caseLength


threeLetterSpaceIntRandom : Generator ThreeLetterSpaceInt
threeLetterSpaceIntRandom =
    let
        highest : ThreeLetterSpaceInt
        highest =
            (letterSpace ^ 3) - 1
    in
    int 0 highest


userIdentifierInit : UserIdentifier
userIdentifierInit =
    ""



-- UPDATE


updateInitialSetUp : ThreeLetterSpaceInt -> UserIdentifier
updateInitialSetUp threeLetterSpaceInt =
    let
        keyCode2Char : KeyCode -> Char
        keyCode2Char keyCode =
            let
                baseKeyCode : KeyCode -> KeyCode
                baseKeyCode keyCode =
                    if keyCode < caseLength then
                        toCode 'a'
                    else
                        toCode 'A'
            in
            keyCode
                % caseLength
                |> (+) keyCode
                |> baseKeyCode
                |> fromCode

        threeDigits : ThreeLetterSpaceInt -> List Int
        threeDigits threeLetterSpaceInt =
            [ (threeLetterSpaceInt // letterSpace // letterSpace) % letterSpace
            , (threeLetterSpaceInt // letterSpace) % letterSpace
            , threeLetterSpaceInt % letterSpace
            ]
    in
    threeDigits threeLetterSpaceInt
        |> List.map keyCode2Char
        |> String.fromList
