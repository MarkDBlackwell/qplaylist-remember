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
        ( UserIdentifier
        , threeLetterSpaceRandom
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
        )


-- MODEL


type alias ThreeLetterSpaceInt =
    Int


type alias UserIdentifier =
    String


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


threeLetterSpaceRandom : Generator Int
threeLetterSpaceRandom =
    let
        threeLetterSpaceHighest : ThreeLetterSpaceInt
        threeLetterSpaceHighest =
            (letterSpace ^ 3) - 1
    in
    Random.int 0 threeLetterSpaceHighest


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
            fromCode (baseKeyCode keyCode + (keyCode % caseLength))

        threeDigits : ThreeLetterSpaceInt -> List Int
        threeDigits threeLetterSpaceInt =
            [ (threeLetterSpaceInt // letterSpace // letterSpace) % letterSpace
            , (threeLetterSpaceInt // letterSpace) % letterSpace
            , threeLetterSpaceInt % letterSpace
            ]
    in
    String.fromList (List.map keyCode2Char (threeDigits threeLetterSpaceInt))
