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


module Utilities
    exposing
        ( indexes
        , matchingIndexes
        , maybeDefaultNothing
        , selectOne
        , startingWith
        , withIndexes
        , withoutOne
        )

-- UPDATE


indexes : List a -> List Int
indexes listA =
    List.range 0 (List.length listA - 1)


matchingIndexes : List a -> a -> List Int
matchingIndexes listA a =
    let
        matchWithIndex : ( Int, a ) -> Maybe Int
        matchWithIndex ( index, another ) =
            if another == a then
                Just index
            else
                Nothing
    in
    List.filterMap matchWithIndex (withIndexes listA)


maybeDefaultNothing : (a -> Maybe b) -> Maybe a -> Maybe b
maybeDefaultNothing function value =
    Maybe.withDefault Nothing (Maybe.map function value)


selectOne : List a -> Int -> Maybe a
selectOne listA index =
    List.head (startingWith listA index)


startingWith : List a -> Int -> List a
startingWith listA index =
    List.drop index listA


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    List.map2 (,) (indexes listA) listA


withoutOne : List a -> Int -> List a
withoutOne listA index =
    List.take index listA
        ++ startingWith listA (index + 1)
