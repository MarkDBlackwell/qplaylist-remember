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
        , maybeMapWithDefault
        , selectOneMaybe
        , startingWith
        , withIndexes
        , withoutOne
        )

-- UPDATE


indexes : List a -> List Int
indexes listA =
    List.length listA
        - 1
        |> List.range 0


matchingIndexes : List a -> a -> List Int
matchingIndexes listA a =
    let
        matchWithIndexMaybe : ( Int, a ) -> Maybe Int
        matchWithIndexMaybe ( index, another ) =
            if another == a then
                Just index
            else
                Nothing
    in
    withIndexes listA
        |> List.filterMap matchWithIndexMaybe


maybeDefaultNothing : (a -> Maybe b) -> Maybe a -> Maybe b
maybeDefaultNothing function value =
    maybeMapWithDefault Nothing function value


maybeMapWithDefault : a -> (b -> a) -> Maybe b -> a
maybeMapWithDefault default function value =
    Maybe.map function value
        |> Maybe.withDefault default


selectOneMaybe : List a -> Int -> Maybe a
selectOneMaybe listA index =
    startingWith listA index
        |> List.head


startingWith : List a -> Int -> List a
startingWith listA index =
    List.drop index listA


withIndexes : List a -> List ( Int, a )
withIndexes listA =
    List.map2 (,) (indexes listA) listA


withoutOne : List a -> Int -> List a
withoutOne listA index =
    index
        + 1
        |> startingWith listA
        |> (++) (List.take index listA)
