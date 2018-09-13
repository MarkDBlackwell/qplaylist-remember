{- Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}

{-
See:
https://www.linkedin.com/pulse/single-page-web-apps-elm-part-three-testing-structure-kevin-greene
-}

import Test
 exposing
  ( Test
  , suite
  , test
  , assertEqual
  )

tests : Test
tests =
    suite "Tests"
        [ test "Equality"
            <| assertEqual "test" "test"
        ]

main : Program Never
main =
    Test.runSuite tests
