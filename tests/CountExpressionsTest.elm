module CountExpressionsTest exposing (all)

import CountExpressions exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "CountExpressions"
        [ test "dummy test" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
