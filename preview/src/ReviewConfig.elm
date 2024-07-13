module ReviewConfig exposing (config)

import CountExpressions
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ CountExpressions.rule
    ]
