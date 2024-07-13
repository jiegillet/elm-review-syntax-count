# elm-review-syntax-count

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`CountExpressions`](https://package.elm-lang.org/packages/jiegillet/elm-review-syntax-count/1.0.0/CountExpressions) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import CountExpressions
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ CountExpressions.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jiegillet/elm-review-syntax-count/example
```
