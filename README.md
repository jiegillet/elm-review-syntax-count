# elm-review-syntax-count

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to explore the distribution of elm-syntax data structures in real projects.

## Provided rules

- [`CountExpressions`](https://package.elm-lang.org/packages/jiegillet/elm-review-syntax-count/1.0.0/CountExpressions) - Reports different counts of expressions and patterns.

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
elm-review --template jiegillet/elm-review-syntax-count/example --report=json --extract
```

To have a better view of the data, if you have `jq` you can use

```bash
elm-review --template jiegillet/elm-review-syntax-count/example --report=json --extract \
  | jq '.extracts .CountExpressions | map_values(to_entries | sort_by(.value) | reverse | from_entries)'
```
