module CountExpressions exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Json.Encode as Encode exposing (Value)
import Review.Rule as Rule exposing (Rule)


type alias Context =
    { expressionCount : Dict String Int
    , operatorCount : Dict String Int
    , prefixOperatorCount : Dict String Int
    , patternCount : Dict String Int
    }


{-| This rule extracts different patterns and expression counts from a project
-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "CountExpressions" emptyContext
        |> Rule.withModuleVisitor
            (Rule.withExpressionEnterVisitor expressionVisitor
                >> Rule.withExpressionEnterVisitor expressionPatternVisitor
                >> Rule.withDeclarationEnterVisitor declarationVisitor
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromModuleToProject = Rule.initContextCreator identity
            , fromProjectToModule = Rule.initContextCreator identity
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


emptyContext : Context
emptyContext =
    { expressionCount = Dict.empty
    , operatorCount = Dict.empty
    , prefixOperatorCount = Dict.empty
    , patternCount = Dict.empty
    }


foldProjectContexts : Context -> Context -> Context
foldProjectContexts a b =
    Context (foldCounts a.expressionCount b.expressionCount)
        (foldCounts a.operatorCount b.operatorCount)
        (foldCounts a.prefixOperatorCount b.prefixOperatorCount)
        (foldCounts a.patternCount b.patternCount)


foldCounts : Dict String Int -> Dict String Int -> Dict String Int
foldCounts countA countB =
    Dict.merge Dict.insert (\key a b -> Dict.insert key (a + b)) Dict.insert countA countB Dict.empty


dataExtractor : Context -> Value
dataExtractor { expressionCount, operatorCount, prefixOperatorCount, patternCount } =
    Encode.object
        [ ( "expressionCount", Encode.dict identity Encode.int expressionCount )
        , ( "operatorCount", Encode.dict identity Encode.int operatorCount )
        , ( "prefixOperatorCount", Encode.dict identity Encode.int prefixOperatorCount )
        , ( "patternCount", Encode.dict identity Encode.int patternCount )
        ]


addToCount : String -> Dict String Int -> Dict String Int
addToCount key =
    Dict.update key (\maybeCount -> Just (1 + Maybe.withDefault 0 maybeCount))


expressionVisitor : Node Expression -> Context -> ( List never, Context )
expressionVisitor (Node _ expression) ({ expressionCount, operatorCount, prefixOperatorCount } as context) =
    case expression of
        -- expressions
        UnitExpr ->
            ( [], { context | expressionCount = addToCount "UnitExpr" expressionCount } )

        Application _ ->
            ( [], { context | expressionCount = addToCount "Application" expressionCount } )

        FunctionOrValue _ _ ->
            ( [], { context | expressionCount = addToCount "FunctionOrValue" expressionCount } )

        IfBlock _ _ _ ->
            ( [], { context | expressionCount = addToCount "IfBlock" expressionCount } )

        Integer _ ->
            ( [], { context | expressionCount = addToCount "NumberExpression" expressionCount } )

        Hex _ ->
            ( [], { context | expressionCount = addToCount "NumberExpression" expressionCount } )

        Floatable _ ->
            ( [], { context | expressionCount = addToCount "NumberExpression" expressionCount } )

        Literal _ ->
            ( [], { context | expressionCount = addToCount "Literal" expressionCount } )

        CharLiteral _ ->
            ( [], { context | expressionCount = addToCount "CharLiteral" expressionCount } )

        TupledExpression _ ->
            ( [], { context | expressionCount = addToCount "TupledExpression" expressionCount } )

        ParenthesizedExpression _ ->
            ( [], { context | expressionCount = addToCount "ParenthesizedExpression" expressionCount } )

        LetExpression _ ->
            ( [], { context | expressionCount = addToCount "LetExpression" expressionCount } )

        CaseExpression _ ->
            ( [], { context | expressionCount = addToCount "CaseExpression" expressionCount } )

        LambdaExpression _ ->
            ( [], { context | expressionCount = addToCount "LambdaExpression" expressionCount } )

        RecordExpr _ ->
            ( [], { context | expressionCount = addToCount "RecordExpr" expressionCount } )

        ListExpr _ ->
            ( [], { context | expressionCount = addToCount "ListExpr" expressionCount } )

        RecordAccessFunction _ ->
            ( [], { context | expressionCount = addToCount "RecordAccessFunction" expressionCount } )

        RecordUpdateExpression _ _ ->
            ( [], { context | expressionCount = addToCount "RecordUpdateExpression" expressionCount } )

        GLSLExpression _ ->
            ( [], { context | expressionCount = addToCount "GLSLExpression" expressionCount } )

        -- operators
        OperatorApplication operator _ _ _ ->
            ( [], { context | operatorCount = addToCount operator operatorCount } )

        Negation _ ->
            ( [], { context | operatorCount = addToCount "Negation" operatorCount } )

        RecordAccess _ _ ->
            ( [], { context | operatorCount = addToCount "RecordAccess" operatorCount } )

        -- prefix operators
        PrefixOperator operator ->
            ( [], { context | prefixOperatorCount = addToCount operator prefixOperatorCount } )

        -- excluded
        Operator _ ->
            ( [], context )


expressionPatternVisitor : Node Expression -> Context -> ( List never, Context )
expressionPatternVisitor (Node _ expression) context =
    case expression of
        LetExpression { declarations } ->
            declarations
                |> List.concatMap getPatternsFromLetDeclaration
                |> patternVisitor context

        CaseExpression { cases } ->
            cases
                |> List.map Tuple.first
                |> patternVisitor context

        LambdaExpression { args } ->
            patternVisitor context args

        _ ->
            ( [], context )


getPatternsFromLetDeclaration : Node LetDeclaration -> List (Node Pattern)
getPatternsFromLetDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        LetDestructuring pattern _ ->
            [ pattern ]

        LetFunction { declaration } ->
            declaration
                |> Node.value
                |> .arguments


declarationVisitor : Node Declaration -> Context -> ( List never, Context )
declarationVisitor (Node _ declaration) context =
    case declaration of
        FunctionDeclaration functionDeclaration ->
            functionDeclaration
                |> .declaration
                |> Node.value
                |> .arguments
                |> patternVisitor context

        Destructuring pattern _ ->
            patternVisitor context [ pattern ]

        _ ->
            ( [], context )


patternVisitor : Context -> List (Node Pattern) -> ( List never, Context )
patternVisitor ({ patternCount } as context) topPatterns =
    let
        encodePattern : Node Pattern -> List String
        encodePattern (Node _ pattern) =
            case pattern of
                AllPattern ->
                    [ "AllPattern" ]

                UnitPattern ->
                    [ "UnitPattern" ]

                CharPattern _ ->
                    [ "CharPattern" ]

                StringPattern _ ->
                    [ "StringPattern" ]

                IntPattern _ ->
                    [ "NumbePattern" ]

                HexPattern _ ->
                    [ "NumbePattern" ]

                FloatPattern _ ->
                    [ "FloatPattern" ]

                TuplePattern patterns ->
                    "TuplePattern" :: List.concatMap encodePattern patterns

                RecordPattern _ ->
                    [ "RecordPattern" ]

                UnConsPattern left right ->
                    "UnConsPattern" :: List.concatMap encodePattern [ left, right ]

                ListPattern patterns ->
                    "ListPattern" :: List.concatMap encodePattern patterns

                VarPattern _ ->
                    [ "VarPattern" ]

                NamedPattern _ patterns ->
                    "NamedPattern" :: List.concatMap encodePattern patterns

                AsPattern subPattern _ ->
                    "AsPattern" :: encodePattern subPattern

                ParenthesizedPattern subPattern ->
                    "ParenthesizedPattern" :: encodePattern subPattern
    in
    ( [], { context | patternCount = topPatterns |> List.concatMap encodePattern |> List.foldl addToCount patternCount } )
