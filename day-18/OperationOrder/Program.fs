open System.IO
open FParsec

let evaluateExpression operators expression =
    let opp =
        OperatorPrecedenceParser<int64, unit, unit>()

    opp.TermParser <-
        (pint64 .>> spaces)
        <|> between (pstring "(" >>. spaces) (pstring ")" .>> spaces) opp.ExpressionParser

    operators |> Seq.iter opp.AddOperator

    match run opp.ExpressionParser expression with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let addition precedence =
    InfixOperator("+", spaces, precedence, Associativity.Left, (+))

let multiplication precedence =
    InfixOperator("*", spaces, precedence, Associativity.Left, (*))

let evaluate (expression: string): int64 =
    expression
    |> evaluateExpression [ addition 1
                            multiplication 1 ]

let evaluateWithNewRules (expression: string): int64 =
    expression
    |> evaluateExpression [ addition 2
                            multiplication 1 ]

[<EntryPoint>]
let main argv =
    let expressions = File.ReadAllLines "./input.txt"

    let sum = expressions |> Array.sumBy evaluate
    printfn "The of of all expression results is %d" sum

    let sumWithNewRules =
        expressions |> Array.sumBy evaluateWithNewRules

    printfn "The of of all expression results when using the new rules is %d" sumWithNewRules

    0
