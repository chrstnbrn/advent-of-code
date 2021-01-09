open System.IO
open FParsec

let evaluate (expression: string): int64 =
    let opp =
        OperatorPrecedenceParser<int64, unit, unit>()

    opp.TermParser <-
        (pint64 .>> spaces)
        <|> between (pstring "(" >>. spaces) (pstring ")" .>> spaces) opp.ExpressionParser

    opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, (+)))
    opp.AddOperator(InfixOperator("*", spaces, 1, Associativity.Left, (*)))

    match run opp.ExpressionParser expression with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

[<EntryPoint>]
let main argv =
    let expressions = File.ReadAllLines "./input.txt"

    let sumOfExpressionResults =
        expressions |> Array.sumBy (evaluate >> int64)

    printfn "The of of all expression results is %d" sumOfExpressionResults

    0
