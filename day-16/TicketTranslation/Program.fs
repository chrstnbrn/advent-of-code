open System
open System.IO
open System.Text.RegularExpressions

type Range = { Start: int; End: int }

type Rule =
    { Field: string
      ValidRanges: Range [] }

type Ticket = int []

let parseRule ruleText =
    let fieldPattern = "^(?<Field>\w+):"
    let rangePattern = "(?<Start>\d+)-(?<End>\d+)"

    let field =
        Regex.Match(ruleText, fieldPattern).Groups.["Field"]
            .Value

    let ranges =
        Regex.Matches(ruleText, rangePattern)
        |> Seq.map
            (fun m ->
                { Start = int m.Groups.["Start"].Value
                  End = int m.Groups.["End"].Value })
        |> Seq.toArray

    { Field = field; ValidRanges = ranges }

let getTicketValues (ticketText: string) = ticketText.Split(',') |> Array.map int

let isInRange value range =
    range.Start <= value && value <= range.End

let isValidValue value rule =
    rule.ValidRanges |> Seq.exists (isInRange value)

let isInvalidValue value rule = rule |> isValidValue value |> not

let getTicketScanningErrorRate ticketTexts ruleTexts =
    let rules = ruleTexts |> Array.map parseRule

    let ticketValues =
        ticketTexts |> Array.collect getTicketValues

    let invalidTicketValues =
        ticketValues
        |> Seq.filter (fun v -> rules |> Array.forall (isInvalidValue v))

    invalidTicketValues |> Seq.sum

[<EntryPoint>]
let main argv =
    let text = File.ReadAllText "./input.txt"

    let blocks =
        text.Split(Environment.NewLine + Environment.NewLine)

    let rules = blocks.[0].Split(Environment.NewLine)

    let tickets =
        blocks.[2].Split(Environment.NewLine)
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.skip 1

    let ticketScanningErrorRate = getTicketScanningErrorRate tickets rules
    printfn "The ticket scanning error rate is %d" ticketScanningErrorRate

    0
