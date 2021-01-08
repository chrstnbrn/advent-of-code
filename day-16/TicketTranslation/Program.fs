open System
open System.IO
open System.Text.RegularExpressions

type Range = { Start: int; End: int }

type Rule =
    { Field: string
      ValidRanges: Range [] }

let parseRule ruleText =
    let fieldPattern = "^(?<Field>.+):"
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

let isValidValueForAnyRule rules value =
    rules |> Seq.exists (isValidValue value)

let getPossibleRules rules values =
    rules
    |> Array.filter (fun r -> values |> Array.forall (fun v -> isValidValue v r))

let getValuesInColumn i = Array.map (Array.item i)

let getPossibleRulesForColumn i rules =
    getValuesInColumn i >> (getPossibleRules rules)

let getFieldsOrder rules tickets =
    tickets
    |> Array.head
    |> Array.mapi (fun i _ -> (i, tickets |> getPossibleRulesForColumn i rules))
    |> Array.sortBy (fun (_, r) -> r.Length)
    |> Array.fold
        (fun result (i, rules) ->
            let usedRules = result |> Array.map snd

            let rule =
                rules |> Array.except usedRules |> Array.head

            Array.concat [ result
                           Array.singleton (i, rule) ])
        [||]
    |> Array.sortBy fst
    |> Array.map (fun (_, r) -> r.Field)

let getTicketScanningErrorRate ticketTexts ruleTexts =
    let rules = ruleTexts |> Array.map parseRule

    let ticketValues =
        ticketTexts |> Array.collect getTicketValues

    let invalidTicketValues =
        ticketValues
        |> Seq.filter (not << (isValidValueForAnyRule rules))

    invalidTicketValues |> Seq.sum

let decodeTicket ruleTexts ticketTexts ticketText =
    let rules = ruleTexts |> Array.map parseRule

    let validTickets =
        ticketTexts
        |> Array.map getTicketValues
        |> Array.filter
            (fun values ->
                values
                |> Seq.forall (isValidValueForAnyRule rules))

    let fieldsOrder = getFieldsOrder rules validTickets

    ticketText
    |> getTicketValues
    |> Array.zip fieldsOrder
    |> Map

let getBlocks (text: string) =
    let blocks =
        text.Split(Environment.NewLine + Environment.NewLine)

    let rules = blocks.[0].Split(Environment.NewLine)

    let yourTicket =
        blocks.[1].Split(Environment.NewLine).[1]

    let tickets =
        blocks.[2].Split(Environment.NewLine)
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.skip 1

    (rules, yourTicket, tickets)

[<EntryPoint>]
let main argv =
    let rules, yourTicket, tickets =
        "./input.txt" |> File.ReadAllText |> getBlocks

    let ticketScanningErrorRate = getTicketScanningErrorRate tickets rules
    printfn "The ticket scanning error rate is %d" ticketScanningErrorRate

    let decodedTicket = decodeTicket rules tickets yourTicket

    let productOfDepartureValues =
        decodedTicket
        |> Map.toSeq
        |> Seq.filter (fun (field, _) -> field.StartsWith("departure"))
        |> Seq.map (snd >> int64)
        |> Seq.reduce (*)

    printfn "The product of the departure values on the ticket is %i" productOfDepartureValues

    0
