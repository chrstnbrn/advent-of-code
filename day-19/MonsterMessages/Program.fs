open System
open System.IO
open System.Text.RegularExpressions

type Rule =
    { Number: int
      Definition: RuleDefinition }

and RuleDefinition =
    | MatchCharacter of char
    | MatchAnySubRule of SubRule []

and SubRule = int []

let parseSubRules (s: string) =
    s.Split('|')
    |> Array.map (
        (fun x ->
            x.Split()
            |> Array.filter (not << String.IsNullOrWhiteSpace)
            |> Array.map int)
    )

let parseRule ruleText =
    let pattern =
        "^(?<Number>\d+):\s*((\"(?<Character>\w)\")|(?<Subrules>(\d+\s*)+(\|(\s*\d+)+)*))$"

    let m = Regex.Match(ruleText, pattern)
    let number = int m.Groups.["Number"].Value

    if (m.Groups.["Character"].Success) then
        let character = m.Groups.["Character"].Value |> char

        { Number = number
          Definition = MatchCharacter character }
    else
        let subRules =
            m.Groups.["Subrules"].Value |> parseSubRules

        { Number = number
          Definition = MatchAnySubRule subRules }

let getRulesMap rules =
    rules
    |> Array.map (parseRule >> (fun rule -> (rule.Number, rule)))
    |> Map

let surroundWith startString endString s = startString + s + endString

let rec getRegexPattern rules ruleNumber =
    let rule = Map.find ruleNumber rules

    let surroundWithBraces = surroundWith "(" ")"

    let getPatternForSubRule =
        Array.map (getRegexPattern rules)
        >> String.concat ""
        >> surroundWithBraces

    match rule.Definition with
    | MatchCharacter ch -> ch.ToString()

    | MatchAnySubRule [| [| 42 |]; [| 42; 8 |] |] when rule.Number = 8 ->
        let patternFor42 = getRegexPattern rules 42
        patternFor42 + "+"

    | MatchAnySubRule [| [| 42; 31 |]; [| 42; 11; 31 |] |] when rule.Number = 11 ->
        let patternFor42 = getRegexPattern rules 42
        let patternFor31 = getRegexPattern rules 31
        sprintf "(?<n>%s)+(?<-n>%s)+(?(n)(?!))" patternFor42 patternFor31

    | MatchAnySubRule subRules ->
        subRules
        |> Array.map (getPatternForSubRule)
        |> String.concat "|"
        |> surroundWithBraces

let getMessagesMatchingRule ruleTexts replaceRules messages ruleNumber =
    let rules =
        replaceRules
        |> Array.fold (fun map rule -> Map.add rule.Number rule map) (getRulesMap ruleTexts)

    let pattern =
        getRegexPattern rules ruleNumber
        |> surroundWith "^" "$"

    messages
    |> Seq.filter (fun m -> Regex.IsMatch(m, pattern))
    |> Set

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt"

    let blocks =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map (fun x -> x.Split(Environment.NewLine))

    let rules, messages = blocks.[0], blocks.[1]

    let numberofMessages =
        getMessagesMatchingRule rules [||] messages 0
        |> Seq.length

    printfn "%d messages completely match rule 0" numberofMessages

    let replaceRules =
        [| { Number = 8
             Definition =
                 MatchAnySubRule [| [| 42 |]
                                    [| 42; 8 |] |] }
           { Number = 11
             Definition =
                 MatchAnySubRule [| [| 42; 31 |]
                                    [| 42; 11; 31 |] |] } |]

    let numberofMessages =
        getMessagesMatchingRule rules replaceRules messages 0
        |> Seq.length

    printfn "After updating rules 8 and 11 %d messages completely match rule 0" numberofMessages

    0
