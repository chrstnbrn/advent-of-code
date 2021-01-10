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

let rec cartesianProduct lists =
    match lists with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in cartesianProduct Ls -> x :: xs
        }

let rec getValidMessages rules ruleNumber =
    let rule = rules |> Map.find ruleNumber

    let getValidMessagesForSubRule =
        Array.map (getValidMessages rules)
        >> Array.toList
        >> cartesianProduct
        >> Seq.map (String.concat "")
        >> Seq.toArray

    match rule.Definition with
    | MatchCharacter ch -> [| ch.ToString() |]
    | MatchAnySubRule subRules ->
        subRules
        |> Array.collect getValidMessagesForSubRule

let getMessagesMatchingRule ruleTexts messages ruleNumber =
    let rules =
        ruleTexts
        |> Array.map (parseRule >> (fun rule -> (rule.Number, rule)))
        |> Map

    let validMessages = getValidMessages rules ruleNumber

    Set.intersect (Set messages) (Set validMessages)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt"

    let blocks =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map (fun x -> x.Split(Environment.NewLine))

    let rules, messages = blocks.[0], blocks.[1]

    let numberofMessages =
        getMessagesMatchingRule rules messages 0
        |> Seq.length

    printfn "%d messages completely match rule 0" numberofMessages

    0
