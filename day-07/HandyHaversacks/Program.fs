open System
open System.IO
open System.Text.RegularExpressions

type Rule =
    { OuterBagColor: string
      InnerBagColors: string seq }

let getOuterBagColor rule =
    let outerBagColorPattern = "^(?<OuterBagColor>\w+ \w+) bags"

    Regex.Match(rule, outerBagColorPattern).Groups.["OuterBagColor"]
        .Value

let getInnterBagColors rule =
    let innerBagColorPattern = "\d+ (?<InnerBagColor>\w+ \w+) bag"

    Regex.Matches(rule, innerBagColorPattern)
    |> Seq.map (fun x -> x.Groups.["InnerBagColor"].Value)

let getRule ruleText =
    let outerBagColor = getOuterBagColor ruleText
    let innerBagColors = getInnterBagColors ruleText

    { OuterBagColor = outerBagColor
      InnerBagColors = innerBagColors }

let rec getOuterBagsRecursively (rules: Rule seq) (color: string) =
    let directOuterBags =
        rules
        |> Seq.filter (fun r -> r.InnerBagColors |> Seq.contains color)
        |> Seq.map (fun r -> r.OuterBagColor)

    seq {
        yield! directOuterBags

        for bag in directOuterBags do
            yield! getOuterBagsRecursively rules bag
    }

let getOuterBags (ruleTexts: string seq) (color: string): string Set =
    let rules = ruleTexts |> Seq.map getRule
    getOuterBagsRecursively rules color |> Set.ofSeq


[<EntryPoint>]
let main argv =
    let rules = "./input.txt" |> File.ReadAllLines

    let numberOfBags =
        getOuterBags rules "shiny gold" |> Set.count

    printfn "%d bags can eventually contain a shiny gold bag." numberOfBags

    0 // return an integer exit code
