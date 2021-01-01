open System.IO
open System.Text.RegularExpressions

type InnerBag = { Color: string; Amount: int }

type Rule =
    { OuterBagColor: string
      InnerBags: InnerBag seq }

let getOuterBagColor rule =
    let outerBagColorPattern = "^(?<OuterBagColor>\w+ \w+) bags"

    Regex.Match(rule, outerBagColorPattern).Groups.["OuterBagColor"]
        .Value

let getInnerBags rule =
    let innerBagColorPattern =
        "(?<Amount>\d+) (?<InnerBagColor>\w+ \w+) bag"

    Regex.Matches(rule, innerBagColorPattern)
    |> Seq.map
        (fun x ->
            let color = x.Groups.["InnerBagColor"].Value
            let amount = x.Groups.["Amount"].Value |> int
            { Color = color; Amount = amount })

let getRule ruleText =
    let outerBagColor = getOuterBagColor ruleText
    let innerBagColors = getInnerBags ruleText

    { OuterBagColor = outerBagColor
      InnerBags = innerBagColors }

let isRuleForInnerBag (color: string) (rule: Rule): bool =
    rule.InnerBags
    |> Seq.map (fun b -> b.Color)
    |> Seq.contains color

let rec getOuterBagColorsRecursively (rules: Rule seq) (color: string) =
    let directOuterBags =
        rules
        |> Seq.filter (isRuleForInnerBag color)
        |> Seq.map (fun r -> r.OuterBagColor)

    seq {
        yield! directOuterBags

        for bag in directOuterBags do
            yield! getOuterBagColorsRecursively rules bag
    }

let getOuterBagColors (ruleTexts: string seq) (color: string): string Set =
    let rules = ruleTexts |> Seq.map getRule

    getOuterBagColorsRecursively rules color
    |> Set.ofSeq

let rec getNumberOfBagsInsideRecursively (rules: Rule seq) (color: string): int =
    let directInnerBags =
        rules
        |> Seq.find (fun r -> r.OuterBagColor = color)
        |> (fun r -> r.InnerBags)

    directInnerBags
    |> Seq.sumBy
        (fun b ->
            b.Amount
            + (b.Amount
               * getNumberOfBagsInsideRecursively rules b.Color))

let getNumberOfBagsInside (ruleTexts: string seq) (color: string): int =
    let rules = ruleTexts |> Seq.map getRule
    getNumberOfBagsInsideRecursively rules color

[<EntryPoint>]
let main argv =
    let rules = "./input.txt" |> File.ReadAllLines

    let bagColor = "shiny gold"

    let numberOfOuterBagColors =
        getOuterBagColors rules bagColor |> Set.count

    let numberOfBagsInside = getNumberOfBagsInside rules bagColor

    printfn "%d bags can eventually contain a %s bag." numberOfOuterBagColors bagColor
    printfn "A single %s bag contains %d bags." bagColor numberOfBagsInside

    0 // return an integer exit code
