open System
open System.IO
open System.Text.RegularExpressions

let requiredFields =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let getFields passport =
    let pattern = "(?<field>\w+):[^\s]+"

    Regex.Matches(passport, pattern)
    |> Seq.map (fun x -> x.Groups.["field"].Value)

let getMissingFields passport =
    requiredFields |> Seq.except (getFields passport)

let isValidPassport passport =
    passport |> getMissingFields |> Seq.isEmpty

[<EntryPoint>]
let main argv =
    let passportText = File.ReadAllText "./input.txt"

    let passports =
        passportText.Split(Environment.NewLine + Environment.NewLine)

    let numberOfValidPassports =
        passports
        |> Seq.filter isValidPassport
        |> Seq.length

    printfn "%d of %d passports are valid" numberOfValidPassports (Seq.length passports)

    0
