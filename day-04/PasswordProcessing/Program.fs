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

let hasRequiredFields passport =
    passport |> getMissingFields |> Seq.isEmpty

let isValidPassport passport = hasRequiredFields passport

[<EntryPoint>]
let main argv =
    let passportText = File.ReadAllText "./input.txt"

    let passports =
        passportText.Split(Environment.NewLine + Environment.NewLine)

    let numberOfPassportsWithRequiredFields =
        passports
        |> Seq.filter hasRequiredFields
        |> Seq.length

    let numberOfValidPassports =
        passports
        |> Seq.filter isValidPassport
        |> Seq.length

    printfn "%d of %d passports have all required fields" numberOfPassportsWithRequiredFields (Seq.length passports)
    printfn "%d of %d passports are valid" numberOfValidPassports (Seq.length passports)

    0
