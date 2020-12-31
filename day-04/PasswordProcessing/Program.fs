open System
open System.IO
open System.Text.RegularExpressions
open Passport
open Option

let getValue s key =
    let pattern = "(" + key + "):(?<value>[^\s]+)"

    let result = Regex.Match(s, pattern)

    if result.Success
       && result.Groups.ContainsKey("value") then
        Some result.Groups.["value"].Value
    else
        None

let getUnvalidatedPassport (passport: string): UnvalidatedPassport option =
    option {
        let getPassportValue = getValue passport
        let! birthYear = getPassportValue "byr"
        let! issueYear = getPassportValue "iyr"
        let! expirationYear = getPassportValue "eyr"
        let! height = getPassportValue "hgt"
        let! hairColor = getPassportValue "hcl"
        let! eyeColor = getPassportValue "ecl"
        let! passportId = getPassportValue "pid"
        let countryId = getPassportValue "cid"

        return
            { BirthYear = birthYear
              IssueYear = issueYear
              ExpirationYear = expirationYear
              Height = height
              HairColor = hairColor
              EyeColor = eyeColor
              PassportId = passportId
              CountryId = countryId }
    }

let hasRequiredFields passport =
    passport |> getUnvalidatedPassport |> (<>) None

let isValidPassport passport =
    passport
    |> getUnvalidatedPassport
    |> Option.bind Passport.create
    |> (<>) None

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
