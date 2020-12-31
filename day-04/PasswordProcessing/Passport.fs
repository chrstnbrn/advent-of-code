namespace Passport

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open Option

module BirthYear =
    type T = BirthYear of int

    let create (s: string) =
        match System.Int32.TryParse s with
        | true, year when year >= 1920 && year <= 2002 -> Some(BirthYear year)
        | _ -> None

module IssueYear =
    type T = IssueYear of int

    let create (s: string) =
        match System.Int32.TryParse s with
        | true, year when year >= 2010 && year <= 2020 -> Some(IssueYear year)
        | _ -> None

module ExpirationYear =
    type T = ExpirationYear of int

    let create (s: string) =
        match System.Int32.TryParse s with
        | true, year when year >= 2020 && year <= 2030 -> Some(ExpirationYear year)
        | _ -> None

module Height =
    type T =
        | HeightInCm of int
        | HeightInIn of int

    type HeightRegex = Regex<"^(?<Height>\d+)(?<Unit>\w+)$", noMethodPrefix=true>

    let create (s: string) =
        let result = HeightRegex().Match(s)

        match result.Height.TryAsInt, result.Unit.TryValue with
        | Some height, Some "cm" when height >= 150 && height <= 193 -> Some <| HeightInCm height
        | Some height, Some "in" when height >= 59 && height <= 76 -> Some <| HeightInIn height
        | _ -> None

module HairColor =
    type T = HairColor of string

    let create (s: string) =
        let pattern = "^#[0-9a-f]{6}$"

        if Regex.IsMatch(s, pattern) then
            Some(HairColor s)
        else
            None

module EyeColor =
    type T = EyeColor of string

    let create (s: string) =
        let colors =
            [ "amb"
              "blu"
              "brn"
              "gry"
              "grn"
              "hzl"
              "oth" ]

        if colors |> List.exists ((=) s) then
            Some(EyeColor s)
        else
            None

module PassportId =
    type T = PassportId of string

    let create (s: string) =
        let pattern = "^\d{9}$"

        if Regex.IsMatch(s, pattern) then
            Some(PassportId s)
        else
            None

module CountryId =
    type T = CountryId of string

    let create (s: string) = Some(CountryId s)

type UnvalidatedPassport =
    { BirthYear: string
      IssueYear: string
      ExpirationYear: string
      Height: string
      HairColor: string
      EyeColor: string
      PassportId: string
      CountryId: string option }

module Passport =
    type T =
        { BirthYear: BirthYear.T
          IssueYear: IssueYear.T
          ExpirationYear: ExpirationYear.T
          Height: Height.T
          HairColor: HairColor.T
          EyeColor: EyeColor.T
          PassportId: PassportId.T
          CountryId: CountryId.T option }

    let create (unvalidatedPassport: UnvalidatedPassport): T option =
        option {
            let! birthYear = BirthYear.create unvalidatedPassport.BirthYear
            let! issueYear = IssueYear.create unvalidatedPassport.IssueYear
            let! expirationYear = ExpirationYear.create unvalidatedPassport.ExpirationYear
            let! height = Height.create unvalidatedPassport.Height
            let! hairColor = HairColor.create unvalidatedPassport.HairColor
            let! eyeColor = EyeColor.create unvalidatedPassport.EyeColor
            let! passportId = PassportId.create unvalidatedPassport.PassportId

            let countryId =
                bind CountryId.create unvalidatedPassport.CountryId

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
