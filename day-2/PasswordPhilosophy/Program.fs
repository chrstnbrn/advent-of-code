open System
open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type PasswordPolicy = { Letter: char; Min: int; Max: int }

type PasswordLine =
    { PasswordPolicy: PasswordPolicy
      Password: string }

type PasswordLineRegex = Regex<"^(?<Min>\d+)-(?<Max>\d+) (?<Letter>\w): (?<Password>.*)$">

let parseLine (line: string): PasswordLine option =
    PasswordLineRegex().TryTypedMatch(line)
    |> Option.map
        (fun x ->
            { PasswordPolicy =
                  { Letter = x.Letter.AsChar
                    Min = x.Min.AsInt
                    Max = x.Max.AsInt }
              Password = x.Password.Value })

let isValid (policy: PasswordPolicy) (password: string): bool =
    let letterOccurences =
        password
        |> Seq.filter ((=) policy.Letter)
        |> Seq.length

    policy.Min <= letterOccurences
    && letterOccurences <= policy.Max

[<EntryPoint>]
let main argv =
    let inputPath =
        Path.Combine(Environment.CurrentDirectory, "input.txt")

    let passwordLines =
        inputPath
        |> File.ReadAllLines
        |> Seq.map parseLine
        |> Seq.choose id

    let isValidLine l = isValid l.PasswordPolicy l.Password

    let numberOfValidPasswords =
        passwordLines
        |> Seq.filter isValidLine
        |> Seq.length

    printfn "%d of %d passwords are valid" numberOfValidPasswords (passwordLines |> Seq.length)

    0
