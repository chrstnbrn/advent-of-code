open System
open System.IO
open System.Text.RegularExpressions

type PasswordPolicy = { Letter: char; Min: int; Max: int; }
type PasswordLine = { PasswordPolicy: PasswordPolicy; Password: string;}

let getPasswordLineFromMatch (matchResult: Match) =
    let letter = char matchResult.Groups.["Letter"].Value
    let min = int matchResult.Groups.["Min"].Value
    let max = int matchResult.Groups.["Max"].Value
    let password = matchResult.Groups.["Password"].Value

    let passwordPolicy = { Letter = letter; Min = min; Max = max;}
    { PasswordPolicy = passwordPolicy; Password = password }

let parseLine (line: string): PasswordLine option =
    let pattern = "^(?<Min>\d+)-(?<Max>\d+) (?<Letter>\w): (?<Password>.*)$"
    let result = Regex.Match(line, pattern)
    if result.Success
        then getPasswordLineFromMatch result |> Some
        else None

let isValid (policy: PasswordPolicy) (password: string): bool =
    let letterOccurences = password |> Seq.filter ((=) policy.Letter) |> Seq.length
    policy.Min <= letterOccurences && letterOccurences <= policy.Max

[<EntryPoint>]
let main argv =
    let inputPath = Path.Combine(Environment.CurrentDirectory, "input.txt")
    let passwordLines = inputPath |> File.ReadAllLines |> Seq.map parseLine |> Seq.choose id

    let isValidLine l = isValid l.PasswordPolicy l.Password
    let numberOfValidPasswords = passwordLines |> Seq.filter isValidLine |> Seq.length

    printfn "%d of %d passwords are valid" numberOfValidPasswords (passwordLines |> Seq.length)

    0
