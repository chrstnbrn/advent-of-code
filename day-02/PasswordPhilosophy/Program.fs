open System
open System.IO
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type PasswordPolicy =
    { Letter: char
      First: int
      Second: int }

type PasswordLine =
    { PasswordPolicy: PasswordPolicy
      Password: string }

type IsValidPassword = PasswordPolicy -> string -> bool

type PasswordLineRegex = Regex<"^(?<First>\d+)-(?<Second>\d+) (?<Letter>\w): (?<Password>.*)$">

let parseLine (line: string): PasswordLine option =
    PasswordLineRegex().TryTypedMatch(line)
    |> Option.map
        (fun x ->
            { PasswordPolicy =
                  { Letter = x.Letter.AsChar
                    First = x.First.AsInt
                    Second = x.Second.AsInt }
              Password = x.Password.Value })

let isValidOld: IsValidPassword =
    fun policy password ->
        let letterOccurences =
            password
            |> Seq.filter ((=) policy.Letter)
            |> Seq.length

        policy.First <= letterOccurences
        && letterOccurences <= policy.Second

let xor a b =
    match a, b with
    | true, false -> true
    | false, true -> true
    | _, _ -> false

let isValidNew: IsValidPassword =
    fun policy password ->
        let letterFirst = password.[policy.First - 1]
        let letterSecond = password.[policy.Second - 1]
        xor (letterFirst = policy.Letter) (letterSecond = policy.Letter)

let countValidPasswords isValidPassword passwordLines =
    let isValidLine l =
        isValidPassword l.PasswordPolicy l.Password

    passwordLines
    |> Seq.filter isValidLine
    |> Seq.length

[<EntryPoint>]
let main argv =
    let inputPath =
        Path.Combine(Environment.CurrentDirectory, "input.txt")

    let passwordLines =
        inputPath
        |> File.ReadAllLines
        |> Seq.map parseLine
        |> Seq.choose id

    let numberOfValidPasswordsOld =
        countValidPasswords isValidOld passwordLines

    let numberOfValidPasswordsNew =
        countValidPasswords isValidNew passwordLines

    printfn
        "%d of %d passwords are valid according to the old interpretation of the policy"
        numberOfValidPasswordsOld
        (passwordLines |> Seq.length)

    printfn
        "%d of %d passwords are valid according to the new interpretation of the policy"
        numberOfValidPasswordsNew
        (passwordLines |> Seq.length)

    0
