open System.IO

let isValidNextNumber preamble n: bool =
    preamble
    |> Seq.allPairs preamble
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.exists (fun (a, b) -> a + b = n)

let isIndexValid (numbers: int64 array) preambleLength index =
    let number = numbers.[index]

    let preamble =
        numbers.[(index - preambleLength)..(index - 1)]

    preamble.Length < preambleLength
    || isValidNextNumber preamble number


let getFirstInvalidNumber (preambleLength: int) (numbers: int64 array) =
    numbers
    |> Seq.indexed
    |> Seq.tryFind (fun (i, _) -> not (isIndexValid numbers preambleLength i))
    |> Option.map snd

[<EntryPoint>]
let main argv =
    let numbers =
        "./input.txt"
        |> File.ReadAllLines
        |> Array.map int64

    let firstInvalidNumber = numbers |> getFirstInvalidNumber 25

    if firstInvalidNumber.IsSome then
        printfn "The first invalid number is %d" firstInvalidNumber.Value
    else
        printfn "All numbers are valid"

    0 // return an integer exit code
