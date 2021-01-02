open System.IO

let isValidNextNumber preamble n: bool =
    preamble
    |> Seq.allPairs preamble
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.exists (fun (a, b) -> a + b = n)

let isIndexValid (numbers: int64 []) preambleLength index =
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

let getContiguousSets (xs: 'T []): 'T [] seq =
    seq {
        for startIndex in [ 0 .. xs.Length - 1 ] do
            for endIndex in [ startIndex + 1 .. xs.Length - 1 ] do
                yield xs.[startIndex..endIndex]
    }

let findContiguousSetWithSum (numbers: int64 []) (sum: int64): int64 [] option =
    numbers
    |> getContiguousSets
    |> Seq.tryFind (fun x -> Seq.sum x = sum)

let getEncryptionWeakness (preambleLength: int) (numbers: int64 []) =
    numbers
    |> getFirstInvalidNumber preambleLength
    |> Option.bind (findContiguousSetWithSum numbers)
    |> Option.map (fun x -> Array.min x + Array.max x)

[<EntryPoint>]
let main argv =
    let numbers =
        "./input.txt"
        |> File.ReadAllLines
        |> Array.map int64

    let preamble = 25

    let firstInvalidNumber =
        numbers |> getFirstInvalidNumber preamble

    if firstInvalidNumber.IsSome then
        printfn "The first invalid number is %d" firstInvalidNumber.Value
    else
        printfn "All numbers are valid"

    let encryptionWeakness =
        numbers |> getEncryptionWeakness preamble

    if encryptionWeakness.IsSome then
        printfn "The encryption weakness is %d" encryptionWeakness.Value
    else
        printfn "There is no encryption weakness"


    0 // return an integer exit code
