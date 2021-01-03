open System.IO

let outletJoltage = 0

let getDeviceJoltage adapters = Array.max adapters + 3

let getJoltageDifferences (adapters: int []): Map<int, int> =
    let joltages =
        Array.concat [ [| outletJoltage |]
                       Array.sort adapters
                       [| getDeviceJoltage adapters |] ]

    let differences =
        joltages
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)

    differences |> Array.countBy id |> Map


let getNumbersOfArrangementsFromPrevious (previous: Map<int, int64>) (adapter: int): Map<int, int64> =
    let sum =
        [ (adapter - 1); (adapter - 2); (adapter - 3) ]
        |> List.choose previous.TryFind
        |> Seq.sum

    Map.add adapter sum previous

let getNumberOfPossibleArrangements (adapters: int []): int64 =
    let deviceJoltage = getDeviceJoltage adapters

    adapters
    |> Array.append [| deviceJoltage |]
    |> Array.sort
    |> Array.fold getNumbersOfArrangementsFromPrevious (Map([ outletJoltage, 1L ]))
    |> Map.find deviceJoltage

module Option =
    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some(f x)
        | _ -> None

[<EntryPoint>]
let main argv =
    let adapters =
        "./input.txt"
        |> File.ReadAllLines
        |> Array.map int

    let joltageDifferences = adapters |> getJoltageDifferences

    let (<*>) = Option.apply

    let result =
        (Some(*))
        <*> (joltageDifferences.TryFind 1)
        <*> (joltageDifferences.TryFind 3)

    printfn "Joltage Differences: %A" joltageDifferences
    printfn "Result: %A" result

    let numberOfPossibleArrangements = getNumberOfPossibleArrangements adapters

    printfn "Number of possible arrangements: %d" numberOfPossibleArrangements

    0 // return an integer exit code
