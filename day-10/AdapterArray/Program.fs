open System.IO

let getJoltageDifferences (adapters: int []): Map<int, int> =
    let sortedAdapaters = adapters |> Array.sort
    let outletJoltage = 0
    let deviceJoltage = Array.last sortedAdapaters + 3

    let joltages =
        Array.concat [ [| outletJoltage |]
                       sortedAdapaters
                       [| deviceJoltage |] ]

    let differences =
        joltages
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)

    differences |> Array.countBy id |> Map

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

    0 // return an integer exit code
