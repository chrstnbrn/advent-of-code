open System
open System.IO;

let findTwoEntriesWithSum sum entries =
    let indexedEntries = entries |> Seq.indexed
    let pairs = seq {
        for (ix, x) in indexedEntries do
        for (iy, y) in indexedEntries |> Seq.skip (ix + 1) do
        x, y
    }
    pairs |> Seq.tryFind (fun (x, y) -> x + y = sum)

let findThreeEntriesWithSum sum entries =
    let indexedEntries = entries |> Seq.indexed
    let triples = seq {
        for (ix, x) in indexedEntries do
        for (iy, y) in indexedEntries |> Seq.skip (ix + 1) do
        for (iz, z) in indexedEntries |> Seq.skip (iy + 1) do
        x, y, z
    }
    triples |> Seq.tryFind (fun (x, y, z) -> x + y + z = sum)

[<EntryPoint>]
let main argv =
    let inputPath = Path.Combine(Environment.CurrentDirectory, "input.txt")
    let entries = inputPath |> File.ReadAllLines |> Seq.map int

    let sum = 2020
    let resultForTwo = entries |> findTwoEntriesWithSum sum
    let resultForThree = entries |> findThreeEntriesWithSum sum

    match resultForTwo with
      | Some (x, y) -> printfn "%d + %d = %d. %d * %d = %d" x y sum x y (x * y)
      | _ -> printfn "No two entries found that have a sum of %d." sum

    match resultForThree with
      | Some (x, y, z) -> printfn "%d + %d + %d = %d. %d * %d * %d = %d" x y z sum x y z (x * y * z)
      | _ -> printfn "No three entries found that have a sum of %d." sum

    0 // return an integer exit code
