open System
open System.IO;

let findEntriesWithSum sum entries =
    let withoutIndex index = Seq.indexed >> Seq.filter (fun (i, _) -> i <> index) >> Seq.map (fun (_, x) -> x)
    let pairs = entries |> Seq.indexed |> Seq.collect (fun (i, x) -> entries |> withoutIndex i |> Seq.map (fun y -> x, y) )
    pairs |> Seq.tryFind (fun (x, y) -> x + y = sum)

[<EntryPoint>]
let main argv =
    let inputPath = Path.Combine(Environment.CurrentDirectory, "input.txt")
    let entries = inputPath |> File.ReadAllLines |> Seq.map int
    let sum = 2020
    let result = entries |> findEntriesWithSum sum

    match result with
      | Some (x, y) -> printfn "%d + %d = %d. %d * %d = %d" x y sum x y (x * y)
      | _ -> printfn "No matching entries found."

    0 // return an integer exit code
