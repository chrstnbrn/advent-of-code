open System
open System.IO;

let rec getCombinations n l =
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (getCombinations (k-1) xs) @ getCombinations k xs

let findEntriesWithSum sum count entries =
    entries |> Seq.toList |> getCombinations count |> Seq.tryFind (fun x -> Seq.sum x = sum)

let printResult sum count result =
    let product = Seq.fold (( * )) 1
    match result with
      | Some x -> printfn "Sum of %A = %d. Product of %A = %d" x sum x (product x)
      | _ -> printfn "No %d entries found that have a sum of %d." count sum

[<EntryPoint>]
let main argv =
    let inputPath = Path.Combine(Environment.CurrentDirectory, "input.txt")
    let entries = inputPath |> File.ReadAllLines |> Seq.map int

    entries |> findEntriesWithSum 2020 2 |> printResult 2020 3
    entries |> findEntriesWithSum 2020 3 |> printResult 2020 3

    0
