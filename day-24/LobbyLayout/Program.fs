open System.IO

type Direction =
    | East
    | Southeast
    | Southwest
    | West
    | Northwest
    | Northeast

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec parseInstruction =
    function
    | "" -> []
    | Prefix "e" rest -> [ East ] @ parseInstruction rest
    | Prefix "se" rest -> [ Southeast ] @ parseInstruction rest
    | Prefix "sw" rest -> [ Southwest ] @ parseInstruction rest
    | Prefix "w" rest -> [ West ] @ parseInstruction rest
    | Prefix "nw" rest -> [ Northwest ] @ parseInstruction rest
    | Prefix "ne" rest -> [ Northeast ] @ parseInstruction rest
    | _ -> failwith "Invalid instruction"

let move (x, y) =
    function
    | East -> (x + 2, y)
    | Southeast -> (x + 1, y - 1)
    | Southwest -> (x - 1, y - 1)
    | West -> (x - 2, y)
    | Northwest -> (x - 1, y + 1)
    | Northeast -> (x + 1, y + 1)

let getCoordinates = Seq.fold move (0, 0)

let applyInstruction blackTiles instruction =
    let coordinates = getCoordinates instruction

    if blackTiles |> Set.contains coordinates then
        blackTiles |> Set.remove coordinates
    else
        blackTiles |> Set.add coordinates

let getBlackTiles (instructionTexts: string []): int =
    instructionTexts
    |> Array.map parseInstruction
    |> Seq.fold applyInstruction Set.empty
    |> Seq.length


[<EntryPoint>]
let main argv =
    let instructions = File.ReadAllLines "./input.txt"

    let blackTiles = getBlackTiles instructions
    printfn "There are %d tiles left with the black side up" blackTiles

    0
