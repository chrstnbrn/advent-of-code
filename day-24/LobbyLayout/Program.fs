open System.IO

type Direction =
    | East
    | Southeast
    | Southwest
    | West
    | Northwest
    | Northeast

type TileColor =
    | Black
    | White

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

let getTile = Seq.fold move (0, 0)

let getTileColor blackTiles tile =
    if blackTiles |> Set.contains tile then
        Black
    else
        White

let flipToWhite blackTiles tile = blackTiles |> Set.remove tile
let flipToBlack blackTiles tile = blackTiles |> Set.add tile

let applyInstruction blackTiles instruction =
    let tile = getTile instruction

    match tile |> getTileColor blackTiles with
    | Black -> tile |> flipToWhite blackTiles
    | White -> tile |> flipToBlack blackTiles

let getNeighbors coordinates =
    let directions =
        [ East
          Southeast
          Southwest
          West
          Northwest
          Northeast ]

    directions |> List.map (move coordinates)

let getAllTiles = Seq.collect getNeighbors >> Set.ofSeq

let getNextTileColor blackTiles tile =
    let blackNeighbors =
        tile
        |> getNeighbors
        |> Seq.filter (getTileColor blackTiles >> (=) Black)
        |> Seq.length

    match tile |> getTileColor blackTiles with
    | Black when blackNeighbors = 0 || blackNeighbors > 2 -> White
    | White when blackNeighbors = 2 -> Black
    | currentColor -> currentColor

let flipTiles blackTiles =
    blackTiles
    |> getAllTiles
    |> Seq.filter (getNextTileColor blackTiles >> (=) Black)
    |> Set.ofSeq

let getBlackTiles (instructionTexts: string []): int =
    instructionTexts
    |> Array.map parseInstruction
    |> Seq.fold applyInstruction Set.empty
    |> Seq.length

let getBlackTilesAfterDays (instructionTexts: string []) (days: int): int =
    let initiallyBlackTiles =
        instructionTexts
        |> Array.map parseInstruction
        |> Seq.fold applyInstruction Set.empty

    [ 1 .. days ]
    |> Seq.fold (fun blackTiles _ -> flipTiles blackTiles) initiallyBlackTiles
    |> Seq.length


[<EntryPoint>]
let main argv =
    let instructions = File.ReadAllLines "./input.txt"

    getBlackTiles instructions
    |> printfn "There are %d tiles left with the black side up"

    getBlackTilesAfterDays instructions 100
    |> printfn "After 100 days %d tiles will be black"

    0
