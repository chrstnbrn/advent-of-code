open System
open System.IO
open System.Text.RegularExpressions

let parseTile (tileText: string []) =
    let tileIdPattern = "Tile (?<Id>\d+):"

    let tileId =
        Regex.Match(tileText.[0], tileIdPattern).Groups.["Id"]
            .Value
        |> int

    let content = tileText |> Array.skip 1 |> array2D

    { Tile.Id = tileId
      Tile.Content = content }

let getCornerTiles tiles =
    let isCorner =
        (Tile.getNeighborTiles tiles)
        >> Seq.length
        >> ((=) 2)

    tiles |> Array.filter isCorner

let getTopLeftTile tiles =
    getCornerTiles tiles
    |> Seq.take 1
    |> Seq.collect Tile.getTileOrientations
    |> Seq.find
        (fun tile ->
            Tile.hasRightNeighbor tiles tile
            && Tile.hasBottomNeighbor tiles tile)

let placeNextTile grid unusedTiles (row, col) =
    let tile =
        match row, col with
        | 0, 0 -> getTopLeftTile unusedTiles
        | _, 0 ->
            let tileAbove = Array2D.get grid (row - 1) 0
            Tile.getBottomNeighbor unusedTiles tileAbove
        | _, _ ->
            let lastTile = Array2D.get grid row (col - 1)
            Tile.getRightNeighbor unusedTiles lastTile

    let newUnusedTiles =
        unusedTiles
        |> Array.filter (fun t -> t.Id <> tile.Id)

    let newGrid =
        grid
        |> Array2D.mapi (fun i j x -> if (i, j) = (row, col) then tile else x)

    (newGrid, newUnusedTiles)

let solve tiles =
    let size =
        tiles |> Array.length |> double |> sqrt |> int

    let emptyGrid = Array2D.zeroCreate<Tile.T> size size

    emptyGrid
    |> Array2D.mapi (fun i j _ -> (i, j))
    |> Array2D.flatten
    |> Seq.fold (fun (g, unusedTiles) c -> placeNextTile g unusedTiles c) (emptyGrid, tiles)
    |> fst

let assemble =
    Array2D.map Tile.removeBorder >> Array2D.combine

let replaceMask mask grid =
    let maskCoordinates =
        mask
        |> Array2D.mapi (fun i j x -> if x = '#' then Some(i, j) else None)
        |> Array2D.flatten
        |> Seq.choose id
        |> Seq.toList

    let result = Array2D.copy grid

    for row in 0 .. (Array2D.length1 grid) - (Array2D.length1 mask) do
        for col in 0 .. (Array2D.length2 grid) - (Array2D.length2 mask) do
            let coordinates =
                maskCoordinates
                |> List.map (fun (i, j) -> (row + i, col + j))

            if List.forall (fun (i, j) -> result.[i, j] = '#') coordinates then
                for (i, j) in coordinates do
                    result.[i, j] <- 'o'

    result

let countNotPartOfSeaMonster habitat =
    let seaMonster =
        array2D [| "                  # "
                   "#    ##    ##    ###"
                   " #  #  #  #  #  #   " |]

    habitat
    |> replaceMask seaMonster
    |> Array2D.flatten
    |> Seq.filter ((=) '#')
    |> Seq.length


let calculateWaterRoughness =
    Array2D.getOrientations
    >> Seq.map countNotPartOfSeaMonster
    >> Seq.min

let getCornerIds tileTexts: int Set =
    tileTexts
    |> Array.map parseTile
    |> getCornerTiles
    |> Array.map (fun t -> t.Id)
    |> Set

let getWaterRoughness tileTexts: int =
    tileTexts
    |> Array.map parseTile
    |> solve
    |> assemble
    |> calculateWaterRoughness

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt"

    let tiles =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map
            (fun x ->
                x.Split(Environment.NewLine)
                |> Array.filter (not << String.IsNullOrWhiteSpace))

    let cornerIds = getCornerIds tiles

    let result =
        cornerIds |> Seq.map int64 |> Seq.reduce (*)

    printfn "The product of the corner IDs is %d" result

    let waterRoughness = getWaterRoughness tiles
    printfn "The water roughness is %d" waterRoughness

    0
