open System
open System.IO
open System.Text.RegularExpressions

type Tile =
    { Id: int
      Content: char [,]
      Borders: TileBorder Set }

and TileBorder = char []

let flatten (A: 'a [,]) = A |> Seq.cast<'a>
let getColumn c (A: _ [,]) = flatten A.[*, c..c] |> Seq.toArray
let getRow r (A: _ [,]) = flatten A.[r..r, *] |> Seq.toArray

let getBorders content =
    [| getRow 0 content
       getRow (content.GetLength 1 - 1) content
       getColumn 0 content
       getColumn (content.GetLength 0 - 1) content |]
    |> Array.collect (fun b -> [| b; Array.rev b |])
    |> Set

let parseTile (tileText: string []) =
    let tileIdPattern = "Tile (?<Id>\d+):"

    let tileId =
        Regex.Match(tileText.[0], tileIdPattern).Groups.["Id"]
            .Value
        |> int

    let contentText = tileText |> Array.skip 1

    let content =
        Array2D.init contentText.[0].Length contentText.Length (fun i j -> contentText.[j].[i])

    { Id = tileId
      Content = content
      Borders = getBorders content }

let lineUp tile1 tile2 =
    tile1 <> tile2
    && Set.intersect tile1.Borders tile2.Borders
       |> (not << Set.isEmpty)

let getCornerIds tileTexts: int Set =
    let tiles = tileTexts |> Array.map parseTile

    let getNeighborTiles tile = tiles |> Array.filter (lineUp tile)

    let isCorner =
        getNeighborTiles >> Seq.length >> ((=) 2)

    tiles
    |> Array.filter isCorner
    |> Array.map (fun t -> t.Id)
    |> Set

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt"

    let tiles =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map
            (fun x ->
                x.Split(Environment.NewLine)
                |> Array.filter (not << String.IsNullOrWhiteSpace))

    let cornerIds = getCornerIds tiles

    let result =
        cornerIds |> Seq.map int64 |> Seq.reduce (*)

    printfn "The product of the corner IDs is %d" result

    0
