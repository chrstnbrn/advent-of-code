module Tile

type T = { Id: int; Content: char [,] }

type private Side =
    | Top
    | Right
    | Bottom
    | Left

let private topBorder tile =
    tile.Content |> Array2D.getRows |> Array.head

let private rightBorder tile =
    tile.Content |> Array2D.getColumns |> Array.last

let private bottomBorder tile =
    tile.Content |> Array2D.getRows |> Array.last

let private leftBorder tile =
    tile.Content |> Array2D.getColumns |> Array.head

let private getBorder =
    function
    | Top -> topBorder
    | Right -> rightBorder
    | Bottom -> bottomBorder
    | Left -> leftBorder

let getTileOrientations tile =
    tile.Content
    |> Array2D.getOrientations
    |> Array.map (fun c -> { tile with Content = c })

let private getOppositeSide =
    function
    | Top -> Bottom
    | Bottom -> Top
    | Left -> Right
    | Right -> Left

let private tryGetNeighbor side (tiles: T []) tile =
    let tileBorder = getBorder side tile
    let oppositeSide = getOppositeSide side

    tiles
    |> Seq.filter (fun t -> t.Id <> tile.Id)
    |> Seq.collect getTileOrientations
    |> Seq.tryFind (fun t -> getBorder oppositeSide t = tileBorder)

let getBottomNeighbor tiles tile =
    tryGetNeighbor Bottom tiles tile |> Option.get

let getRightNeighbor tiles tile =
    tryGetNeighbor Right tiles tile |> Option.get

let hasBottomNeighbor tiles tile =
    tryGetNeighbor Bottom tiles tile |> Option.isSome

let hasRightNeighbor tiles tile =
    tryGetNeighbor Right tiles tile |> Option.isSome

let getNeighborTiles tiles tile =
    [ Top; Bottom; Right; Left ]
    |> Seq.choose (fun side -> tryGetNeighbor side tiles tile)

let removeBorder tile =
    tile.Content.[1..(Array2D.length1 tile.Content) - 2, 1..(Array2D.length2 tile.Content) - 2]
