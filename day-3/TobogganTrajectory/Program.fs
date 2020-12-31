open System
open System.IO

type MapTemplate = string array
type Map = (char seq) array
type Position = { X: int; Y: int }
type Slope = { Right: int; Down: int }

let move (position: Position) (slope: Slope): Position =
    { X = position.X + slope.Right
      Y = position.Y + slope.Down }

let isTree (map: Map) (position: Position): bool =
    let content =
        map.[position.Y - 1] |> Seq.item (position.X - 1)

    content = '#'

let isValidPosition (map: Map) (position: Position) =
    position.X >= 1
    && position.Y >= 1
    && position.Y <= map.Length

let getTrajectory (map: Map) (startPosition: Position) (slope: Slope): Position seq =
    let rec trajectory p =
        seq {
            yield p
            yield! trajectory (move p slope)
        }

    startPosition
    |> trajectory
    |> Seq.takeWhile (isValidPosition map)

let rec cycle xs =
    seq {
        yield! xs
        yield! cycle xs
    }

let createMap (m: string array): Map = m |> Array.map cycle

let countTrees (mapTemplate: MapTemplate) (slope: Slope): int =
    let map = mapTemplate |> createMap
    let startPosition = { X = 1; Y = 1 }
    let trajectory = getTrajectory map startPosition slope

    trajectory
    |> Seq.filter (isTree map)
    |> Seq.length


[<EntryPoint>]
let main argv =
    let inputPath =
        Path.Combine(Environment.CurrentDirectory, "input.txt")

    let mapTemplate = inputPath |> File.ReadAllLines

    let slopes =
        [ { Right = 1; Down = 1 }
          { Right = 3; Down = 1 }
          { Right = 5; Down = 1 }
          { Right = 7; Down = 1 }
          { Right = 1; Down = 2 } ]

    let results =
        slopes
        |> List.map (fun s -> (s, countTrees mapTemplate s))

    printfn "%A" results

    let product =
        results
        |> List.map (snd >> int64)
        |> List.fold (*) 1L

    printfn "Result: %d" product

    0
