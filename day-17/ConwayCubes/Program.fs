open System
open System.IO

type CubeState =
    | Active
    | Inactive

let getNeighborCoordinates (x, y, z) =
    seq {
        for i in [ x - 1 .. x + 1 ] do
            for j in [ y - 1 .. y + 1 ] do
                for k in [ z - 1 .. z + 1 ] do
                    if (i, j, k) <> (x, y, z) then
                        yield (i, j, k)
    }
    |> Set

let getNextCubeState coordinate activeCoordinates =
    let state =
        if Set.contains coordinate activeCoordinates then
            Active
        else
            Inactive

    let activeNeighbors =
        coordinate
        |> getNeighborCoordinates
        |> Set.intersect activeCoordinates
        |> Seq.length

    match (state, activeNeighbors) with
    | (Active, 2)
    | (_, 3) -> Active
    | _ -> Inactive

let toCubeState =
    function
    | '.' -> Inactive
    | '#' -> Active
    | c -> failwithf "Invalid CubeState %c" c

let simulateCycle activeCubes =
    activeCubes
    |> Seq.collect getNeighborCoordinates
    |> Set
    |> Set.union activeCubes
    |> Set.filter (fun c -> getNextCubeState c activeCubes = Active)

let getActiveCubes (initialState: string array) =
    seq {
        for x in [ 0 .. initialState.[0].Length - 1 ] do
            for y in [ 0 .. initialState.Length - 1 ] do
                let state = initialState.[y].[x] |> toCubeState
                if (state = Active) then yield (x, y, 0)
    }
    |> Set

let getNumberOfActiveCubes (cycles: int) (initialState: string array): int =
    let initiallyActiveCubes = getActiveCubes initialState

    let finalState =
        [ 1 .. cycles ]
        |> Seq.fold (fun s _ -> simulateCycle s) initiallyActiveCubes

    finalState |> Seq.length

[<EntryPoint>]
let main argv =
    let initialState = File.ReadAllLines "./input.txt"

    let numberofActiveCubes = getNumberOfActiveCubes 6 initialState
    printfn "After the sixth cycle %d cubes are left in the active state" numberofActiveCubes

    0
