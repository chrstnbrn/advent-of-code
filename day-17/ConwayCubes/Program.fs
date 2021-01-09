open System.IO

type CubeState =
    | Active
    | Inactive

let rec getNeighborCoordinatesRec =
    function
    | [||] -> List.singleton [||]
    | xs ->
        let head = Array.head xs
        let heads = [ head - 1; head; head + 1 ]

        xs
        |> Array.tail
        |> getNeighborCoordinatesRec
        |> List.allPairs heads
        |> List.map (fun (h, t) -> Array.append [| h |] t)

let getNeighborCoordinates coordinate =
    coordinate
    |> getNeighborCoordinatesRec
    |> Seq.except [ coordinate ]
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

let getActiveCubes (initialState: string array) (dimensions: int) =
    seq {
        for x in [ 0 .. initialState.[0].Length - 1 ] do
            for y in [ 0 .. initialState.Length - 1 ] do
                let state = initialState.[y].[x] |> toCubeState

                if (state = Active) then
                    yield Array.append [| x; y |] (Array.zeroCreate (dimensions - 2))
    }
    |> Set

let getNumberOfActiveCubes (initialState: string array) (dimensions: int) (cycles: int): int =
    let initiallyActiveCubes = getActiveCubes initialState dimensions

    let finalState =
        [ 1 .. cycles ]
        |> Seq.fold (fun s _ -> simulateCycle s) initiallyActiveCubes

    finalState |> Seq.length

[<EntryPoint>]
let main argv =
    let initialState = File.ReadAllLines "./input.txt"

    let numberofActiveCubesIn3D = getNumberOfActiveCubes initialState 3 6

    printfn
        "After the sixth cycle in a 3-dimensional space %d cubes are left in the active state"
        numberofActiveCubesIn3D

    let numberofActiveCubesIn4D = getNumberOfActiveCubes initialState 4 6

    printfn
        "After the sixth cycle in a 4-dimensional space %d cubes are left in the active state"
        numberofActiveCubesIn4D

    0
