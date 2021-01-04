open System
open System.IO

type Distance = int

type Degree =
    | Degree90
    | Degree180
    | Degree270

type Instruction =
    | North of Distance
    | South of Distance
    | East of Distance
    | West of Distance
    | Left of Degree
    | Right of Degree
    | Forward of Distance

type Position = int * int

let getDegree =
    function
    | 90 -> Degree90
    | 180 -> Degree180
    | 270 -> Degree270
    | d -> failwithf "Invalid degree: %d" d

let getNavigationInstruction (text: string) =
    let value = text.[1..] |> int

    match text.[0] with
    | 'N' -> North value
    | 'S' -> South value
    | 'E' -> East value
    | 'W' -> West value
    | 'L' -> Left <| getDegree value
    | 'R' -> Right <| getDegree value
    | 'F' -> Forward value
    | a -> failwithf "Invalid action: %c" a

let rec times n f =
    if n < 1 then
        id
    else
        f >> times (n - 1) f

let getTurns =
    function
    | Degree90 -> 1
    | Degree180 -> 2
    | Degree270 -> 3

let calculateManhattanDistance (x1, y1) (x2, y2) =
    let dx: int = x1 - x2
    let dy: int = y1 - y2
    Math.Abs dx + Math.Abs dy

module Part1 =
    type Direction =
        | North
        | South
        | East
        | West

    type ShipState1 =
        { Position: Position
          Direction: Direction }

    let moveY dx state =
        { state with
              Position = (fst state.Position, snd state.Position + dx) }

    let moveX dy state =
        { state with
              Position = (fst state.Position + dy, snd state.Position) }

    let moveForward d state =
        match state.Direction with
        | North -> moveY d state
        | South -> moveY -d state
        | East -> moveX d state
        | West -> moveX -d state

    let turnRight degree state =
        let turnRightOnce =
            function
            | North -> East
            | East -> South
            | South -> West
            | West -> North

        let turns = getTurns degree

        let newDirection =
            state.Direction |> times turns turnRightOnce

        { state with Direction = newDirection }

    let turnLeft degree state =
        let turnLeftOnce =
            function
            | North -> West
            | East -> North
            | South -> East
            | West -> South

        let turns = getTurns degree

        let newDirection =
            state.Direction |> times turns turnLeftOnce

        { state with Direction = newDirection }

    let move (state: ShipState1) instruction =
        match instruction with
        | Instruction.North v -> moveY v state
        | Instruction.South v -> moveY -v state
        | Instruction.East v -> moveX v state
        | Instruction.West v -> moveX -v state
        | Instruction.Left v -> turnLeft v state
        | Instruction.Right v -> turnRight v state
        | Instruction.Forward v -> moveForward v state

    let getManhattanDistance (navigationInstructionTexts: string array) =
        let initialState = { Position = (0, 0); Direction = East }

        let endState =
            navigationInstructionTexts
            |> Array.map getNavigationInstruction
            |> Seq.fold move initialState

        calculateManhattanDistance initialState.Position endState.Position

module Part2 =
    type ShipState2 =
        { ShipPosition: Position
          WaypointPosition: Position }

    let moveY dx state =
        { state with
              ShipPosition = (fst state.ShipPosition, snd state.ShipPosition + dx) }

    let moveX dy state =
        { state with
              ShipPosition = (fst state.ShipPosition + dy, snd state.ShipPosition) }

    let moveForward d state = state

    let turnRight degree state = state

    let turnLeft degree state = state

    let move state instruction =
        match instruction with
        | North v -> moveY v state
        | South v -> moveY -v state
        | East v -> moveX v state
        | West v -> moveX -v state
        | Left v -> turnLeft v state
        | Right v -> turnRight v state
        | Forward v -> moveForward v state

    let getManhattanDistance (navigationInstructionTexts: string array) =
        let initialState =
            { ShipPosition = (0, 0)
              WaypointPosition = (10, 1) }

        let endState =
            navigationInstructionTexts
            |> Array.map getNavigationInstruction
            |> Seq.fold move initialState

        calculateManhattanDistance initialState.ShipPosition endState.ShipPosition

[<EntryPoint>]
let main argv =
    let navigationInstructions = File.ReadAllLines "./input.txt"

    navigationInstructions
    |> Part1.getManhattanDistance
    |> printfn "Part 1: The Manhattan distance between the ship's start and end position is %d"

    navigationInstructions
    |> Part2.getManhattanDistance
    |> printfn "Part 2: The Manhattan distance between the ship's start and end position is %d"

    0
