open System
open System.IO

type Degree =
    | Degree90
    | Degree180
    | Degree270

type Instruction =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of Degree
    | Right of Degree
    | Forward of int

type Position = int * int

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let subtract (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let multiply a (x, y) = (a * x, a * y)

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

    let moveInDirection d state =
        { state with
              Position = add state.Position d }

    let moveForward d state =
        match state.Direction with
        | North -> state |> moveInDirection (0, d)
        | South -> state |> moveInDirection (0, -d)
        | East -> state |> moveInDirection (d, 0)
        | West -> state |> moveInDirection (-d, 0)

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

    let move =
        function
        | Instruction.North v -> moveInDirection (0, v)
        | Instruction.South v -> moveInDirection (0, -v)
        | Instruction.East v -> moveInDirection (v, 0)
        | Instruction.West v -> moveInDirection (-v, 0)
        | Instruction.Left v -> turnLeft v
        | Instruction.Right v -> turnRight v
        | Instruction.Forward v -> moveForward v

    let getManhattanDistance (navigationInstructionTexts: string array) =
        let initialState = { Position = (0, 0); Direction = East }

        let endState =
            navigationInstructionTexts
            |> Array.map getNavigationInstruction
            |> Seq.fold (fun state instruction -> move instruction state) initialState

        calculateManhattanDistance initialState.Position endState.Position

module Part2 =
    type ShipState2 =
        { ShipPosition: Position
          WaypointPosition: Position }

    let moveWaypoint d state =
        { state with
              WaypointPosition = add state.WaypointPosition d }

    let moveShipToWaypoint n state =
        let (+), (-), (*) = (add, subtract, multiply)

        let d =
            n * (state.WaypointPosition - state.ShipPosition)

        { ShipPosition = state.ShipPosition + d
          WaypointPosition = state.WaypointPosition + d }

    let rotateWaypoint degree state =
        let degreeToRadian d = d * Math.PI / 180.0
        let sin = degreeToRadian >> Math.Sin >> int
        let cos = degreeToRadian >> Math.Cos >> int

        let rotateAroundOrigin d (x, y) =
            (x * cos d - y * sin d, x * sin d + y * cos d)

        let newWaypointPosition =
            subtract state.WaypointPosition state.ShipPosition
            |> rotateAroundOrigin degree
            |> add state.ShipPosition

        { state with
              WaypointPosition = newWaypointPosition }

    let degreeNumber =
        function
        | Degree90 -> 90.0
        | Degree180 -> 180.0
        | Degree270 -> 270.0

    let move =
        function
        | North v -> moveWaypoint (0, v)
        | South v -> moveWaypoint (0, -v)
        | East v -> moveWaypoint (v, 0)
        | West v -> moveWaypoint (-v, 0)
        | Left v -> rotateWaypoint (degreeNumber v)
        | Right v -> rotateWaypoint -(degreeNumber v)
        | Forward v -> moveShipToWaypoint v

    let getManhattanDistance (navigationInstructionTexts: string array) =
        let initialState =
            { ShipPosition = (0, 0)
              WaypointPosition = (10, 1) }

        let endState =
            navigationInstructionTexts
            |> Array.map getNavigationInstruction
            |> Seq.fold (fun state instruction -> move instruction state) initialState

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
