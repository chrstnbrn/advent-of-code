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

type Direction =
    | North
    | South
    | East
    | West

type ShipState =
    { Position: Position
      Direction: Direction }

let getDegree =
    function
    | 90 -> Degree90
    | 180 -> Degree180
    | 270 -> Degree270
    | d -> failwithf "Invalid degree: %d" d

let getAction value =
    function
    | 'N' -> Instruction.North value
    | 'S' -> Instruction.South value
    | 'E' -> Instruction.East value
    | 'W' -> Instruction.West value
    | 'L' -> Instruction.Left <| getDegree value
    | 'R' -> Instruction.Right <| getDegree value
    | 'F' -> Instruction.Forward value
    | a -> failwithf "Invalid action: %c" a

let getNavigationInstruction (text: string) =
    let value = text.[1..] |> int
    getAction value text.[0]

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

let move state instruction =
    match instruction with
    | Instruction.North v -> moveY v state
    | Instruction.South v -> moveY -v state
    | Instruction.East v -> moveX v state
    | Instruction.West v -> moveX -v state
    | Instruction.Left v -> turnLeft v state
    | Instruction.Right v -> turnRight v state
    | Instruction.Forward v -> moveForward v state

let calculateManhattanDistance (x1, y1) (x2, y2) =
    let dx: int = x1 - x2
    let dy: int = y1 - y2
    Math.Abs dx + Math.Abs dy

let getManhattanDistance (navigationInstructionTexts: string array) =
    let initialState = { Position = (0, 0); Direction = East }

    let endState =
        navigationInstructionTexts
        |> Array.map getNavigationInstruction
        |> Seq.fold move initialState

    calculateManhattanDistance initialState.Position endState.Position

[<EntryPoint>]
let main argv =
    let navigationInstructions = File.ReadAllLines "./input.txt"

    let manhattanDistance =
        getManhattanDistance navigationInstructions

    printfn "The Manhattan distance between the ship's start and end position is %d" manhattanDistance

    0
