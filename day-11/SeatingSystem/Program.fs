open System
open System.IO

type Position =
    | Floor
    | EmptySeat
    | OccupiedSeat

type SeatLayout = Position [,]

let tryGet (seatLayout: SeatLayout) (i, j) =
    let isValidDimensionIndex dimension index =
        index >= 0
        && index < seatLayout.GetLength dimension

    if isValidDimensionIndex 0 i
       && isValidDimensionIndex 1 j then
        Some(Array2D.get seatLayout i j)
    else
        None

let getAdjacentPositions i j seatLayout =
    List.allPairs [ i - 1 .. i + 1 ] [ j - 1 .. j + 1 ]
    |> List.except [ (i, j) ]
    |> List.choose (tryGet seatLayout)

let rec getPositionsInDirection seatLayout (i, j) (di, dj) =
    seq {
        let newIndexes = (i + di, j + dj)
        let position = tryGet seatLayout newIndexes

        if position.IsSome then
            yield position.Value
            yield! getPositionsInDirection seatLayout newIndexes (di, dj)
    }

let getSeatsWithinSight i j seatLayout =
    let directions =
        List.allPairs [ -1 .. 1 ] [ -1 .. 1 ]
        |> List.except [ (0, 0) ]

    directions
    |> List.map (getPositionsInDirection seatLayout (i, j))
    |> List.choose (Seq.tryFind (fun p -> p = EmptySeat || p = OccupiedSeat))

let count x = Seq.filter ((=) x) >> Seq.length

let getNextPosition1 seatLayout i j =
    let position = Array2D.get seatLayout i j

    let adjacentOccupiedSeats =
        seatLayout
        |> getAdjacentPositions i j
        |> count OccupiedSeat

    match position with
    | EmptySeat when adjacentOccupiedSeats = 0 -> OccupiedSeat
    | OccupiedSeat when adjacentOccupiedSeats >= 4 -> EmptySeat
    | _ -> position

let getNextPosition2 seatLayout i j =
    let position = Array2D.get seatLayout i j

    let occupiedSeatsWithinSight =
        seatLayout
        |> getSeatsWithinSight i j
        |> count OccupiedSeat

    match position with
    | EmptySeat when occupiedSeatsWithinSight = 0 -> OccupiedSeat
    | OccupiedSeat when occupiedSeatsWithinSight >= 5 -> EmptySeat
    | _ -> position

let getNextSeatLayout getNextPosition seatLayout =
    seatLayout
    |> Array2D.mapi (fun i j _ -> getNextPosition seatLayout i j)

let getSeatLayout (seatLayoutTexts: string array): SeatLayout =
    let getPosition =
        function
        | '.' -> Floor
        | 'L' -> EmptySeat
        | '#' -> OccupiedSeat
        | p -> raise (ArgumentException("Invalid position " + p.ToString()))

    Array2D.init seatLayoutTexts.Length seatLayoutTexts.[0].Length (fun i j -> seatLayoutTexts.[i].[j] |> getPosition)

let getFinalSeatLayout getNextPosition seatLayoutTexts =
    let rec generateSeatLayouts s =
        seq {
            yield s

            yield!
                s
                |> getNextSeatLayout getNextPosition
                |> generateSeatLayouts
        }

    seatLayoutTexts
    |> getSeatLayout
    |> generateSeatLayouts
    |> Seq.pairwise
    |> Seq.takeWhile (fun (a, b) -> a <> b)
    |> Seq.map snd
    |> Seq.last

let getOccupiedSeats = Seq.cast<Position> >> count OccupiedSeat

let getNumberOfOccupiedSeats1 =
    getFinalSeatLayout getNextPosition1
    >> getOccupiedSeats

let getNumberOfOccupiedSeats2 =
    getFinalSeatLayout getNextPosition2
    >> getOccupiedSeats

[<EntryPoint>]
let main argv =
    let seatLayout = "./input.txt" |> File.ReadAllLines

    printfn "When applying the rule of Part 1 %d seats end up occupied" (getNumberOfOccupiedSeats1 seatLayout)
    printfn "When applying the rule of Part 2 %d seats end up occupied" (getNumberOfOccupiedSeats2 seatLayout)

    0
