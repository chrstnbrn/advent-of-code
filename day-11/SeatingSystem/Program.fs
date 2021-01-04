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

let count x = Seq.filter ((=) x) >> Seq.length

let getNextPosition seatLayout i j =
    let position = Array2D.get seatLayout i j

    let adjacentOccupiedSeats =
        seatLayout
        |> getAdjacentPositions i j
        |> count OccupiedSeat

    match position with
    | EmptySeat when adjacentOccupiedSeats = 0 -> OccupiedSeat
    | OccupiedSeat when adjacentOccupiedSeats >= 4 -> EmptySeat
    | _ -> position

let getNextSeatLayout seatLayout =
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

let getNumberOfOccupiedSeats (seatLayoutTexts: string array) =
    let rec generateSeatLayouts s =
        seq {
            yield s
            yield! s |> getNextSeatLayout |> generateSeatLayouts
        }

    let finalSeatLayout =
        seatLayoutTexts
        |> getSeatLayout
        |> generateSeatLayouts
        |> Seq.pairwise
        |> Seq.takeWhile (fun (a, b) -> a <> b)
        |> Seq.map snd
        |> Seq.last

    finalSeatLayout
    |> Seq.cast<Position>
    |> count OccupiedSeat

[<EntryPoint>]
let main argv =
    let seatLayout = "./input.txt" |> File.ReadAllLines

    let numberOfOccupiedSeats = getNumberOfOccupiedSeats seatLayout

    printfn "%d seats end up occupied" numberOfOccupiedSeats

    0
