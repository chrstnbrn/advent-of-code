open System
open System.IO
open System.Text.RegularExpressions

type Seat = { Row: int; Column: int; SeatId: int }

let getRowAndColumnCode (code: string): string * string =
    let pattern = "^(?<Row>[FB]{7})(?<Column>[RL]{3})$"
    let matchResult = Regex.Match(code, pattern)

    let rowCode = matchResult.Groups.["Row"].Value
    let columnCode = matchResult.Groups.["Column"].Value

    rowCode, columnCode

let toBinaryString (zeroChar: char) (oneChar: char) (s: string): string =
    s.Replace(zeroChar, '0').Replace(oneChar, '1')

let binaryStringToInt s = Convert.ToInt32(s, 2)

let decode (code: string) (zeroChar: char) (oneChar: char): int =
    code
    |> toBinaryString zeroChar oneChar
    |> binaryStringToInt

let decodeRow rowCode = decode rowCode 'F' 'B'
let decodeColumn columnCode = decode columnCode 'L' 'R'

let getSeatId row column = row * 8 + column

let decodeSeat (code: string): Seat =
    let rowCode, columnCode = getRowAndColumnCode code

    let row = decodeRow rowCode
    let column = decodeColumn columnCode
    let seatId = getSeatId row column

    { Row = row
      Column = column
      SeatId = seatId }

let findEmptySeatId (seats: Seat seq): int =
    let seatIds = seats |> Seq.map (fun x -> x.SeatId)
    let minSeatId = seatIds |> Seq.min
    let maxSeatId = seatIds |> Seq.max
    let seatIdSet = seatIds |> Set.ofSeq

    let isSeatTaken id = seatIdSet |> Set.contains id
    let isSeatFree id = not <| isSeatTaken id

    [ minSeatId .. maxSeatId ]
    |> Seq.find
        (fun id ->
            isSeatFree id
            && isSeatTaken (id - 1)
            && isSeatTaken (id + 1))

[<EntryPoint>]
let main argv =
    let inputPath = "./input.txt"
    let codes = File.ReadAllLines inputPath

    let seats = codes |> Seq.map decodeSeat

    let highestSeatId =
        seats |> Seq.map (fun x -> x.SeatId) |> Seq.max

    let emptySeatId = seats |> findEmptySeatId

    printfn "The highest SeatId is %d" highestSeatId
    printfn "The SeatId of the empty seat id %d" emptySeatId

    0
