open System
open System.IO

let getSchedule busId = Seq.initInfinite (fun i -> i * busId)

let getNextDeparture time =
    getSchedule >> Seq.find (fun x -> x >= time)

let getAvailableBusIds (busIdsText: string) =
    busIdsText.Split ','
    |> Array.filter ((<>) "x")
    |> Array.map int

let getResult (arrivalTimeText: string) (busIdsText: string): int =
    let arrivalTime = int arrivalTimeText

    let (busId, departure) =
        busIdsText
        |> getAvailableBusIds
        |> Array.map (fun id -> (id, getNextDeparture arrivalTime id))
        |> Array.minBy snd

    let waitTime = departure - arrivalTime

    busId * waitTime

let rec greatestCommonDivisor a b =
    if b = 0I then
        abs a
    else
        greatestCommonDivisor b (a % b)

let leastCommonMultiple a b = a * b / (greatestCommonDivisor a b)

type Constraint = { BusId: bigint; Offset: bigint }

let getConstraints (busIdsText: string) =
    busIdsText.Split ','
    |> Array.indexed
    |> Array.filter (fun (_, x) -> x <> "x")
    |> Array.map
        (fun (i, busId) ->
            { BusId = bigint (int busId)
              Offset = bigint (i) })

let departsAt time busId = time % busId = 0I

let getNext (timestamp, period) busConstraint =
    let nextTimestamp =
        Seq.initInfinite (fun i -> timestamp + bigint (i) * period)
        |> Seq.find (fun t -> departsAt (t + busConstraint.Offset) busConstraint.BusId)

    let nextPeriod =
        leastCommonMultiple period busConstraint.BusId

    (nextTimestamp, nextPeriod)

let getTimestamp (busIdsText: string): bigint =
    let constraints = getConstraints busIdsText
    let firstBusId = constraints.[0].BusId
    let initialTimestamp, initialPeriod = firstBusId, firstBusId

    constraints
    |> Seq.fold getNext (initialTimestamp, initialPeriod)
    |> fst

[<EntryPoint>]
let main argv =
    let input = "./input.txt" |> File.ReadAllLines

    let startTime = input.[0]
    let busIds = input.[1]

    let result = getResult startTime busIds
    printfn "Part 1: The result is %d" result

    let timestamp = getTimestamp busIds
    printfn "Part 2: The timestamp is %A" timestamp

    0
