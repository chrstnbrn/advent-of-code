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

[<EntryPoint>]
let main argv =
    let input = "./input.txt" |> File.ReadAllLines

    let startTime = input.[0]
    let busIds = input.[1]

    let result = getResult startTime busIds

    printfn "The result is %d" result

    0
