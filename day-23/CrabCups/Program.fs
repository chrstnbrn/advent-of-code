open System.Collections.Generic

let getNextCup (node: LinkedListNode<int>) =
    if isNull node.Next then
        node.List.First
    else
        node.Next

let rec findDestinationCup (map: Map<int, LinkedListNode<int>>) min max value =
    if value < min then
        findDestinationCup map min max max
    else
        match Map.tryFind value map with
        | Some c when not (isNull c.List) -> c
        | _ -> findDestinationCup map min max (value - 1)

let pickUp number cup =
    seq {
        for _ in [ 1 .. number ] do
            let cupToRemove = getNextCup cup
            yield cupToRemove
            cup.List.Remove(cupToRemove)
    }
    |> Seq.toList

let addAfter (cups: LinkedListNode<int> list) (currentCup: LinkedListNode<int>) =
    for cup in List.rev cups do
        currentCup.List.AddAfter(currentCup, cup)

let move currentCup map min max =
    let pickedUpCups = pickUp 3 currentCup

    findDestinationCup map min max (currentCup.Value - 1)
    |> addAfter pickedUpCups

    getNextCup currentCup

let getCups labeling =
    let linkedList = new LinkedList<int>()
    let cups = labeling |> Seq.map (string >> int)
    cups |> Seq.iter (linkedList.AddLast >> ignore)
    linkedList

let toMap (cups: LinkedList<int>) =
    seq {
        let mutable current = cups.First

        while not (isNull current) do
            yield (current.Value, current)
            current <- current.Next
    }
    |> Map.ofSeq

let getCupsClockwiseFrom cup map =
    let mutable current = map |> Map.find cup |> getNextCup

    seq {
        while current.Value <> cup do
            yield current.Value
            current <- getNextCup current
    }

let simulateMoves (labeling: string) (moves: int): string =
    let cups = getCups labeling
    let minCup = cups |> Seq.min
    let maxCup = cups |> Seq.max
    let map = toMap cups

    Seq.init moves ignore
    |> Seq.fold (fun currentcup _ -> move currentcup map minCup maxCup) cups.First
    |> ignore

    map
    |> getCupsClockwiseFrom 1
    |> Seq.map string
    |> String.concat ""

let simulateMoves2 (labeling: string) (moves: int): int64 =
    let cups = getCups labeling
    let minCup = cups |> Seq.min
    let maxCup = 1_000_000

    for i in [ Seq.max cups + 1 .. maxCup ] do
        cups.AddLast(i) |> ignore

    let map = toMap cups

    Seq.init moves ignore
    |> Seq.fold (fun currentCup _ -> move currentCup map minCup maxCup) cups.First
    |> ignore

    map
    |> getCupsClockwiseFrom 1
    |> Seq.take 2
    |> Seq.map int64
    |> Seq.reduce (*)

[<EntryPoint>]
let main argv =
    let input = "871369452"

    let result = simulateMoves input 100
    printfn "The labels on the cups after cup 1 are %s" result

    let result2 = simulateMoves2 input 10_000_000
    printfn "The product of the two cups immediately after cup 1 is %d" result2

    0
