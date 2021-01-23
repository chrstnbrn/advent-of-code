let getDestinationIndex currentCup =
    Array.indexed
    >> Array.partition (fun (_, x) -> x < currentCup)
    >> (fun (smaller, bigger) -> [| smaller; bigger |])
    >> Array.collect (Array.sortByDescending snd)
    >> Array.map fst
    >> Array.head

let move (cups: int array) =
    let currentCup = cups.[0]
    let removedCups = cups.[1..3]
    let otherCups = cups.[4..]
    let destinationIndex = getDestinationIndex currentCup otherCups

    [ otherCups.[..destinationIndex]
      removedCups
      otherCups.[destinationIndex + 1..]
      [| currentCup |] ]
    |> Array.concat

let getCups labeling =
    labeling |> Seq.map (string >> int) |> Seq.toArray

let getCupsClockwiseFrom cup cups =
    let index = cups |> Array.findIndex ((=) cup)
    Array.append cups.[index + 1..] cups.[0..index - 1]

let simulateMoves (labeling: string) (moves: int): string =
    let cups = getCups labeling

    Seq.init moves ignore
    |> Seq.fold (fun state _ -> move state) cups
    |> getCupsClockwiseFrom 1
    |> Array.map string
    |> String.concat ""

[<EntryPoint>]
let main argv =
    let result = simulateMoves "871369452" 100
    printfn "The labels on the cups after cup 1 are %s" result
    0
