let rec getNumbers spokenNumbers previousTurn previousNumber =
    let secondToLastTurnSpoken =
        spokenNumbers |> Map.tryFind previousNumber

    let number =
        secondToLastTurnSpoken
        |> Option.map (fun n -> previousTurn - n)
        |> Option.defaultValue 0

    let newSpokenNumbers =
        spokenNumbers
        |> Map.add previousNumber previousTurn

    seq {
        yield number
        yield! getNumbers newSpokenNumbers (previousTurn + 1) number
    }

let getNthSpokenNumber (n: int) (startingNumbers: int list) =
    let initialNumbers =
        Map(
            startingNumbers
            |> Seq.take (startingNumbers.Length - 1)
            |> Seq.mapi (fun i x -> (x, i + 1))
        )

    getNumbers initialNumbers startingNumbers.Length (List.last startingNumbers)
    |> Seq.append startingNumbers
    |> Seq.item (n - 1)

[<EntryPoint>]
let main argv =
    let startingNumbers = [ 1; 0; 15; 2; 10; 13 ]

    let number = getNthSpokenNumber 2020 startingNumbers
    printfn "The 2020th number spoken is %d" number

    0
