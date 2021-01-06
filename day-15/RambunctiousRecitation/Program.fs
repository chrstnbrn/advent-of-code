open System.Collections.Generic

let rec getNumbers (spokenNumbers: Dictionary<int, int>) previousTurn previousNumber =
    let secondToLastTurnSpoken =
        if spokenNumbers.ContainsKey(previousNumber) then
            Some spokenNumbers.[previousNumber]
        else
            None

    let number =
        secondToLastTurnSpoken
        |> Option.map (fun n -> previousTurn - n)
        |> Option.defaultValue 0

    spokenNumbers.[previousNumber] <- previousTurn

    seq {
        yield number
        yield! getNumbers spokenNumbers (previousTurn + 1) number
    }

let getNthSpokenNumber (n: int) (startingNumbers: int list) =
    let initialNumbers = Dictionary<int, int>()

    startingNumbers
    |> Seq.take (startingNumbers.Length - 1)
    |> Seq.iteri (fun i x -> (initialNumbers.[x] <- (i + 1)))

    getNumbers initialNumbers startingNumbers.Length (List.last startingNumbers)
    |> Seq.append startingNumbers
    |> Seq.item (n - 1)

[<EntryPoint>]
let main argv =
    let startingNumbers = [ 1; 0; 15; 2; 10; 13 ]

    let number2020 = getNthSpokenNumber 2020 startingNumbers
    printfn "The 2020th number spoken is %d" number2020

    let number30000000 =
        getNthSpokenNumber 30000000 startingNumbers

    printfn "The 30000000th number spoken is %d" number30000000

    0
