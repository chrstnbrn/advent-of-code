open System
open System.IO

let countAnswers (answers: string array): int =
    answers |> String.Concat |> Set.ofSeq |> Set.count

[<EntryPoint>]
let main argv =
    let inputPath = "./input.txt"
    let text = inputPath |> File.ReadAllText

    let groupAnswers =
        text.Split(Environment.NewLine + Environment.NewLine)
        |> Seq.map (fun x -> x.Split Environment.NewLine)

    let sumOfYesAnswers = groupAnswers |> Seq.sumBy countAnswers

    printfn "There are %d questions answered with yes" sumOfYesAnswers

    0 // return an integer exit code
