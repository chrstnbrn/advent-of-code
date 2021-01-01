open System
open System.IO

let countAnswersOfAnyone (answers: string array): int =
    answers
    |> Seq.map Set.ofSeq
    |> Set.unionMany
    |> Set.count

let countAnswersOfEveryone (answers: string array): int =
    answers
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.count

[<EntryPoint>]
let main argv =
    let inputPath = "./input.txt"
    let text = inputPath |> File.ReadAllText

    let groupAnswers =
        text.Split(Environment.NewLine + Environment.NewLine)
        |> Seq.map (fun x -> x.Split Environment.NewLine)

    let sumOfYesAnswersOfAnyone =
        groupAnswers |> Seq.sumBy countAnswersOfAnyone

    let sumOfYesAnswersOfEveryone =
        groupAnswers |> Seq.sumBy countAnswersOfEveryone

    printfn "There are %d questions that anyone within their groups answered with yes" sumOfYesAnswersOfAnyone
    printfn "There are %d questions that everyone within the groups answered with yes" sumOfYesAnswersOfEveryone

    0 // return an integer exit code
