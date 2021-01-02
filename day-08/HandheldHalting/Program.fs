open System
open System.IO

type Operation =
    | Nop
    | Acc
    | Jmp

type Instruction = { Operation: Operation; Argument: int }

let getOperation (s: string): Operation =
    match s with
    | "nop" -> Nop
    | "acc" -> Acc
    | "jmp" -> Jmp
    | _ -> raise (ArgumentException("Invalid operation: " + s))

let getInstruction (instructionText: string): Instruction =
    let parts = instructionText.Split(' ')
    let operation = parts.[0] |> getOperation
    let argument = parts.[1] |> int

    { Operation = operation
      Argument = argument }

let getNextIndex (instructions: Instruction list) (currentIndex: int): int =
    let currentInstruction = instructions.[currentIndex]

    match currentInstruction.Operation with
    | Jmp -> currentIndex + currentInstruction.Argument
    | _ -> currentIndex + 1

let rec getInstructionsBeforeLoop (instructions: Instruction list) (visitedIndexes: int list): Instruction seq =
    let currentIndex = visitedIndexes |> Seq.last
    let nextIndex = getNextIndex instructions currentIndex

    seq {
        yield instructions.[currentIndex]

        if (not <| Seq.contains nextIndex visitedIndexes) then
            yield! getInstructionsBeforeLoop instructions (visitedIndexes @ [ nextIndex ])
    }

let getValueBeforeRepeatingInstruction (instructionTexts: string seq): int =
    let instructions =
        instructionTexts
        |> Seq.map getInstruction
        |> Seq.toList

    let instructionsBeforeLoop =
        getInstructionsBeforeLoop instructions [ 0 ]

    instructionsBeforeLoop
    |> Seq.filter (fun x -> x.Operation = Acc)
    |> Seq.sumBy (fun x -> x.Argument)

[<EntryPoint>]
let main argv =
    let instructions = "./input.txt" |> File.ReadAllLines

    let value =
        getValueBeforeRepeatingInstruction instructions

    printfn "The value before any instruction is run a second time is %d" value

    0 // return an integer exit code
