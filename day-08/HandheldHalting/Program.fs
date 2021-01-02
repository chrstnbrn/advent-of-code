open System
open System.IO

type Operation =
    | Nop
    | Acc
    | Jmp

type Instruction =
    { Index: int
      Operation: Operation
      Argument: int }

type Result =
    { Terminated: bool
      Accumulator: int
      Instructions: Instruction list }

let getOperation (s: string): Operation =
    match s with
    | "nop" -> Nop
    | "acc" -> Acc
    | "jmp" -> Jmp
    | _ -> raise (ArgumentException("Invalid operation: " + s))

let getInstruction (index: int) (instructionText: string): Instruction =
    let parts = instructionText.Split(' ')
    let operation = parts.[0] |> getOperation
    let argument = parts.[1] |> int

    { Index = index
      Operation = operation
      Argument = argument }

let getInstructions (instructionTexts: string seq): Instruction list =
    instructionTexts
    |> Seq.mapi getInstruction
    |> Seq.toList

let argumentOrDefault (instruction: Instruction) (operation: Operation) (defaultValue: int): int =
    if instruction.Operation = operation then
        instruction.Argument
    else
        defaultValue

let getNextInstruction (instructions: Instruction list) (currentInstruction: Instruction): Instruction option =
    let increment =
        argumentOrDefault currentInstruction Jmp 1

    instructions
    |> Seq.tryItem (currentInstruction.Index + increment)

let run (instructions: Instruction list): Result =
    let mutable currentInstruction = instructions |> List.tryHead
    let mutable accumulator = 0
    let mutable executedInstructions = []
    let mutable loopDetected = false

    while currentInstruction.IsSome && not loopDetected do
        let instruction = currentInstruction.Value

        accumulator <- accumulator + argumentOrDefault instruction Acc 0

        executedInstructions <- executedInstructions @ [ instruction ]
        currentInstruction <- getNextInstruction instructions instruction

        loopDetected <-
            currentInstruction.IsSome
            && Seq.contains currentInstruction.Value executedInstructions

    { Terminated = not loopDetected
      Accumulator = accumulator
      Instructions = executedInstructions }

let getAccumulator r = r.Accumulator

let getValueBeforeRepeatingInstruction (instructionTexts: string seq): int =
    instructionTexts
    |> getInstructions
    |> run
    |> getAccumulator

let changeInstruction (instruction: Instruction): Instruction =
    match instruction.Operation with
    | Jmp -> { instruction with Operation = Nop }
    | Nop -> { instruction with Operation = Jmp }
    | _ -> instruction

let replaceAtIndex index element =
    List.mapi (fun i x -> if i = index then element else x)

let changeInstructionAtIndex (instructions: Instruction list) (index: int): Instruction list =
    let changedInstruction = changeInstruction instructions.[index]

    instructions
    |> replaceAtIndex index changedInstruction

let getChangedInstructions (instructions: Instruction list): Instruction list seq =
    instructions
    |> Seq.filter (fun x -> (x.Operation = Jmp) || (x.Operation = Nop))
    |> Seq.map (fun x -> changeInstructionAtIndex instructions x.Index)

let getValueAfterTermination (instructionTexts: string seq): int =
    instructionTexts
    |> getInstructions
    |> getChangedInstructions
    |> Seq.map run
    |> Seq.find (fun x -> x.Terminated)
    |> getAccumulator

[<EntryPoint>]
let main argv =
    let instructions = "./input.txt" |> File.ReadAllLines

    let valueBeforeRepeatingInstruction =
        getValueBeforeRepeatingInstruction instructions

    let valueAfterTermination = getValueAfterTermination instructions

    printfn "The value before any instruction is run a second time is %d" valueBeforeRepeatingInstruction
    printfn "The value after the program terminates is %d" valueAfterTermination

    0 // return an integer exit code
