open System
open System.Collections
open System.IO
open System.Text.RegularExpressions

type BitmaskBit =
    | OverwriteWithZero
    | OverwriteWithOne
    | Unchanged

type Bitmask = BitmaskBit array

type WriteValue =
    { MemoryAddress: uint64
      Value: uint64 }

type Instruction =
    | UpdateBitmask of Bitmask
    | WriteValue of WriteValue

type State =
    { Memory: Map<uint64, uint64>
      Bitmask: Bitmask }

let parseBitmaskBit =
    function
    | '0' -> OverwriteWithZero
    | '1' -> OverwriteWithOne
    | 'X' -> Unchanged
    | ch -> failwithf "Invalid bitmask char: %c" ch

let parseUpdateBitmask text =
    let pattern = "mask = (?<Bitmask>[X01]{36})"
    let result = Regex.Match(text, pattern)

    if result.Success then
        let bitMaskString = result.Groups.["Bitmask"].Value

        let bitmask: Bitmask =
            bitMaskString
            |> Seq.toArray
            |> Array.map parseBitmaskBit

        Some(UpdateBitmask bitmask)
    else
        None

let parseWriteValue text =
    let pattern =
        "mem\[(?<MemoryAddress>\d+)\] = (?<Value>\d+)"

    let result = Regex.Match(text, pattern)

    if result.Success then
        let memoryAddress =
            result.Groups.["MemoryAddress"].Value |> uint64

        let value = result.Groups.["Value"].Value |> uint64

        Some(
            WriteValue
                { MemoryAddress = memoryAddress
                  Value = value }
        )
    else
        None

let parseInstruction text: Instruction =
    match parseUpdateBitmask text, parseWriteValue text with
    | Some i, None
    | None, Some i -> i
    | _ -> failwithf "Invalid instruction %s" text

let bitArrayToSeq (bitArray: BitArray) =
    seq {
        for i = 0 to bitArray.Length - 1 do
            yield bitArray.Get(i)
    }

let bitArrayToUInt64 (bitArray: BitArray): uint64 =
    let mutable bytes = Array.zeroCreate 8
    bitArray.CopyTo(bytes, 0)
    BitConverter.ToUInt64(bytes, 0)

let getBit bit =
    function
    | OverwriteWithZero -> false
    | OverwriteWithOne -> true
    | Unchanged -> bit

let getValue (value: uint64) (bitmask: Bitmask): uint64 =
    let bitArray =
        value
        |> BitConverter.GetBytes
        |> BitArray
        |> bitArrayToSeq

    bitmask
    |> Seq.rev
    |> Seq.zip bitArray
    |> Seq.map (fun (bit, bitmaskBit) -> getBit bit bitmaskBit)
    |> Seq.toArray
    |> BitArray
    |> bitArrayToUInt64

let getMemory state writeValue =
    let newValue = getValue writeValue.Value state.Bitmask
    Map.add writeValue.MemoryAddress newValue state.Memory

let getNextState state instruction =
    match instruction with
    | UpdateBitmask bitmask -> { state with Bitmask = bitmask }
    | WriteValue w ->
        { state with
              Memory = getMemory state w }

let run instructions =
    let initialState = { Memory = Map.empty; Bitmask = [||] }
    instructions |> Seq.fold getNextState initialState

let getSum (program: string array) =
    let finalState =
        program |> Array.map parseInstruction |> run

    finalState.Memory |> Map.toSeq |> Seq.sumBy snd

[<EntryPoint>]
let main argv =
    let program = File.ReadAllLines "./input.txt"
    let sum = getSum program

    printfn "The sum of all values is %d" sum

    0
