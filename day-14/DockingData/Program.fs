open System
open System.Collections
open System.IO
open System.Text.RegularExpressions

type BitmaskBit =
    | Zero
    | One
    | X

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

module UInt64 =
    let getBits (value: uint64) =
        value
        |> BitConverter.GetBytes
        |> BitArray
        |> Seq.cast<bool>

    let fromBitArray (bitArray: BitArray): uint64 =
        let mutable bytes = Array.zeroCreate 8
        bitArray.CopyTo(bytes, 0)
        BitConverter.ToUInt64(bytes, 0)

let parseBitmaskBit =
    function
    | '0' -> Zero
    | '1' -> One
    | 'X' -> X
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

let getNextState decoder state instruction =
    match instruction with
    | UpdateBitmask bitmask -> { state with Bitmask = bitmask }
    | WriteValue w -> { state with Memory = decoder state w }

let run decoder instructions =
    let initialState = { Memory = Map.empty; Bitmask = [||] }

    instructions
    |> Seq.fold (getNextState decoder) initialState

let sumOfAllValues = Map.toSeq >> Seq.sumBy snd

module Part1 =
    let getBit bit =
        function
        | Zero -> false
        | One -> true
        | X -> bit

    let getValue (value: uint64) (bitmask: Bitmask): uint64 =
        let bits = UInt64.getBits value

        bitmask
        |> Seq.rev
        |> Seq.zip bits
        |> Seq.map (fun (bit, bitmaskBit) -> getBit bit bitmaskBit)
        |> Seq.toArray
        |> BitArray
        |> UInt64.fromBitArray

    let decoder state writeValue =
        let newValue = getValue writeValue.Value state.Bitmask
        Map.add writeValue.MemoryAddress newValue state.Memory

    let getSum (program: string array): uint64 =
        let finalState =
            program
            |> Array.map parseInstruction
            |> run decoder

        finalState.Memory |> sumOfAllValues

module Part2 =
    type AddressBit =
        | Zero
        | One
        | Floating

    let getAddressBit bit =
        function
        | BitmaskBit.Zero -> if bit then One else Zero
        | BitmaskBit.One -> One
        | BitmaskBit.X -> Floating

    let replaceAtIndex index value =
        Array.mapi (fun i x -> if i = index then value else x)

    let rec getAddressesForTemplate template =
        seq {
            if Array.contains Floating template then
                let index =
                    template |> Array.findIndex ((=) Floating)

                for value in [ One; Zero ] do
                    let newTemplate = template |> replaceAtIndex index value
                    yield! getAddressesForTemplate newTemplate
            else
                yield
                    template
                    |> Array.map ((=) One)
                    |> BitArray
                    |> UInt64.fromBitArray
        }

    let getAddresses (writeValue: WriteValue) (bitmask: Bitmask): uint64 seq =
        let bits = UInt64.getBits writeValue.MemoryAddress

        bitmask
        |> Seq.rev
        |> Seq.zip bits
        |> Seq.map (fun (bit, bitmaskBit) -> getAddressBit bit bitmaskBit)
        |> Seq.toArray
        |> getAddressesForTemplate

    let decoder state writeValue =
        let addresses = getAddresses writeValue state.Bitmask
        let setValue map address = Map.add address writeValue.Value map
        addresses |> Seq.fold setValue state.Memory

    let getSum (program: string array): uint64 =
        let finalState =
            program
            |> Array.map parseInstruction
            |> run decoder

        finalState.Memory |> sumOfAllValues

[<EntryPoint>]
let main argv =
    let program = File.ReadAllLines "./input.txt"

    let sum1 = Part1.getSum program
    printfn "Part1: The sum of all values is %d" sum1

    let sum2 = Part2.getSum program
    printfn "Part2: The sum of all values is %d" sum2

    0
