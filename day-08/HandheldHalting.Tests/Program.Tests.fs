module HandheldHalting.Tests

open NUnit.Framework
open Program

let instructions =
    [ "nop +0"
      "acc +1"
      "jmp +4"
      "acc +3"
      "jmp -3"
      "acc -99"
      "acc +1"
      "jmp -4"
      "acc +6" ]

[<Test>]
let getValueBeforeRepeatingInstruction () =
    let actual =
        getValueBeforeRepeatingInstruction instructions

    Assert.AreEqual(5, actual)

[<Test>]
let getValueAfterTermination () =
    let actual = getValueAfterTermination instructions

    Assert.AreEqual(8, actual)
