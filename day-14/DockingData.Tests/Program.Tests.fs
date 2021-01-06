module DockingData.Tests

open NUnit.Framework
open Program

[<Test>]
let getSum () =
    let program =
        [| "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
           "mem[8] = 11"
           "mem[7] = 101"
           "mem[8] = 0" |]

    let actual = getSum program

    Assert.AreEqual(165, actual)
