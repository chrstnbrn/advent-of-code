module DockingData.Tests

open NUnit.Framework
open Program

module Part1 =

    [<Test>]
    let getSum () =
        let program =
            [| "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
               "mem[8] = 11"
               "mem[7] = 101"
               "mem[8] = 0" |]

        let actual = Part1.getSum program

        Assert.AreEqual(165, actual)

module Part2 =

    [<Test>]
    let getSum () =
        let program =
            [| "mask = 000000000000000000000000000000X1001X"
               "mem[42] = 100"
               "mask = 00000000000000000000000000000000X0XX"
               "mem[26] = 1" |]

        let actual = Part2.getSum program

        Assert.AreEqual(208, actual)
