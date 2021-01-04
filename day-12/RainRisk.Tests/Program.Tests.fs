module RainRisk.Tests

open NUnit.Framework
open Program

module Part1 =
    [<Test>]
    let getManhattanDistance () =
        let navigationInstructions = [| "F10"; "N3"; "F7"; "R90"; "F11" |]

        let actual =
            Part1.getManhattanDistance navigationInstructions

        Assert.AreEqual(25, actual)

module Part2 =
    [<Test>]
    let getManhattanDistance2 () =
        let navigationInstructions = [| "F10"; "N3"; "F7"; "R90"; "F11" |]

        let actual =
            Part2.getManhattanDistance navigationInstructions

        Assert.AreEqual(286, actual)
