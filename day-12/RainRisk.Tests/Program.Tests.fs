module RainRisk.Tests

open NUnit.Framework
open Program

[<Test>]
let getManhattanDistance () =
    let navigationInstructions = [| "F10"; "N3"; "F7"; "R90"; "F11" |]

    let actual =
        getManhattanDistance navigationInstructions

    Assert.AreEqual(25, actual)
