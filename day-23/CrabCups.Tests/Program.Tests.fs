module CrabCups.Tests

open NUnit.Framework
open Program

let simulateMovesTestData =
    [ TestCaseData("389125467", 10).Returns("92658374")
      TestCaseData("389125467", 100).Returns("67384529") ]

[<TestCaseSource(nameof simulateMovesTestData)>]
let simulateMoves labeling moves = simulateMoves labeling moves
