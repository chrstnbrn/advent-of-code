module OperationOrder.Tests

open NUnit.Framework
open Program

let evaluateTestCases =
    [ TestCaseData("1 + 2 * 3 + 4 * 5 + 6").Returns(71)
      TestCaseData("1 + (2 * 3) + (4 * (5 + 6))")
          .Returns(51)
      TestCaseData("2 * 3 + (4 * 5)").Returns(26)
      TestCaseData("5 + (8 * 3 + 9 + 3 * 4 * 3)")
          .Returns(437)
      TestCaseData("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
          .Returns(12240)
      TestCaseData("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
          .Returns(13632) ]

[<TestCaseSource(nameof evaluateTestCases)>]
let evaluate expression = evaluate expression
