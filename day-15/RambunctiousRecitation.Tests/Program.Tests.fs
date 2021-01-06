module RambunctiousRecitation.Tests

open NUnit.Framework
open Program

let getNthSpokenNumberTestCases =
    [ TestCaseData([ 0; 3; 6 ], 1).Returns(0)
      TestCaseData([ 0; 3; 6 ], 2).Returns(3)
      TestCaseData([ 0; 3; 6 ], 3).Returns(6)
      TestCaseData([ 0; 3; 6 ], 4).Returns(0)
      TestCaseData([ 0; 3; 6 ], 5).Returns(3)
      TestCaseData([ 0; 3; 6 ], 6).Returns(3)
      TestCaseData([ 0; 3; 6 ], 7).Returns(1)
      TestCaseData([ 0; 3; 6 ], 8).Returns(0)
      TestCaseData([ 0; 3; 6 ], 9).Returns(4)
      TestCaseData([ 0; 3; 6 ], 10).Returns(0)
      TestCaseData([ 1; 2; 2 ], 4).Returns(1)
      TestCaseData([ 1; 3; 2 ], 2020).Returns(1)
      TestCaseData([ 2; 1; 3 ], 2020).Returns(10)
      TestCaseData([ 1; 2; 3 ], 2020).Returns(27)
      TestCaseData([ 2; 3; 1 ], 2020).Returns(78)
      TestCaseData([ 3; 2; 1 ], 2020).Returns(438)
      TestCaseData([ 3; 1; 2 ], 2020).Returns(1836)
      TestCaseData([ 0; 3; 6 ], 30000000)
          .Returns(175594)
      TestCaseData([ 1; 3; 2 ], 30000000).Returns(2578)
      TestCaseData([ 2; 1; 3 ], 30000000)
          .Returns(3544142)
      TestCaseData([ 1; 2; 3 ], 30000000)
          .Returns(261214)
      TestCaseData([ 2; 3; 1 ], 30000000)
          .Returns(6895259)
      TestCaseData([ 3; 2; 1 ], 30000000).Returns(18)
      TestCaseData([ 3; 1; 2 ], 30000000).Returns(362) ]


[<TestCaseSource(nameof getNthSpokenNumberTestCases)>]
let getNthSpokenNumber startingNumbers n = getNthSpokenNumber n startingNumbers
