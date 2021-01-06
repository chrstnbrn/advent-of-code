module ShuttleSearch.Tests

open NUnit.Framework
open Program


[<Test>]
let getResult () =
    let actual = getResult "939" "7,13,x,x,59,x,31,19"
    Assert.AreEqual(295, actual)

let getTimestampTestData =
    [ TestCaseData("10,11,x,13,14").Returns(10I)
      TestCaseData("7,13,x,x,59,x,31,19")
          .Returns(1068781I)
      TestCaseData("17,x,13,19").Returns(3417I)
      TestCaseData("67,7,59,61").Returns(754018I)
      TestCaseData("67,x,7,59,61").Returns(779210I)
      TestCaseData("67,7,x,59,61").Returns(1261476I)
      TestCaseData("1789,37,47,1889")
          .Returns(1202161486I) ]

[<TestCaseSource(nameof getTimestampTestData)>]
let getTimeStamp busIdsText = getTimestamp busIdsText
