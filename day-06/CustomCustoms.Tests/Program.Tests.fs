module CustomCustoms.Tests

open NUnit.Framework
open Program

let countAnswersOfAnyoneTestCases =
    [ TestCaseData([| "abc" |]).Returns(3)
      TestCaseData([| "a"; "b"; "c" |]).Returns(3)
      TestCaseData([| "ab"; "ac" |]).Returns(3)
      TestCaseData([| "a"; "a"; "a"; "a" |]).Returns(1)
      TestCaseData([| "b" |]).Returns(1) ]

[<TestCaseSource(nameof countAnswersOfAnyoneTestCases)>]
let ``countAnswersOfAnyone should return the number of answers where anyone answered with yes`` answers =
    countAnswersOfAnyone answers

let countAnswersOfEveryoneTestCases =
    [ TestCaseData([| "abc" |]).Returns(3)
      TestCaseData([| "a"; "b"; "c" |]).Returns(0)
      TestCaseData([| "ab"; "ac" |]).Returns(1)
      TestCaseData([| "a"; "a"; "a"; "a" |]).Returns(1)
      TestCaseData([| "b" |]).Returns(1) ]

[<TestCaseSource(nameof countAnswersOfEveryoneTestCases)>]
let ``countAnswersOfEveryone should return the number of answers where everyone answered with yes`` answers =
    countAnswersOfEveryone answers
