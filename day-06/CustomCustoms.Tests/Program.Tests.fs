module CustomCustoms.Tests

open NUnit.Framework
open Program

let countAnswersTestCases =
    [ TestCaseData([| "abc" |]).Returns(3)
      TestCaseData([| "a"; "b"; "c" |]).Returns(3)
      TestCaseData([| "ab"; "ac" |]).Returns(3)
      TestCaseData([| "a"; "a"; "a"; "a" |]).Returns(1)
      TestCaseData([| "b" |]).Returns(1) ]

[<TestCaseSource(nameof countAnswersTestCases)>]
let countAnswers answers = countAnswers answers
