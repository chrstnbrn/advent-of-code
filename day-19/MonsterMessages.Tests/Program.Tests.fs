module MonsterMessages.Tests

open NUnit.Framework
open Program

[<Test>]
let getMessagesMatchingRule () =
    let rules =
        [| "0: 4 1 5"
           "1: 2 3 | 3 2"
           "2: 4 4 | 5 5"
           "3: 4 5 | 5 4"
           "4: \"a\""
           "5: \"b\"" |]

    let messages =
        [| "ababbb"
           "bababa"
           "abbbab"
           "aaabbb"
           "aaaabbb" |]

    let actual = getMessagesMatchingRule rules messages 0

    let expected = [| "ababbb"; "abbbab" |]
    Assert.AreEqual(expected, actual)
