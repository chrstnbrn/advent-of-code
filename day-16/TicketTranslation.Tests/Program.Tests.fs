module TicketTranslation.Tests

open NUnit.Framework
open Program

[<Test>]
let getTicketScanningErrorRate () =
    let rules =
        [| "class: 1-3 or 5-7"
           "row: 6-11 or 33-44"
           "seat: 13-40 or 45-50" |]

    let tickets =
        [| "7,3,47"
           "40,4,50"
           "55,2,20"
           "38,6,12" |]

    let actual = getTicketScanningErrorRate tickets rules

    Assert.AreEqual(71, actual)
