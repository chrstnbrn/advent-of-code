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

[<Test>]
let decodeTicket () =
    let rules =
        [| "class: 0-1 or 4-19"
           "row: 0-5 or 8-19"
           "seat: 0-13 or 16-19" |]

    let tickets = [| "3,9,18"; "15,1,5"; "5,14,9" |]

    let ticket = "11,12,13"

    let actual = decodeTicket rules tickets ticket

    let expected =
        Map(
            [ ("class", 12)
              ("row", 11)
              "seat", 13 ]
        )

    Assert.AreEqual(expected, actual)
