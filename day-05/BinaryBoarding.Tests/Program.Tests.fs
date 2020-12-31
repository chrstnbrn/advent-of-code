module BinaryBoarding.Tests

open NUnit.Framework
open Program

[<TestCase("FBFBBFFRLR", 44, 5, 357)>]
[<TestCase("BFFFBBFRRR", 70, 7, 567)>]
[<TestCase("FFFBBBFRRR", 14, 7, 119)>]
[<TestCase("BBFFBBFRLL", 102, 4, 820)>]
let decodeSeat code row column seatId =
    let actual = decodeSeat code

    let expected =
        { Row = row
          Column = column
          SeatId = seatId }

    Assert.AreEqual(expected, actual)
