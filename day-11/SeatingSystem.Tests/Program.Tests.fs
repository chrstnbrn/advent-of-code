module SeatingSystem.Tests

open NUnit.Framework
open Program

let seatLayout =
    [| "L.LL.LL.LL"
       "LLLLLLL.LL"
       "L.L.L..L.."
       "LLLL.LL.LL"
       "L.LL.LL.LL"
       "L.LLLLL.LL"
       "..L.L....."
       "LLLLLLLLLL"
       "L.LLLLLL.L"
       "L.LLLLL.LL" |]

[<Test>]
let getNumberOfOccupiedSeats1 () =
    let actual = getNumberOfOccupiedSeats1 seatLayout
    Assert.AreEqual(37, actual)

[<Test>]
let getNumberOfOccupiedSeats2 () =
    let actual = getNumberOfOccupiedSeats2 seatLayout
    Assert.AreEqual(26, actual)
