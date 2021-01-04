module ShuttleSearch.Tests

open NUnit.Framework
open Program


[<Test>]
let getResult () =
    let actual = getResult "939" "7,13,x,x,59,x,31,19"
    Assert.AreEqual(295, actual)
