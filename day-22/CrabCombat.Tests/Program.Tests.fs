module CrabCombat.Tests

open NUnit.Framework
open Program

[<Test>]
let getScore () =
    let player1Deck = [ 9; 2; 6; 3; 1 ]
    let player2Deck = [ 5; 8; 4; 7; 10 ]

    let actual = getScore player1Deck player2Deck

    Assert.AreEqual(306, actual)
