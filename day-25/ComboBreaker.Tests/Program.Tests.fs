module ComboBreaker.Tests

open NUnit.Framework
open Program

[<Test>]
let getEncryptionKey () =
    let cardPublicKey = 5764801L
    let doorPublicKey = 17807724L

    let actual =
        getEncryptionKey cardPublicKey doorPublicKey

    Assert.AreEqual(14897079L, actual)
