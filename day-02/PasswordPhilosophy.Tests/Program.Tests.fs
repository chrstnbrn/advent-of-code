module PasswordPhilosophy.Tests

open NUnit.Framework
open Program

[<TestFixture>]
module ParseLine =
    [<Test>]
    let ```Should return None if the line is empty`` () =
        let actual = parseLine ""
        Assert.AreEqual(None, actual)

    [<Test>]
    let ```Should return None if the line has the wrong format`` () =
        let actual = parseLine "wrongformat"
        Assert.AreEqual(None, actual)

    [<Test>]
    let ```Should return None if first and second are letters`` () =
        let actual = parseLine "a-z a: mypassword "
        Assert.AreEqual(None, actual)

    [<Test>]
    let ```Should return PasswordLine if the line has a valid format`` () =
        let actual = parseLine "1-9 a: mypassword"

        let expected =
            Some
                { PasswordPolicy = { Letter = 'a'; First = 1; Second = 9 }
                  Password = "mypassword" }

        Assert.AreEqual(expected, actual)

[<TestCase('a', 2, 4, "abcdef", ExpectedResult = false)>]
[<TestCase('a', 2, 4, "aabcadaefa", ExpectedResult = false)>]
[<TestCase('a', 2, 4, "aabc", ExpectedResult = true)>]
[<TestCase('a', 2, 4, "aabac", ExpectedResult = true)>]
[<TestCase('a', 2, 4, "aabaca", ExpectedResult = true)>]
let ``isValidOld`` letter first second password =
    let policy =
        { Letter = letter
          First = first
          Second = second }

    isValidOld policy password

[<TestCase('a', 1, 3, "abcde", ExpectedResult = true)>]
[<TestCase('b', 1, 3, "cdefg", ExpectedResult = false)>]
[<TestCase('c', 2, 9, "ccccccccc", ExpectedResult = false)>]
let ``isValidNew`` letter first second password =
    let policy =
        { Letter = letter
          First = first
          Second = second }

    isValidNew policy password
