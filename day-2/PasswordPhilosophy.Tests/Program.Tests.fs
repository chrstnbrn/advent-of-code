module PasswordPhilosophy.Tests

open NUnit.Framework
open Program

[<TestFixture>]
module ParseLine =
  [<Test>]
  let ```Should return None if the line is empty``() =
      let actual = parseLine ""
      Assert.AreEqual(None, actual)

  [<Test>]
  let ```Should return None if the line has the wrong format``() =
      let actual = parseLine "wrongformat"
      Assert.AreEqual(None, actual)

  [<Test>]
  let ```Should return None if min and max are letters``() =
      let actual = parseLine "a-z a: mypassword "
      Assert.AreEqual(None, actual)

  [<Test>]
  let ```Should return PasswordLine if the line has a valid format``() =
      let actual = parseLine "1-9 a: mypassword"
      let expected = Some { PasswordPolicy = {Letter = 'a'; Min = 1; Max = 9;}; Password = "mypassword"}
      Assert.AreEqual(expected, actual)

[<TestCase('a', 2, 4, "abcdef", ExpectedResult = false)>]
[<TestCase('a', 2, 4, "aabcadaefa", ExpectedResult = false)>]
[<TestCase('a', 2, 4, "aabc", ExpectedResult = true)>]
[<TestCase('a', 2, 4, "aabac", ExpectedResult = true)>]
[<TestCase('a', 2, 4, "aabaca", ExpectedResult = true)>]
let ``isValidLine`` letter min max password =
    let policy = { Letter = letter; Min = min; Max = max; }
    isValid policy password
