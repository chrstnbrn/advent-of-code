module EncodingError.Tests

open NUnit.Framework
open Program

module getFirstInvalidNumber =
    [<Test>]
    let ``should return None if numbers are empty`` () =
        let numbers = [||]
        let actual = getFirstInvalidNumber 5 numbers
        Assert.AreEqual(None, actual)

    [<Test>]
    let ``should return None if numbers are less than preamble length`` () =
        let numbers = [| 1L; 2L; 3L; 4L |]
        let actual = getFirstInvalidNumber 5 numbers
        Assert.AreEqual(None, actual)

    [<Test>]
    let ``should return the first number that is invalid`` () =
        let numbers =
            [| 35L
               20L
               15L
               25L
               47L
               40L
               62L
               55L
               65L
               95L
               102L
               117L
               150L
               182L
               127L
               219L
               299L
               277L
               309L
               576L |]

        let actual = getFirstInvalidNumber 5 numbers

        Assert.AreEqual(Some 127L, actual)
