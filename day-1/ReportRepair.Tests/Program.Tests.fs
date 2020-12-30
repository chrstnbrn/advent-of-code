module ReportRepair.Tests

open NUnit.Framework
open Program

[<Test>]
let ``No entries are returned if sequence is empty``() =
    let actual = findEntriesWithSum 100 2 []
    Assert.AreEqual(None, actual)
    
[<Test>]
let ``No entries are returned if count is 0``() =
    let actual = findEntriesWithSum 10 0 [10; 20]
    Assert.AreEqual(None, actual)
    
[<Test>]
let ``No entries are returned if count is -1``() =
    let actual = findEntriesWithSum 10 -1 [10; 20]
    Assert.AreEqual(None, actual)

[<Test>]
let ``No entries are returned if sequence contains one element that is not equal to the sum``() =
    let actual = findEntriesWithSum 100 2 [90]
    Assert.AreEqual(None, actual)

[<Test>]
let ``No entries are returned if sequence contains one element that is equal to the sum``() =
    let actual = findEntriesWithSum 100 2 [100]
    Assert.AreEqual(None, actual)

[<Test>]
let ``No entries are returned if sequence contains two elements and one is half of the sum``() =
    let actual = findEntriesWithSum 100 2 [50; 60]
    Assert.AreEqual(None, actual)

[<Test>]
let ``Returns the two entries that sum up to the sum``() =
    let actual = findEntriesWithSum 100 2 [10; 20; 30; 40; 50; 60]
    Assert.AreEqual(Some [40; 60], actual)

[<Test>]
let ``Returns the three entries that sum up to the sum`` () =
    let actual = findEntriesWithSum 100 3 [10; 20; 30; 40; 50; 60]
    Assert.AreEqual(Some [10; 30; 60], actual)
