module AdapterArray.Tests

open NUnit.Framework
open Program

let getJoltageDifferenceTestCases =
    [ TestCaseData([| 16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4 |])
        .Returns(Map([ (1, 7); (3, 5) ]))
      TestCaseData([| 28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3 |])
          .Returns(Map([ (1, 22); (3, 10) ])) ]

[<TestCaseSource(nameof getJoltageDifferenceTestCases)>]
let getJoltageDifferences adapters = getJoltageDifferences adapters

let getNumberOfPossibleArrangementsTestCases =
    [ TestCaseData([| 16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4 |])
        .Returns(8L)

      TestCaseData([| 28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3 |])
          .Returns(19208L) ]

[<TestCaseSource(nameof getNumberOfPossibleArrangementsTestCases)>]
let getNumberOfPossibleArrangements adapters =
    getNumberOfPossibleArrangements adapters
