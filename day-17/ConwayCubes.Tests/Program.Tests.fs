module ConwayCubes.Tests

open NUnit.Framework
open Program

let getNumberOfActiveCubesTestCases =
    [ TestCaseData([| ".#."; "..#"; "###" |], 3, 6)
        .Returns(112)
      TestCaseData([| ".#."; "..#"; "###" |], 4, 6)
          .Returns(848) ]

[<TestCaseSource(nameof getNumberOfActiveCubesTestCases)>]
let getNumberOfActiveCubes initialState dimensions cycles =
    getNumberOfActiveCubes initialState dimensions cycles

[<Test>]
let getNeighborCoordinates () =
    let cubeCoordinate = [| 1; 2; 3 |]

    let actual = getNeighborCoordinates cubeCoordinate

    let expected =
        [ [| 0; 1; 2 |]
          [| 0; 1; 3 |]
          [| 0; 1; 4 |]
          [| 0; 2; 2 |]
          [| 0; 2; 3 |]
          [| 0; 2; 4 |]
          [| 0; 3; 2 |]
          [| 0; 3; 3 |]
          [| 0; 3; 4 |]
          [| 1; 1; 2 |]
          [| 1; 1; 3 |]
          [| 1; 1; 4 |]
          [| 1; 2; 2 |]
          [| 1; 2; 4 |]
          [| 1; 3; 2 |]
          [| 1; 3; 3 |]
          [| 1; 3; 4 |]
          [| 2; 1; 2 |]
          [| 2; 1; 3 |]
          [| 2; 1; 4 |]
          [| 2; 2; 2 |]
          [| 2; 2; 3 |]
          [| 2; 2; 4 |]
          [| 2; 3; 2 |]
          [| 2; 3; 3 |]
          [| 2; 3; 4 |] ]

    Assert.AreEqual(expected, actual)

[<Test>]
let simulateCycle () =
    let activeCubes =
        Set [ [| 1; 0; 0 |]
              [| 2; 1; 0 |]
              [| 0; 2; 0 |]
              [| 1; 2; 0 |]
              [| 2; 2; 0 |] ]

    let actual = simulateCycle activeCubes

    let expected =
        Set [ [| 0; 1; -1 |]
              [| 2; 2; -1 |]
              [| 1; 3; -1 |]
              [| 0; 1; 0 |]
              [| 2; 1; 0 |]
              [| 1; 2; 0 |]
              [| 2; 2; 0 |]
              [| 1; 3; 0 |]
              [| 0; 1; 1 |]
              [| 2; 2; 1 |]
              [| 1; 3; 1 |] ]

    Assert.AreEqual(expected, actual)
