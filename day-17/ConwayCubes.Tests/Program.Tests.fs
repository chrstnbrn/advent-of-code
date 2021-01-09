module ConwayCubes.Tests

open NUnit.Framework
open Program

[<Test>]
let getNumberOfActiveCubes () =
    let cycles = 6
    let initialState = [| ".#."; "..#"; "###" |]

    let actual =
        getNumberOfActiveCubes cycles initialState

    Assert.AreEqual(112, actual)

[<Test>]
let getNeighborCoordinates () =
    let cubeCoordinate = (1, 2, 3)

    let actual = getNeighborCoordinates cubeCoordinate

    let expected =
        [ (0, 1, 2)
          (0, 1, 3)
          (0, 1, 4)
          (0, 2, 2)
          (0, 2, 3)
          (0, 2, 4)
          (0, 3, 2)
          (0, 3, 3)
          (0, 3, 4)
          (1, 1, 2)
          (1, 1, 3)
          (1, 1, 4)
          (1, 2, 2)
          (1, 2, 4)
          (1, 3, 2)
          (1, 3, 3)
          (1, 3, 4)
          (2, 1, 2)
          (2, 1, 3)
          (2, 1, 4)
          (2, 2, 2)
          (2, 2, 3)
          (2, 2, 4)
          (2, 3, 2)
          (2, 3, 3)
          (2, 3, 4) ]

    Assert.AreEqual(expected, actual)

[<Test>]
let simulateCycle () =
    let activeCubes =
        Set [ (1, 0, 0)
              (2, 1, 0)
              (0, 2, 0)
              (1, 2, 0)
              (2, 2, 0) ]

    let actual = simulateCycle activeCubes

    let expected =
        Set [ (0, 1, -1)
              (2, 2, -1)
              (1, 3, -1)
              (0, 1, 0)
              (2, 1, 0)
              (1, 2, 0)
              (2, 2, 0)
              (1, 3, 0)
              (0, 1, 1)
              (2, 2, 1)
              (1, 3, 1) ]

    Assert.AreEqual(expected, actual)
