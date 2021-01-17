module Array2D.Tests

open NUnit.Framework

let getRowTestCases =
    [ TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   0)
        .Returns([| 1; 2; 3 |])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   1)
          .Returns([| 4; 5; 6 |])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   2)
          .Returns([| 7; 8; 9 |])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   3)
          .Returns([| 10; 11; 12 |]) ]

[<TestCaseSource(nameof getRowTestCases)>]
let getRow (a: int [,]) (row: int) = getRow row a

let getColumnTestCases =
    [ TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   0)
        .Returns([| 1; 4; 7; 10 |])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   1)
          .Returns([| 2; 5; 8; 11 |])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ],
                   2)
          .Returns([| 3; 6; 9; 12 |]) ]

[<TestCaseSource(nameof getColumnTestCases)>]
let getColumn (a: int [,]) (column: int) = getColumn column a

let getRowsTestCases =
    [ TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ])
        .Returns([| [| 1; 2; 3 |]
                    [| 4; 5; 6 |]
                    [| 7; 8; 9 |]
                    [| 10; 11; 12 |] |]) ]

[<TestCaseSource(nameof getRowsTestCases)>]
let getRows (a: int [,]) = getRows a

let getColumnsTestCases =
    [ TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ])
        .Returns([| [| 1; 4; 7; 10 |]
                    [| 2; 5; 8; 11 |]
                    [| 3; 6; 9; 12 |] |]) ]

[<TestCaseSource(nameof getColumnsTestCases)>]
let getColumns (a: int [,]) = getColumns a

let rotateTestCases =
    [ TestCaseData(array2D [ [ 1 ] ])
        .Returns(array2D [ [ 1 ] ])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ])
          .Returns(array2D [ [ 10; 7; 4; 1 ]
                             [ 11; 8; 5; 2 ]
                             [ 12; 9; 6; 3 ] ]) ]

[<TestCaseSource(nameof rotateTestCases)>]
let rotate (a: int [,]) = rotate a

let flipTestCases =
    [ TestCaseData(array2D [ [ 1 ] ])
        .Returns(array2D [ [ 1 ] ])
      TestCaseData(array2D [ [ 1; 2; 3 ]
                             [ 4; 5; 6 ]
                             [ 7; 8; 9 ]
                             [ 10; 11; 12 ] ])
          .Returns(array2D [ [ 3; 2; 1 ]
                             [ 6; 5; 4 ]
                             [ 9; 8; 7 ]
                             [ 12; 11; 10 ] ]) ]

[<TestCaseSource(nameof flipTestCases)>]
let flip (a: int [,]) = flip a

[<Test>]
let getOrientations () =
    let grid =
        array2D [ [ 10; 11 ]
                  [ 12; 13 ]
                  [ 14; 15 ] ]

    let actual = getOrientations grid

    let expected =
        [| array2D [ [ 10; 11 ]
                     [ 12; 13 ]
                     [ 14; 15 ] ]
           array2D [ [ 14; 12; 10 ]
                     [ 15; 13; 11 ] ]
           array2D [ [ 15; 14 ]
                     [ 13; 12 ]
                     [ 11; 10 ] ]
           array2D [ [ 11; 13; 15 ]
                     [ 10; 12; 14 ] ]
           array2D [ [ 11; 10 ]
                     [ 13; 12 ]
                     [ 15; 14 ] ]
           array2D [ [ 15; 13; 11 ]
                     [ 14; 12; 10 ] ]
           array2D [ [ 14; 15 ]
                     [ 12; 13 ]
                     [ 10; 11 ] ]
           array2D [ [ 10; 12; 14 ]
                     [ 11; 13; 15 ] ] |]

    Assert.AreEqual(expected, actual)

let combineTestCases =
    [ TestCaseData(array2D [ [ array2D [ [ 1; 2; 3 ]; [ 7; 8; 9 ] ]
                               array2D [ [ 4; 5; 6 ]; [ 10; 11; 12 ] ] ]
                             [ array2D [ [ 13; 14; 15 ]
                                         [ 19; 20; 21 ] ]
                               array2D [ [ 16; 17; 18 ]
                                         [ 22; 23; 24 ] ] ] ])
        .Returns(array2D [ [ 1; 2; 3; 4; 5; 6 ]
                           [ 7; 8; 9; 10; 11; 12 ]
                           [ 13; 14; 15; 16; 17; 18 ]
                           [ 19; 20; 21; 22; 23; 24 ] ]) ]

[<TestCaseSource(nameof combineTestCases)>]
let combine (a: int [,] [,]) = combine a
