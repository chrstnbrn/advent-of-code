module JurassicJigsaw.Tests

open NUnit.Framework
open Program

[<Test>]
let parseTile () =
    let tileText =
        [| "Tile 2311:"
           "..##"
           "##.."
           "#..."
           "####" |]

    let actual = parseTile tileText

    let expected =
        { Tile.Id = 2311
          Tile.Content =
              array2D [ [ '.'; '.'; '#'; '#' ]
                        [ '#'; '#'; '.'; '.' ]
                        [ '#'; '.'; '.'; '.' ]
                        [ '#'; '#'; '#'; '#' ] ] }

    Assert.AreEqual(expected, actual)

[<Test>]
let getCornerIds () =
    let tiles =
        [| [| "Tile 2311:"
              "..##.#..#."
              "##..#....."
              "#...##..#."
              "####.#...#"
              "##.##.###."
              "##...#.###"
              ".#.#.#..##"
              "..#....#.."
              "###...#.#."
              "..###..###" |]
           [| "Tile 1951:"
              "#.##...##."
              "#.####...#"
              ".....#..##"
              "#...######"
              ".##.#....#"
              ".###.#####"
              "###.##.##."
              ".###....#."
              "..#.#..#.#"
              "#...##.#.." |]
           [| "Tile 1171:"
              "####...##."
              "#..##.#..#"
              "##.#..#.#."
              ".###.####."
              "..###.####"
              ".##....##."
              ".#...####."
              "#.##.####."
              "####..#..."
              ".....##..." |]
           [| "Tile 1427:"
              "###.##.#.."
              ".#..#.##.."
              ".#.##.#..#"
              "#.#.#.##.#"
              "....#...##"
              "...##..##."
              "...#.#####"
              ".#.####.#."
              "..#..###.#"
              "..##.#..#." |]
           [| "Tile 1489:"
              "##.#.#...."
              "..##...#.."
              ".##..##..."
              "..#...#..."
              "#####...#."
              "#..#.#.#.#"
              "...#.#.#.."
              "##.#...##."
              "..##.##.##"
              "###.##.#.." |]
           [| "Tile 2473:"
              "#....####."
              "#..#.##..."
              "#.##..#..."
              "######.#.#"
              ".#...#.#.#"
              ".#########"
              ".###.#..#."
              "########.#"
              "##...##.#."
              "..###.#.#." |]
           [| "Tile 2971:"
              "..#.#....#"
              "#...###..."
              "#.#.###..."
              "##.##..#.."
              ".#####..##"
              ".#..####.#"
              "#..#.#..#."
              "..####.###"
              "..#.#.###."
              "...#.#.#.#" |]
           [| "Tile 2729:"
              "...#.#.#.#"
              "####.#...."
              "..#.#....."
              "....#..#.#"
              ".##..##.#."
              ".#.####..."
              "####.#.#.."
              "##.####..."
              "##..#.##.."
              "#.##...##." |]
           [| "Tile 3079:"
              "#.#.#####."
              ".#..######"
              "..#......."
              "######...."
              "####.#..#."
              ".#...#.##."
              "#.#####.##"
              "..#.###..."
              "..#......."
              "..#.###..." |] |]

    let actual = getCornerIds tiles

    let expected = Set [| 1951; 3079; 2971; 1171 |]
    Assert.AreEqual(expected, actual)

[<Test>]
let getWaterRoughness () =
    let tiles =
        [| [| "Tile 2311:"
              "..##.#..#."
              "##..#....."
              "#...##..#."
              "####.#...#"
              "##.##.###."
              "##...#.###"
              ".#.#.#..##"
              "..#....#.."
              "###...#.#."
              "..###..###" |]
           [| "Tile 1951:"
              "#.##...##."
              "#.####...#"
              ".....#..##"
              "#...######"
              ".##.#....#"
              ".###.#####"
              "###.##.##."
              ".###....#."
              "..#.#..#.#"
              "#...##.#.." |]
           [| "Tile 1171:"
              "####...##."
              "#..##.#..#"
              "##.#..#.#."
              ".###.####."
              "..###.####"
              ".##....##."
              ".#...####."
              "#.##.####."
              "####..#..."
              ".....##..." |]
           [| "Tile 1427:"
              "###.##.#.."
              ".#..#.##.."
              ".#.##.#..#"
              "#.#.#.##.#"
              "....#...##"
              "...##..##."
              "...#.#####"
              ".#.####.#."
              "..#..###.#"
              "..##.#..#." |]
           [| "Tile 1489:"
              "##.#.#...."
              "..##...#.."
              ".##..##..."
              "..#...#..."
              "#####...#."
              "#..#.#.#.#"
              "...#.#.#.."
              "##.#...##."
              "..##.##.##"
              "###.##.#.." |]
           [| "Tile 2473:"
              "#....####."
              "#..#.##..."
              "#.##..#..."
              "######.#.#"
              ".#...#.#.#"
              ".#########"
              ".###.#..#."
              "########.#"
              "##...##.#."
              "..###.#.#." |]
           [| "Tile 2971:"
              "..#.#....#"
              "#...###..."
              "#.#.###..."
              "##.##..#.."
              ".#####..##"
              ".#..####.#"
              "#..#.#..#."
              "..####.###"
              "..#.#.###."
              "...#.#.#.#" |]
           [| "Tile 2729:"
              "...#.#.#.#"
              "####.#...."
              "..#.#....."
              "....#..#.#"
              ".##..##.#."
              ".#.####..."
              "####.#.#.."
              "##.####..."
              "##..#.##.."
              "#.##...##." |]
           [| "Tile 3079:"
              "#.#.#####."
              ".#..######"
              "..#......."
              "######...."
              "####.#..#."
              ".#...#.##."
              "#.#####.##"
              "..#.###..."
              "..#......."
              "..#.###..." |] |]

    let actual = getWaterRoughness tiles

    Assert.AreEqual(273, actual)

[<Test>]
let replaceMask () =
    let grid =
        array2D [ [ '.'; '#'; '#'; '.'; '#' ]
                  [ '#'; '#'; '#'; '.'; '#' ]
                  [ '.'; '#'; '.'; '#'; '#' ]
                  [ '#'; '#'; '#'; '#'; '.' ] ]

    let mask =
        array2D [ [ ' '; '#'; '#' ]
                  [ '#'; '#'; ' ' ] ]

    let actual = replaceMask mask grid

    let expected =
        array2D [ [ '.'; 'o'; 'o'; '.'; '#' ]
                  [ 'o'; 'o'; '#'; '.'; '#' ]
                  [ '.'; '#'; '.'; 'o'; 'o' ]
                  [ '#'; '#'; 'o'; 'o'; '.' ] ]

    Assert.AreEqual(expected, actual)
