module LobbyLayout.Tests

open NUnit.Framework
open Program

[<Test>]
let getBlackTiles () =
    let instructions =
        [| "sesenwnenenewseeswwswswwnenewsewsw"
           "neeenesenwnwwswnenewnwwsewnenwseswesw"
           "seswneswswsenwwnwse"
           "nwnwneseeswswnenewneswwnewseswneseene"
           "swweswneswnenwsewnwneneseenw"
           "eesenwseswswnenwswnwnwsewwnwsene"
           "sewnenenenesenwsewnenwwwse"
           "wenwwweseeeweswwwnwwe"
           "wsweesenenewnwwnwsenewsenwwsesesenwne"
           "neeswseenwwswnwswswnw"
           "nenwswwsewswnenenewsenwsenwnesesenew"
           "enewnwewneswsewnwswenweswnenwsenwsw"
           "sweneswneswneneenwnewenewwneswswnese"
           "swwesenesewenwneswnwwneseswwne"
           "enesenwswwswneneswsenwnewswseenwsese"
           "wnwnesenesenenwwnenwsewesewsesesew"
           "nenewswnwewswnenesenwnesewesw"
           "eneswnwswnwsenenwnwnwwseeswneewsenese"
           "neswnwewnwnwseenwseesewsenwsweewe"
           "wseweeenwnesenwwwswnew" |]

    let actual = getBlackTiles instructions

    Assert.AreEqual(10, actual)


[<Test>]
let getBlackTilesAfterDays () =
    let instructions =
        [| "sesenwnenenewseeswwswswwnenewsewsw"
           "neeenesenwnwwswnenewnwwsewnenwseswesw"
           "seswneswswsenwwnwse"
           "nwnwneseeswswnenewneswwnewseswneseene"
           "swweswneswnenwsewnwneneseenw"
           "eesenwseswswnenwswnwnwsewwnwsene"
           "sewnenenenesenwsewnenwwwse"
           "wenwwweseeeweswwwnwwe"
           "wsweesenenewnwwnwsenewsenwwsesesenwne"
           "neeswseenwwswnwswswnw"
           "nenwswwsewswnenenewsenwsenwnesesenew"
           "enewnwewneswsewnwswenweswnenwsenwsw"
           "sweneswneswneneenwnewenewwneswswnese"
           "swwesenesewenwneswnwwneseswwne"
           "enesenwswwswneneswsenwnewswseenwsese"
           "wnwnesenesenenwwnenwsewesewsesesew"
           "nenewswnwewswnenesenwnesewesw"
           "eneswnwswnwsenenwnwnwwseeswneewsenese"
           "neswnwewnwnwseenwseesewsenwsweewe"
           "wseweeenwnesenwwwswnew" |]

    let actual = getBlackTilesAfterDays instructions 100

    Assert.AreEqual(2208, actual)
