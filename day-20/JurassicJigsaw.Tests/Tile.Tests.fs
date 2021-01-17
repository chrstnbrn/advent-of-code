module Tile.Tests

open NUnit.Framework

[<Test>]
let removeBorder () =
    let tile =
        { Tile.Id = 0
          Tile.Content =
              array2D [ [ ' '; '#'; ' '; '#' ]
                        [ '#'; '#'; ' '; ' ' ]
                        [ '#'; ' '; ' '; '#' ] ] }

    let actual = removeBorder tile

    let expected = array2D [ [ '#'; ' ' ] ]
    Assert.AreEqual(expected, actual)
