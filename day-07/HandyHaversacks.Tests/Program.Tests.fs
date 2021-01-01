module HandyHaversacks.Tests

open NUnit.Framework
open Program

let rules =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
      "bright white bags contain 1 shiny gold bag."
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
      "faded blue bags contain no other bags."
      "dotted black bags contain no other bags." ]

[<Test>]
let getOuterBagColors () =
    let color = "shiny gold"
    let actual = getOuterBagColors rules color

    let expected =
        Set.ofList [ "bright white"
                     "muted yellow"
                     "dark orange"
                     "light red" ]

    Assert.AreEqual(expected, actual)

[<Test>]
let getNumberOfBagsInside () =
    let color = "shiny gold"
    let actual = getNumberOfBagsInside rules color
    Assert.AreEqual(32, actual)
