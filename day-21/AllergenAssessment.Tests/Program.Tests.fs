module AllergenAssessment.Tests

open NUnit.Framework
open Program

[<Test>]
let countIngredientsWithoutAllergens () =
    let foods =
        [| "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
           "trh fvjkl sbzzf mxmxvkd (contains dairy)"
           "sqjhc fvjkl (contains soy)"
           "sqjhc mxmxvkd sbzzf (contains fish)" |]

    let actual = countIngredientsWithoutAllergens foods

    Assert.AreEqual(5, actual)
