open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Food =
    { Ingredients: Ingredient []
      ListedAllergens: Allergen [] }

and Ingredient = Ingredient of string

and Allergen = Allergen of string

let parseFood text =
    let pattern =
        "(?<Ingredients>.+) \(contains (?<Allergens>.+)\)"

    let result = Regex.Match(text, pattern)

    let ingredients =
        result.Groups.["Ingredients"].Value.Split(' ')
        |> Array.map Ingredient

    let allergens =
        result.Groups.["Allergens"].Value.Split(", ")
        |> Array.map Allergen

    { Ingredients = ingredients
      ListedAllergens = allergens }

let getPossibleIngredientsPerAllergen foods =
    foods
    |> Array.collect
        (fun food ->
            food.ListedAllergens
            |> Array.map (fun a -> (a, Set food.Ingredients)))
    |> Array.groupBy fst
    |> Array.map (fun (allergen, groups) -> (allergen, groups |> Array.map snd |> Set.intersectMany))


let rec getMapRecursively ingredientAllergenMap unknownIngredients =
    if Array.isEmpty unknownIngredients then
        ingredientAllergenMap
    else
        let foundElements =
            unknownIngredients
            |> Array.filter (fun (_, ingredients) -> Seq.length ingredients = 1)

        let newMap =
            foundElements
            |> Seq.fold
                (fun map (allergen, ingredients) -> Map.add (Seq.head ingredients) allergen map)
                ingredientAllergenMap

        let foundIngredients =
            foundElements |> Array.map snd |> Set.unionMany

        let newUnknownIngredients =
            unknownIngredients
            |> Array.except foundElements
            |> Array.map (fun (allergen, ingredients) -> (allergen, Set.difference ingredients foundIngredients))

        getMapRecursively newMap newUnknownIngredients


let getIngredientAllergenMap =
    getPossibleIngredientsPerAllergen
    >> getMapRecursively Map.empty<Ingredient, Allergen>

let countIngredientsWithoutAllergens (foodTexts: string []): int =
    let foods = foodTexts |> Array.map parseFood
    let map = getIngredientAllergenMap foods

    foods
    |> Array.collect (fun f -> f.Ingredients)
    |> Seq.filter (fun i -> not (Map.containsKey i map))
    |> Seq.length

let getCanonicalDangerousIngredientList (foodTexts: string []): string =
    foodTexts
    |> Array.map parseFood
    |> getIngredientAllergenMap
    |> Map.toArray
    |> Array.sortBy snd
    |> Array.map (fun ((Ingredient i), _) -> i)
    |> String.concat ","

[<EntryPoint>]
let main argv =
    let foods = File.ReadAllLines "./input.txt"

    let ingredientsWithoutAllergens = countIngredientsWithoutAllergens foods
    printfn "There are %d ingredients without allergens" ingredientsWithoutAllergens

    let list =
        getCanonicalDangerousIngredientList foods

    printfn "The canonical dangerious ingredient list is %s" list

    0
