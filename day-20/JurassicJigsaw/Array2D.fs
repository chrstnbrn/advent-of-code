module Array2D

let flatten (A: 'a [,]) = A |> Seq.cast<'a>
let getColumn c (A: _ [,]) = flatten A.[*, c..c] |> Seq.toArray
let getRow r (A: _ [,]) = flatten A.[r..r, *] |> Seq.toArray

let getRows (A: _ [,]) =
    seq {
        for row in [ 0 .. (Array2D.length1 A - 1) ] do
            yield getRow row A
    }
    |> Seq.toArray

let getColumns (A: _ [,]) =
    seq {
        for column in [ 0 .. (Array2D.length2 A - 1) ] do
            yield getColumn column A
    }
    |> Seq.toArray

let rotate grid =
    let m, n =
        Array2D.length1 grid, Array2D.length2 grid

    Array2D.init n m (fun i j -> Array2D.get grid (m - j - 1) i)

let flip grid =
    let width = Array2D.length2 grid

    grid
    |> Array2D.mapi (fun i j _ -> Array2D.get grid i (width - 1 - j))

let rec private times f x n =
    if n < 1 then
        x
    else
        times f (f x) (n - 1)

let getOrientations grid =
    let getRotations g =
        [| 0; 1; 2; 3 |] |> Array.map (times rotate g)

    [| grid; flip grid |]
    |> Array.collect getRotations

let combine a =
    let head = Array2D.get a 0 0
    let m = Array2D.length1 head
    let n = Array2D.length2 head

    let length1 = m * Array2D.length1 a
    let length2 = n * Array2D.length2 a

    Array2D.init length1 length2 (fun i j -> a.[i / m, j / n].[i % m, j % n])
