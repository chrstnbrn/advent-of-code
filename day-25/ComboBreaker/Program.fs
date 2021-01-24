open System

let transformOnce subjectNumber value = value * subjectNumber % 20201227L

let transform subjectNumber loopSize =
    [ 1 .. loopSize ]
    |> Seq.fold (fun value _ -> transformOnce subjectNumber value) 1L

let getLoopSize publicKey =
    Seq.initInfinite ((+) 1)
    |> Seq.scan (fun (_, value) loopSize -> (loopSize, transformOnce 7L value)) (0, 1L)
    |> Seq.find (snd >> (=) publicKey)
    |> fst

let getEncryptionKey cardPublicKey doorPublicKey =
    cardPublicKey
    |> getLoopSize
    |> transform doorPublicKey

[<EntryPoint>]
let main argv =
    let cardPublicKey = 1526110L
    let doorPublicKey = 20175123L

    getEncryptionKey cardPublicKey doorPublicKey
    |> printfn "The encryption key is %d"

    0
