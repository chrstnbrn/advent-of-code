open System.IO

type GameState =
    { Player1Deck: Deck
      Player2Deck: Deck }

and Deck = int list

and FinalGameState =
    | Player1Win of Deck
    | Player2Win of Deck

and Player =
    | Player1
    | Player2

and PlayerState = { Card: int; Deck: Deck }

let createGameState player1Deck player2Deck =
    { Player1Deck = player1Deck
      Player2Deck = player2Deck }

let playDeck deck =
    { Card = List.head deck
      Deck = List.tail deck }

let play state =
    (playDeck state.Player1Deck, playDeck state.Player2Deck)

let getWinningDeck =
    function
    | Player1Win deck -> deck
    | Player2Win deck -> deck

let getWinner =
    function
    | Player1Win _ -> Player1
    | Player2Win _ -> Player2

let getWinnerByHighestCard player1 player2 =
    if player1.Card > player2.Card then
        Player1
    else
        Player2

let getNextState getWinner state =
    let (player1, player2) = play state

    match getWinner player1 player2 with
    | Player1 -> createGameState (player1.Deck @ [ player1.Card; player2.Card ]) player2.Deck
    | Player2 -> createGameState player1.Deck (player2.Deck @ [ player2.Card; player1.Card ])

module Combat =
    let rec play state =
        state
        |> tryGetFinalState
        |> Option.defaultWith
            (fun () ->
                state
                |> getNextState getWinnerByHighestCard
                |> play)

    and tryGetFinalState =
        function
        | { Player2Deck = [] } as state -> Some(Player1Win state.Player1Deck)
        | { Player1Deck = [] } as state -> Some(Player2Win state.Player2Deck)
        | _ -> None

module Option =
    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some(f x)
        | _ -> None

module RecursiveCombat =
    let rec play previousRounds state =
        state
        |> tryGetFinalState previousRounds
        |> Option.defaultWith
            (fun () ->
                state
                |> getNextState getRoundWinner
                |> play (Set.add state previousRounds))

    and tryGetFinalState previousRounds =
        function
        | { Player2Deck = [] } as state -> Some(Player1Win state.Player1Deck)
        | { Player1Deck = [] } as state -> Some(Player2Win state.Player2Deck)
        | state when Set.contains state previousRounds -> Some(Player1Win state.Player1Deck)
        | _ -> None

    and getRoundWinner player1 player2 =
        getSubGameWinner player1 player2
        |> Option.defaultWith (fun () -> getWinnerByHighestCard player1 player2)

    and getSubGameWinner player1 player2 =
        if player1.Deck.Length >= player1.Card
           && player2.Deck.Length >= player2.Card then
            let getSubGameDeck p = p.Deck |> List.take p.Card

            createGameState (getSubGameDeck player1) (getSubGameDeck player2)
            |> play Set.empty
            |> getWinner
            |> Some
        else
            None

let calculateScore =
    List.rev
    >> List.mapi (fun i card -> (i + 1) * card)
    >> List.sum

let getScore player1Deck player2Deck =
    createGameState player1Deck player2Deck
    |> Combat.play
    |> getWinningDeck
    |> calculateScore

let getScoreAfterRecursiveCombat player1Deck player2Deck =
    createGameState player1Deck player2Deck
    |> RecursiveCombat.play Set.empty
    |> getWinningDeck
    |> calculateScore

let getDeck title =
    Array.skipWhile ((<>) title)
    >> Array.skip 1
    >> Array.takeWhile ((<>) "")
    >> Array.map int
    >> Array.toList

[<EntryPoint>]
let main argv =
    let fileContent = File.ReadAllLines "./input.txt"

    let player1Deck = getDeck "Player 1:" fileContent
    let player2Deck = getDeck "Player 2:" fileContent

    let score = getScore player1Deck player2Deck
    printfn "The winning player's score is %d" score

    let scoreAfterRecursiveCombat =
        getScoreAfterRecursiveCombat player1Deck player2Deck

    printfn "The winning player's score after recursive combat is %d" scoreAfterRecursiveCombat

    0
