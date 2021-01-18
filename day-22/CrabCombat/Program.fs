open System.IO

type GameState =
    { Player1Deck: Deck
      Player2Deck: Deck }

and Deck = int list

let getWinningDeck =
    function
    | { Player1Deck = []; Player2Deck = deck } -> deck
    | { Player1Deck = deck; Player2Deck = [] } -> deck
    | _ -> failwith "The game is not over yet"

let isGameOver state =
    List.isEmpty state.Player1Deck
    || List.isEmpty state.Player2Deck

let playCard deck = (List.head deck, List.tail deck)

let rec play state =
    if (isGameOver state) then
        state
    else
        let (player1Card, player1Deck) = playCard state.Player1Deck
        let (player2Card, player2Deck) = playCard state.Player2Deck

        let player1Wins = player1Card > player2Card

        let newState =
            if player1Wins then
                { Player1Deck = player1Deck @ [ player1Card; player2Card ]
                  Player2Deck = player2Deck }
            else
                { Player1Deck = player1Deck
                  Player2Deck = player2Deck @ [ player2Card; player1Card ] }

        play newState

let calculateScore =
    getWinningDeck
    >> List.rev
    >> List.mapi (fun i card -> (i + 1) * card)
    >> List.sum

let getScore player1Deck player2Deck =
    let initialState =
        { Player1Deck = player1Deck
          Player2Deck = player2Deck }

    initialState |> play |> calculateScore

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

    0
