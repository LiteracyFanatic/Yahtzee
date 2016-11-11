module Yahtzee.Tests.Score

open Yahtzee.Core
open Yahtzee.Core.Score
open Yahtzee.Core.Game
open NUnit.Framework
open FsUnit
open FSharpx.Collections

let myDice = 
    [1; 1; 1; 1; 1;] 
    |> List.map Die
    |> Dice

let myChoices = [Hold; Hold; Hold; Roll; Roll]


let player1 = createPlayer "Player 1"
let player2 = createPlayer "Player 2"
let player3 = createPlayer "Player 3"
let player4 = createPlayer "Player 4"


let initialDetails = { 
    ActivePlayer = player1
    OtherPlayers = Queue.ofList [player2; player3; player4]
    Dice = myDice
}

let capabilityProvider = {
    RollProvider = rollProvider
    MarkScoreProvider = markScoreProvider 
}

let myGame = {
    State = InitialRoll
    Details = initialDetails
}

let canCreateGameWith p r =
    match createGame p with
    | Success _ -> true
    | Error _ -> false
    |> should equal r

[<Test>]
let ``can create game with one player`` () = canCreateGameWith [player1] true

[<Test>]
let ``can create game with two players`` () = canCreateGameWith [player1; player2] true

[<Test>]
let ``can't create game zero players`` () = canCreateGameWith [] false


[<Test>]
let ``move players`` () =
    let newDetails = { 
        ActivePlayer = player2
        OtherPlayers = Queue.ofList [player3; player4; player1]
        Dice = myDice
    }

    let newGame = { myGame with Details = newDetails }

    movePlayers myGame |> should equal newGame

let cap = Game.getAvailableCapabilities capabilityProvider myGame
cap.MarkScore