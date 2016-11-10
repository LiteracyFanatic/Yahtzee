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

let startCard = { 
    Aces = None
    Twos = None
    Threes = None
    Fours = None
    Fives = None
    Sixes = None
    ThreeOfAKind = None
    FourOfAKind = None
    FullHouse = None
    SmallStraight = None
    LargeStraight = None
    Yahtzee = None
    Chance = None
    AvailableCategories = allCategories
}

let myPlayer = { 
    Name = "Jordan"
    Scorecard = startCard
}
let player2 = {
    Name = "p2"
    Scorecard = startCard
}
let player3 = {
    Name = "p3"
    Scorecard = startCard
}
let player4 = {
    Name = "p4"
    Scorecard = startCard
}

let initialDetails = { 
    ActivePlayer = myPlayer
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
    CapabilityProvider = capabilityProvider
}

[<Test>]
let ``move players`` () =
    let newDetails = { 
        ActivePlayer = player2
        OtherPlayers = Queue.ofList [player3; player4; myPlayer]
        Dice = myDice
    }

    let newGame = { myGame with Details = newDetails }
    let moved = movePlayers myGame
    moved.Details |> should equal newGame.Details