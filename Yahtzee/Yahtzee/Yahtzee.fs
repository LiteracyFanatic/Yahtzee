﻿namespace Yahtzee.Core

module Score =
    #if INTERACTIVE
        #r "../packages/FSharpx.Collections.1.15.2/lib/net40/FSharpx.Collections.dll"
        #r "../packages/FSharpx.Extras.2.2.0/lib/net45/FSharpx.Extras.dll"
    #endif
    open FSharpx.Collections
    open FSharpx.Functional
    open FSharpx.Functional.Lens.Operators

    type Score = Score of int

    let getPoints = function
        | None -> 0 
        | Some(Score(s)) -> s

    let printScoreOption (s: Score option) =
        match s with
        | Some(Score(n)) -> n.ToString()
        | None -> "__"

    type Category = 
        | Aces
        | Twos
        | Threes
        | Fours
        | Fives
        | Sixes
        | ThreeOfAKind
        | FourOfAKind
        | FullHouse
        | SmallStraight
        | LargeStraight
        | Yahtzee
        | Chance
        override x.ToString() =
            match x with
                | Aces -> "Aces"
                | Twos -> "Twos"
                | Threes -> "Threes"
                | Fours -> "Fours"
                | Fives -> "Fives"
                | Sixes -> "Sixes"
                | ThreeOfAKind -> "Three-Of-A-Kind"
                | FourOfAKind -> "Four-Of-A-Kind"
                | FullHouse -> "Full House"
                | SmallStraight -> "Small Straight"
                | LargeStraight -> "Large Straight"
                | Yahtzee -> "Yahtzee"
                | Chance -> "Chance"
    
    let (|Upper|Lower|) c =
        match c with
        | Aces
        | Twos
        | Threes
        | Fours
        | Fives
        | Sixes -> Upper
        | ThreeOfAKind
        | FourOfAKind
        | FullHouse
        | SmallStraight
        | LargeStraight
        | Yahtzee
        | Chance -> Lower

    type Scorecard = { 
        Aces: Score option
        Twos: Score option
        Threes: Score option
        Fours: Score option
        Fives: Score option
        Sixes: Score option
        ThreeOfAKind: Score option
        FourOfAKind: Score option
        FullHouse: Score option
        SmallStraight: Score option
        LargeStraight: Score option
        Yahtzee: Score option
        Chance: Score option
        AvailableCategories: Set<Category>
    } with
        static member aces =
            { Get = fun x -> x.Aces
              Set = fun v x -> { x with Aces = v } }
        static member twos =
            { Get = fun x -> x.Twos
              Set = fun v x -> { x with Twos = v } }
        static member threes =
            { Get = fun x -> x.Threes
              Set = fun v x -> { x with Threes = v } }
        static member fours =
            { Get = fun x -> x.Fours
              Set = fun v x -> { x with Fours = v } }
        static member fives =
            { Get = fun x -> x.Fives
              Set = fun v x -> { x with Fives = v } }
        static member sixes =
            { Get = fun x -> x.Sixes
              Set = fun v x -> { x with Sixes = v } }
        static member threeOfAKind =
            { Get = fun x -> x.ThreeOfAKind
              Set = fun v x -> {x with ThreeOfAKind = v}}
        static member fourOfAKind =
            { Get = fun x -> x.FourOfAKind
              Set = fun v x -> {x with FourOfAKind = v}}
        static member fullHouse =
            { Get = fun x -> x.FullHouse
              Set = fun v x -> {x with FullHouse = v}}
        static member smallStraight =
            { Get = fun x -> x.SmallStraight
              Set = fun v x -> {x with SmallStraight = v}}
        static member largeStraight =
            { Get = fun x -> x.LargeStraight
              Set = fun v x -> {x with LargeStraight = v}}
        static member yahtzee =
            { Get = fun x -> x.Yahtzee
              Set = fun v x -> {x with Yahtzee = v}}
        static member chance =
            { Get = fun x -> x.Chance
              Set = fun v x -> {x with Chance = v}}
        static member avaiableCategories =
            { Get = fun x -> x.AvailableCategories
              Set = fun v x -> {x with AvailableCategories = v}}
        override x.ToString() =
            sprintf "Scorecard\n\nUpper:\nAces = %s\nTwos = %s\nThrees = %s\nFours = %s\nFives = %s\nSixes =%s\n\nLower:\nThree-Of-A-Kind = %s\nFour-Of-A-Kind = %s\nFull House = %s\nSmall Straight = %s\nLarge Straight = %s\nYahtzee = %s\nChance = %s"
                (printScoreOption x.Aces)
                (printScoreOption x.Twos)
                (printScoreOption x.Threes)
                (printScoreOption x.Fours)
                (printScoreOption x.Fives)
                (printScoreOption x.Sixes)
                (printScoreOption x.ThreeOfAKind)
                (printScoreOption x.FourOfAKind)
                (printScoreOption x.FullHouse)
                (printScoreOption x.SmallStraight)
                (printScoreOption x.LargeStraight)
                (printScoreOption x.Yahtzee)
                (printScoreOption x.Chance)

    let tallyScore sc =
        let upperScore = 
            [ sc.Aces
              sc.Twos
              sc.Threes
              sc.Fours
              sc.Fives
              sc.Sixes ]
            |> List.sumBy getPoints
        
        let lowerScore = 
            [ sc.ThreeOfAKind
              sc.FourOfAKind
              sc.FullHouse
              sc.SmallStraight
              sc.LargeStraight
              sc.Yahtzee
              sc.Chance ]
            |> List.sumBy getPoints

        let upperBonus = if upperScore >= 63 then 35 else 0
        Score(upperScore + lowerScore + upperBonus)

    type Die = 
        | Die of int
        override x.ToString() =
            let (Die(n)) = x
            sprintf "|%s|" (n.ToString())

    type Dice = 
        | Dice of Die list
        override x.ToString() =
            let (Dice(ds)) = x
            ds
            |> List.map string
            |> String.concat " "

    let sumOfDice = List.sumBy (fun (Die(n)) -> n)

    type DiceChoice =
        | Hold
        | Roll

//    type Command =
//        | MarkScore of Category
//        | RollDice
//        override x.ToString() = 
//            match x with
//            | MarkScore c -> c.ToString()
//            | RollDice -> "RollDice"


//    let startOfGameCommands = 
//        Set.ofList 
//            [ MarkScore(Aces)
//              MarkScore(Twos)
//              MarkScore(Threes)
//              MarkScore(Fours)
//              MarkScore(Fives)
//              MarkScore(Sixes)
//              MarkScore(ThreeOfAKind)
//              MarkScore(FourOfAKind)
//              MarkScore(FullHouse)
//              MarkScore(SmallStraight)
//              MarkScore(LargeStraight)
//              MarkScore(Yahtzee)
//              MarkScore(Chance) ]

    let rand = System.Random()

    let rollDice cs (Dice(ds)) =
        (cs, ds) 
        ||> List.map2 (fun c d -> 
            match c with 
            | Hold -> d 
            | Roll -> Die(rand.Next(1, 7)))
        |> Dice


    let scoreNs n (Dice(ds)) =
        ds
        |> List.filter ((=) (Die(n)))
        |> sumOfDice
        |> Score

    let scoreAces = scoreNs 1 
    let scoreTwos = scoreNs 2
    let scoreThrees = scoreNs 3
    let scoreFours = scoreNs 4
    let scoreFives = scoreNs 5
    let scoreSixes = scoreNs 6
    
    let scoreNOfAKind n (Dice(ds)) =
        let valid = 
            ds
            |> List.countBy id
            |> List.exists (fun (_, count) -> count >= n)
        if valid then sumOfDice ds else 0
        |> Score

    let scoreThreeOfAKind = scoreNOfAKind 3
    let scoreFourOfAKind = scoreNOfAKind 4

    let scoreFullHouse (Dice(ds)) =
        match (ds |> List.groupBy id |> List.length) with
        | 2 -> 25
        | _ -> 0
        |> Score

    let isStraight (Dice(ds)) =
        ds
        |> List.pairwise
        |> List.forall (fun (Die(a), Die(b)) -> a + 1 = b)

    let scoreSmallStraight (Dice(ds)) =
        let valid = 
            ds
            |> List.sort
            |> List.distinct
            |> List.windowed 4
            |> List.map Dice
            |> List.exists isStraight
        if valid then 30 else 0
        |> Score

    let scoreLargeStraight (Dice(ds)) =
        let valid = 
            ds
            |> List.sort
            |> Dice
            |> isStraight
        if valid then 40 else 0
        |> Score

    let scoreYahtzee (Dice(ds)) =
         match (ds |> List.distinct |> List.length) with
         | 1 -> 50
         | _ -> 0
         |> Score

    let scoreChance (Dice(ds)) =
        ds
        |> sumOfDice
        |> Score

module Game =
    
    open FSharpx.Collections
    open FSharpx.Functional
    open FSharpx.Functional.Lens.Operators
    open Score

    type Player = {
        Name: string 
        Scorecard: Scorecard
//        Commands: Set<Command>
    } with
        static member name =
            { Get = fun x -> x.Name
              Set = fun v x -> { x with Name = v } } 
        static member scorecard =
            { Get = fun x -> x.Scorecard
              Set = fun v x -> { x with Scorecard = v } }
//        static member commands =
//            { Get = fun x -> x.Commands
//              Set = fun v x -> { x with Commands = v } }
        override x.ToString() =
//            let commands =
//                x.Commands
//                |> Set.toList 
//                |> List.map string
//                |> String.concat "\n"
            sprintf "Name: %s\n\n\n%s\n"
                (x.Name)
                (x.Scorecard.ToString())
//                (commands)

    let allCategories =
        Set.ofList [
            Aces
            Twos
            Threes
            Fours
            Fives
            Sixes
            ThreeOfAKind
            FourOfAKind
            FullHouse
            SmallStraight
            LargeStraight
            Yahtzee
            Chance ]

    type GameDetails = { 
        ActivePlayer: Player
        OtherPlayers: Queue<Player>
        Dice: Dice
//        DiceChoice: DiceChoice List
//        Rolls: int 
    } with
        static member activePlayer =
            { Get = fun x -> x.ActivePlayer
              Set = fun v x -> { x with ActivePlayer = v}}
        static member otherPlayers =
            { Get = fun x -> x.OtherPlayers
              Set = fun v x -> {x with OtherPlayers = v} }
//        static member diceChoice =
//            { Get = fun x -> x.DiceChoice
//              Set = fun v x -> {x with DiceChoice = v} }
//        static member rolls =
//            { Get = fun x -> x.Rolls
//              Set = fun v x -> { x with Rolls = v } }
        static member dice =
            { Get = fun x -> x.Dice
              Set = fun v x -> { x with Dice = v } }
        override x.ToString() =
            let others =
                x.OtherPlayers
                |> Queue.toSeq
                |> Seq.map string
                |> String.concat "\n\n\n"
            sprintf "Dice = %s\n\n\nActive Player\n----------\n\n%s\n\nOther Players\n----------\n\n%s"
                (x.Dice.ToString())
//                (x.Rolls.ToString())
                (x.ActivePlayer.ToString())
                (others)
       
    type GameState =
        | InitialRoll
        | SecondRoll
        | FinalRoll

//    [<StructuralEquality>]
    type Game = {
        State: GameState
        Details: GameDetails
        CapabilityProvider: CapabilityProvider
    } with
        static member state =
            { Get = fun x -> x.State
              Set = fun v x -> { x with State = v } }
        static member details =
            { Get = fun x -> x.Details
              Set = fun v x -> { x with Details = v } }

    and RollCapability = DiceChoice list -> Game -> Game

    and MarkScoreCapability = Category -> Game -> Game

    and CapabilityProvider = {
        RollProvider: GameState -> RollCapability option
        MarkScoreProvider: GameState -> MarkScoreCapability option
    }

    type Capabilities = {
        Roll: RollCapability option
        MarkScore: MarkScoreCapability option
    }

    let getAvailableCapabilities capabilityProvider gameState =
        { Roll = capabilityProvider.RollProvider gameState
          MarkScore = capabilityProvider.MarkScoreProvider gameState }
         
    let updatePlayerScore f l = 
        f >> Some >> (Player.scorecard >>| l).Set


    let scorePlayer = function
        | Aces -> 
            updatePlayerScore scoreAces Scorecard.aces
        | Twos -> 
            updatePlayerScore scoreTwos Scorecard.twos
        | Threes -> 
            updatePlayerScore scoreThrees Scorecard.threes
        | Fours -> 
            updatePlayerScore scoreFours Scorecard.fours
        | Fives -> 
            updatePlayerScore scoreFives Scorecard.fives
        | Sixes -> 
            updatePlayerScore scoreSixes Scorecard.sixes
        | ThreeOfAKind -> 
            updatePlayerScore scoreFives Scorecard.fives
        | FourOfAKind -> 
            updatePlayerScore scoreThreeOfAKind Scorecard.threeOfAKind
        | FullHouse ->  
            updatePlayerScore scoreFullHouse Scorecard.fullHouse
        | SmallStraight -> 
            updatePlayerScore scoreSmallStraight Scorecard.smallStraight
        | LargeStraight -> 
            updatePlayerScore scoreLargeStraight Scorecard.largeStraight
        | Yahtzee -> 
            updatePlayerScore scoreYahtzee Scorecard.yahtzee
        | Chance -> 
            updatePlayerScore scoreChance Scorecard.chance

    let activePlayerAvailableCategories =
        Game.details 
        >>| GameDetails.activePlayer 
        >>| Player.scorecard 
        >>| Scorecard.avaiableCategories

    let setLastPlayer p = GameDetails.otherPlayers.Update (Queue.tail >> Queue.conj p)
    
    let markScore c g = 
        ((Game.details >>| GameDetails.activePlayer).Update 
            (scorePlayer c ((Game.details >>| GameDetails.dice).Get g))
        >> (activePlayerAvailableCategories.Update (Set.remove c))) g

    let markScoreProvider = function
        | InitialRoll
        | SecondRoll 
        | FinalRoll -> Some(markScore)

//    let mapIf p f1 v = if p v then f1 v else v

//    let roll diceChoices =
//        GameDetails.dice.Update (rollDice diceChoices) 
//        >> (GameDetails.rolls += -1)
//        >> mapIf 
//            (GameDetails.rolls.Get >> (<) 1)
//            (activePlayerCommands.Update (Set.remove RollDice))

    let roll diceChoices =
        (Game.details >>| GameDetails.dice).Update (rollDice diceChoices)  
        >> (Game.state.Update (function
            | InitialRoll -> SecondRoll
            | SecondRoll -> FinalRoll
            | FinalRoll -> FinalRoll))

    let rollProvider = function
        | InitialRoll
        | SecondRoll -> Some(roll)
        | FinalRoll -> None

    let getNextPlayer = (Game.details >>| GameDetails.otherPlayers).Get >> Queue.head

    let getActivePlayer = (Game.details >>| GameDetails.activePlayer).Get

    let movePlayers g =
        g
        |> (Game.details >>| GameDetails.activePlayer).Set (getNextPlayer g)
        |> (Game.details >>| GameDetails.otherPlayers).Update 
            (g |> (getActivePlayer >> Queue.conj) 
            >> Queue.tail )
            
//    let resetDice = GameDetails.rolls.Set 2

//    let nextTurn =
//        movePlayers
//        >> resetDice
//        >> activePlayerCommands.Update (Set.add RollDice)
//
//    let exec command gameDetails =
//        match command with
//        | MarkScore category ->
//            gameDetails
//            |> updateScore category 
//            |> nextTurn
//        | RollDice ->
//            roll gameDetails 
    


//module TestApp =
//
//    open System.Windows
//
//    let f = new Form()
//    let lb = new ListBox()
//
//    let connectToListBox =
//        lb.Items.Clear()
//        myGame.ActivePlayer.Commands 
//        |> Set.toArray
//        |> Array.map box
//        |> lb.Items.AddRange
//
//    connectToListBox
//
//    lb.DoubleClick.Add (fun e ->
//        myGame <- exec (unbox lb.SelectedItem) myGame
//        connectToListBox
//        printfn "%s" (myGame.ToString()))
//
//    f.Controls.Add lb
//    f.Show()
//
//    roll myGame

    //myGame <- exec  (MarkScore(Yahtzee)) myGame
    //printfn "%s" (myGame.ToString())
    //
    //myGame
    //|> exec RollDice
    //|> exec RollDice
    //|> exec (MarkScore(ThreeOfAKind))
    //|> exec RollDice
    //|> exec (MarkScore(Twos))
    //|> printfn "%O"
