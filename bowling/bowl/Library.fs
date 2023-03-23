namespace bowl

module Bowling =
    type NonLastFrame =
        | Normal of int * int
        | Spare of int
        | Strike
    
    type LastFrame =
        | Normal of int * int
        | WithBonus of int * int * int
        
    type Frame =
        | NonLast of NonLastFrame
        | Last of LastFrame
    
    type Game = {
        Frames: NonLastFrame list
        LastFrame: LastFrame
    }
    
    type Remainder = Game
    
    let restOf game =
        let rem =
            match game.Frames with
            | head::rest -> rest
            | [] -> []
        { Frames = rem; LastFrame = game.LastFrame }
        
    let nextTwoRolls game =
        match game.Frames with
        | head::rest -> match head with
                        | NonLastFrame.Normal (a, b) -> a, b
                        | Spare a -> a, 10 - a
                        | Strike -> match rest with
                                    | head::rest -> match head with
                                                    | NonLastFrame.Normal (a, b) -> 10, a
                                                    | Spare a -> 10, a
                                                    | Strike -> 10, 10
                                    | [] -> 10, match game.LastFrame with
                                                | Normal (a, b) -> a
                                                | WithBonus (a, b, c) -> a
        | [] -> match game.LastFrame with
                | Normal (a, b) -> a, b
                | WithBonus (a, b, c) -> a, b
        
    let scoreNonLast frame remainder =
        let c, d = nextTwoRolls remainder
        match frame with
        | NonLastFrame.Normal (a, b) -> a + b
        | Spare pins -> 10 + c
        | Strike -> 10 + c + d
        
    let scoreLast (frame:LastFrame) =
        match frame with
        | Normal (a, b) -> a + b
        | WithBonus (a, b, c) -> a + b + c
        
    let scoreGame game =
        let framesScore = game.Frames |> List.map scoreNonLast |> List.sum
        let lastFrameScore = scoreLast game.LastFrame
        framesScore + lastFrameScore