namespace bowl

module Bowling =
    type Frame =
        | Normal of int * int
        | Spare of int
        | Strike
        | Last of int * int * int

    let next2Rolls rest =
        match rest with
        | [] -> failwith "no!"
        | Normal (a,b) :: rest -> a,b
        | Spare a :: rest -> a, 10 - a
        | Last (a,b,c) :: rest -> a,b
        | Strike :: rest ->
            match rest with
            | [] -> failwith "Npoo"
            | Normal (a,b) :: rest -> 10, a
            | Spare a :: rest -> 10, a
            | Strike :: rest -> 10, 10
            | Last (a,b,c) :: rest -> 10, a

    let scoreFrame frame rest =
        let baseScore =
            match frame with
            | Normal (a, b) -> a + b
            | Last (a, b, c) -> a + b + c
            | Spare _ -> 10
            | Strike -> 10
        let additionalScore =
            match frame with
            | Normal _ -> 0
            | Last _ -> 0
            | Spare _ ->
                let a, _ = next2Rolls rest
                a
            | Strike ->
                let a, b = next2Rolls rest
                a + b
        baseScore + additionalScore

    let rec scoreFrames (frames: Frame list) =
        match frames with
        | [] -> 0
        | _ ->
            let next = List.head frames
            let rest = List.tail frames
            scoreFrame next rest + scoreFrames rest
