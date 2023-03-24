namespace bowl

module Bowling =
    type Frame =
        | Normal of int * int
        | Spare of int
        | Strike
        | Last of int * int * int

    // let next2Rolls rest =
    //     match rest with
    //     | [] -> failwith "no!"
    //     | Normal (a,b) :: rest -> a,b
    //     | Spare a :: rest -> a, 10 - a
    //     | Last (a,b,c) :: rest -> a,b
    //     | Strike :: rest ->
    //         match rest with
    //         | [] -> failwith "Npoo"
    //         | Normal (a,b) :: rest -> 10, a
    //         | Spare a :: rest -> 10, a
    //         | Strike :: rest -> 10, 10
    //         | Last (a,b,c) :: rest -> 10, a

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
                match rest with
                | [] -> failwith "last frame can't be a spare"
                | next :: rest ->
                    match next with
                    | Normal (a, _) -> a
                    | Last (a,_,_) -> a
                    | Spare a -> a
                    | Strike -> 10
            | Strike ->
                match rest with
                | [] -> failwith "last frame can't be a strike"
                | Normal (a,b) :: _ -> a + b
                | Last (a,b,c) :: _ -> a + b
                | Spare _ :: _ -> 10
                | Strike :: rest -> 10 +
                    match rest with
                    | [] -> failwith "last frame can't be a strike"
                    | Normal (a,_)::_ -> a
                    | Last (a,_,_)::_ -> a
                    | Spare a::_ -> a
                    | Strike::_ -> 10
        baseScore + additionalScore

    let rec scoreFrames (frames: Frame list) =
        match frames with
        | [] -> 0
        | _ ->
            let next = List.head frames
            let rest = List.tail frames
            scoreFrame next rest + scoreFrames rest
