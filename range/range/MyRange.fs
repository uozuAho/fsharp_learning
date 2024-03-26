namespace range

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    let isValidRange(a, b) =
        match a, b with
        | Closed _, Closed _ -> true
        | Closed x, Open y
        | Open x, Closed y
        | Open x, Open y -> x < y

    type Range private (a:Endpoint, b:Endpoint) =
        member this.a = a
        member this.b = b
        static member tryCreate(a, b) =
            if isValidRange(a, b) then Some(Range(a, b))
            else None

    let contains (range:Range) value =
        let a, b = range.a, range.b
        match a, b with
        | Open s, Open e -> s < value && value < e
        | Open s, Closed e -> s < value && value <= e
        | Closed s, Open e -> s <= value && value < e
        | Closed s, Closed e -> s <= value && value <= e
