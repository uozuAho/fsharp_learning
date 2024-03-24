namespace range

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    let a_greater_than_b(a, b) =
        match a, b with
        | Closed x, Closed y
        | Closed x, Open y
        | Open x, Closed y
        | Open x, Open y ->
            x > y

    type Range private (a:Endpoint, b:Endpoint) =
        member this.a = a
        member this.b = b
        static member create(a, b) =
            if a_greater_than_b(a, b) then failwith "Invalid range"
            match a, b with
            | Closed _, Closed _ -> Range(a, b)
            | Closed x, Open y
            | Open x, Closed y
            | Open x, Open y ->
                if x = y then failwith "Open range cannot be degenerate"
                else Range(a, b)

    let contains (range:Range) value =
        let a, b = range.a, range.b
        match a, b with
        | Open s, Open e -> s < value && value < e
        | Open s, Closed e -> s < value && value <= e
        | Closed s, Open e -> s <= value && value < e
        | Closed s, Closed e -> s <= value && value <= e
