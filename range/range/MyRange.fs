namespace range

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    type Range private (a:Endpoint, b:Endpoint) =
        member this.a = a
        member this.b = b
        static member create(a, b) = Range(a, b)


    let contains (range:Range) value =
        let a, b = range.a, range.b
        match a, b with
        | Open s, Open e -> s < value && value < e
        | Open s, Closed e -> s < value && value <= e
        | Closed s, Open e -> s <= value && value < e
        | Closed s, Closed e -> s <= value && value <= e
