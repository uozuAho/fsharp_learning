namespace range

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    type Range = Endpoint * Endpoint

    let contains range value =
        let (start, stop) = range
        match start, stop with
        | Open s, Open e -> s < value && value < e
        | Open s, Closed e -> s < value && value <= e
        | Closed s, Open e -> s <= value && value < e
        | Closed s, Closed e -> s <= value && value <= e
