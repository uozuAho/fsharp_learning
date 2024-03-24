namespace range

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    type Range = Endpoint * Endpoint

    let contains range value =
        true
