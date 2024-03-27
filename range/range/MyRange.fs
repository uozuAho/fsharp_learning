namespace range

open System.Text.RegularExpressions

module MyRange =
    type Endpoint =
        | Open of int
        | Closed of int

    let private isValidRange(a, b) =
        match a, b with
        | Closed _, Closed _ -> true
        | Closed x, Open y
        | Open x, Closed y
        | Open x, Open y -> x < y

    let private parseEndpoints str =
        let pattern = @"(\[|\()(\d+),\s*(\d+)(\]|\))"
        let m = Regex.Match(str, pattern)
        if m.Success then
            let a = int m.Groups.[2].Value
            let b = int m.Groups.[3].Value
            let a = if m.Groups.[1].Value = "(" then Open a else Closed a
            let b = if m.Groups.[4].Value = ")" then Open b else Closed b
            Some(a, b)
        else None

    type Range private (a:Endpoint, b:Endpoint) =
        member this.a = a
        member this.b = b
        static member tryCreate(a, b) =
            if isValidRange(a, b) then Some(Range(a, b))
            else None
        static member tryCreate str =
            match parseEndpoints str with
            | Some(a, b) -> Range.tryCreate(a, b)
            | None -> None
        override this.Equals(obj) =
            match obj with
            | :? Range as other -> this.a = other.a && this.b = other.b
            | _ -> false
        override this.GetHashCode() = hash (a, b)

    let contains (range:Range) value =
        let a, b = range.a, range.b
        match a, b with
        | Open s, Open e -> s < value && value < e
        | Open s, Closed e -> s < value && value <= e
        | Closed s, Open e -> s <= value && value < e
        | Closed s, Closed e -> s <= value && value <= e

    let overlaps (range1:Range, range2:Range) =
        let a1, b1 = range1.a, range1.b
        let a2, b2 = range2.a, range2.b
        let adjacent1 =
            match b1, a2 with
            | Closed x, Closed y when x = y -> true
            | _ -> false
        let adjacent2 =
            match b2, a1 with
            | Closed x, Closed y when x = y -> true
            | _ -> false
        let b1isInside2 = contains range2 (match b1 with | Closed x -> x | Open x -> x)
        let a1isInside2 = contains range2 (match a1 with | Closed x -> x | Open x -> x)
        let b2isInside1 = contains range1 (match b2 with | Closed x -> x | Open x -> x)
        let a2isInside1 = contains range1 (match a2 with | Closed x -> x | Open x -> x)
        adjacent1 || adjacent2
        || b1isInside2
        || a1isInside2
        || b2isInside1
        || a2isInside1

