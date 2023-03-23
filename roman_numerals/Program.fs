module roman

let quotRem num divisor =
    let quotient = num / divisor
    let remainder = num % divisor
    (quotient, remainder)

let nextNumerals num div numeral thresh nextNumeral =
    let numerals = ""
    let quot, rem = quotRem num div
    let numerals = numerals + String.replicate quot numeral
    let num = num - quot * div
    let numerals, num =
        match num with
        | num when rem >= thresh -> numerals + nextNumeral, num - thresh
        | _ -> numerals, num
    (numerals, num)

let toRoman num =
    let rem = num
    let numerals = ""
    let next, rem = nextNumerals rem 1000 "M" 900 "CM"
    let numerals = numerals + next
    let next, rem = nextNumerals rem 500 "D" 400 "CD"
    let numerals = numerals + next
    let next, rem = nextNumerals rem 100 "C" 90 "XC"
    let numerals = numerals + next
    let next, rem = nextNumerals rem 50 "L" 40 "XL"
    let numerals = numerals + next
    let next, rem = nextNumerals rem 10 "X" 9 "IX"
    let numerals = numerals + next
    let next, rem = nextNumerals rem 5 "V" 4 "IV"
    let numerals = numerals + next
    let numerals = numerals + String.replicate rem "I"
    numerals