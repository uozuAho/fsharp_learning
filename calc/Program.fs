// Stack is a list of floats
// Instead of `type Stack = float list`,
// define a single case union type. This ensures
// that the stack type is unique from other
// 'float list' types.
type Stack = StackContents of float list

// Push to a stack.
// On the order of parameters: the most 'changeable'
// parameter should come last. See https://fsharpforfunandprofit.com/posts/partial-application
let push x (StackContents contents) =
    StackContents (x::contents)

// Using partial application, we can define numbers as
// functions that push to a stack.
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0
let SIX = push 6.0
let SEVEN = push 7.0
let EIGHT = push 8.0
let NINE = push 9.0
let EMPTY = StackContents []

let pop (StackContents contents) =
    match contents with
    | top::rest ->
        let newStack = StackContents rest
        (top,newStack)
    | [] ->
        // for now, exception
        // error cases are preferable, as unhandled
        // errors won't compile
        failwith "Stack underflow"
   
let binary mathFn stack =
    let y,stack' = pop stack
    let x,stack'' = pop stack'
    let z = mathFn x y
    push z stack''
    
let ADD = binary (+)
let MUL = binary (*)
let SUB = binary (-)
let DIV = binary (/)

let unary f stack =
    let x,stack' = pop stack
    push (f x) stack'
    
let NEG = unary (fun x -> -x)
let SQUARE = unary (fun x -> x * x)

let SHOW stack =
    let x,_ = pop stack
    printfn $"The answer is %f{x}"
    stack
    
let START = EMPTY

// composition vs piping:
// composition creates new functions from the given functions
// piping just chains function calls

let ADDDBL = ADD >> ADD