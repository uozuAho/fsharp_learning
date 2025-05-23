(*
Script test helper
*)

type TestFunc = unit -> bool * string

type Tests() =
    let mutable testFunctions = []

    member this.add name (func:TestFunc) =
        testFunctions <- (name, func) :: testFunctions

    member this.run =
        printfn "Running tests..."
        if testFunctions.IsEmpty then
            printfn "No tests to run."
        else
            testFunctions
            |> List.rev
            |> List.iter (fun x ->
                let name, func = x
                try
                    let passed, msg = func()
                    if passed then
                        printf "."
                        // printfn $"{name}: PASS"
                    else
                        printfn $"{name}: FAIL: {msg}"
                with ex ->
                    printfn $"{name}: ERROR - {ex.Message}"
            )
            printfn $"Ran {testFunctions.Length} tests"

let assertEq act exp =
    fun _ ->
        if act = exp then true, ""
        else false, $"Expected {exp}, got {act}"
