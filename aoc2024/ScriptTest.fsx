(*
Script test helper
*)

type Tests() =
    let mutable testFunctions = []

    member this.add name func =
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
                    let passed = func()
                    if passed then
                        printf "."
                        // printfn $"{name}: PASS"
                    else
                        printfn $"{name}: FAIL"
                with ex ->
                    printfn $"{name}: ERROR - {ex.Message}"
            )
            printfn "DONE"
