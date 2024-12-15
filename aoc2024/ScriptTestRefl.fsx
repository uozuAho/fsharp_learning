(*
An attempt at a lightweight test framework for F# scripts. Doesn't work
as it can't find functions with the test attribute.
*)

open System
open System.Reflection

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type TestAttribute() = inherit Attribute()

// let findTests1 =
//     let assembly = Assembly.GetExecutingAssembly()
//     printfn "%A" assembly

//     assembly.GetTypes()
//     |> Array.collect (fun t ->
//         printfn "%A" t
//         t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
//         |> Array.where (fun m ->
//             printfn "  %A" m
//             for a in m.GetCustomAttributes() do
//                 printfn $"    {a}"
//             let attr = m.GetCustomAttribute<TestAttribute>() :> obj
//             attr <> null)
//     )

let findTests2 =
    let assembly = Assembly.GetExecutingAssembly()
    printfn "%A" assembly

    assembly.GetTypes()
    |> Array.collect (fun t -> t.GetMethods())
    |> Array.choose (fun m ->
        m.CustomAttributes
        |> Seq.tryFind (fun a -> a.AttributeType = typeof<TestAttribute>)
        |> Option.map (fun _ -> m)
    )

let runTests =
    let testMethods = findTests2

    if testMethods.Length = 0 then
        printfn "No tests found in the assembly."
    else
        for method in testMethods do
            printfn "Running: %s.%s" method.DeclaringType.FullName method.Name
            try
                let result = method.Invoke(null, [||])
                match result with
                | :? bool as passed when passed -> printfn "Result: PASS"
                | :? bool -> printfn "Result: FAIL"
                | _ -> printfn "Result: Unknown"
            with ex ->
                printfn "Test failed with exception: %s" ex.Message

module MyTests =
    [<Test>]
    let ``one is one`` = 1 = 1

runTests
