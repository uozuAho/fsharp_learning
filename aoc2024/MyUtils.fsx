open System

let readStdin =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)

// not sure if this is 100% correct. Getting weird behaviour with stdin
let isStdInFromPipe =
    Console.IsInputRedirected

let enumerate seq =
    seq |> Seq.mapi (fun i x -> (i, x))

let private getCallStack () =
    try
        let stackTrace = System.Diagnostics.StackTrace(true)
        stackTrace.GetFrames()
        |> Seq.takeWhile (fun x -> x.GetMethod().Name <> "InvokeMethod") // ignores everything after "my code"
    with
    | ex -> failwith $"Error retrieving call stack: {ex.Message}"

let private printCallStack (frames:seq<Diagnostics.StackFrame>) =
    for frame in frames do
        let method = frame.GetMethod()
        let fileName = frame.GetFileName()
        let lineNumber = frame.GetFileLineNumber()
        printfn "  Method: %s (File: %s, Line: %d)"
            (method.Name)
            (if isNull fileName then "Unknown" else fileName)
            (if lineNumber > 0 then lineNumber else -1)

let private getAssertFrame (frames:seq<Diagnostics.StackFrame>) =
    let rec find (frames:list<Diagnostics.StackFrame>) =
        match frames with
        | [] | [_] -> failwith "D'oh"
        | f1::f2::rest ->
            match f1.GetMethod().Name with
            | "wozAssert" -> f2
            | _ -> find (f2::rest)

    find (Seq.toList frames)

let private getAssertCode (frame:Diagnostics.StackFrame) =
    let file = frame.GetFileName()
    let lineNum = frame.GetFileLineNumber()
    let lineOfCode =
        System.IO.File.ReadLines file
        |> Seq.mapi (fun i str -> (i, str))
        |> Seq.where (fun idxStr ->
            let idx, _ = idxStr
            idx = lineNum - 1) // 1-indexed
        |> Seq.map snd
        |> Seq.exactlyOne
    lineOfCode.Trim()

// regular `assert` prints too much and crashes fsi
// Usage: dotnet fsi --define:DEBUG, otherwise it does nothing.
// Code line numbers are incorrect without DEBUG
let wozAssert assertion =
    #if DEBUG
    if not assertion then
        let stack = getCallStack()
        let frame = stack |> getAssertFrame
        let file = Array.last (frame.GetFileName().Split('/')) // just linux paths for now
        let lineNum = frame.GetFileLineNumber()
        let assertCode = frame |> getAssertCode
        printfn $"{file}:{lineNum}: Assertion failed: {assertCode}"
        // printCallStack stack
    #endif
    ()
