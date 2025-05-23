open System
open System.Diagnostics

let private getCallStack () =
    try
        let stackTrace = StackTrace(true)
        stackTrace.GetFrames()
        |> Seq.takeWhile (fun x -> x.GetMethod().Name <> "InvokeMethod") // ignores everything after "my code"
    with
    | ex -> failwith $"Error retrieving call stack: {ex.Message}"

let private printCallStack (frames:seq<StackFrame>) =
    for frame in frames do
        let method = frame.GetMethod()
        let fileName = frame.GetFileName()
        let lineNumber = frame.GetFileLineNumber()
        printfn "  Method: %s (File: %s, Line: %d)"
            (method.Name)
            (if isNull fileName then "Unknown" else fileName)
            (if lineNumber > 0 then lineNumber else -1)

let private getAssertFrame (frames:seq<StackFrame>) =
    let rec find (frames:list<StackFrame>) =
        match frames with
        | [] | [_] -> failwith "D'oh"
        | f1::f2::rest ->
            match f1.GetMethod().Name with
            | "wozAssert" -> f2
            | _ -> find (f2::rest)

    find (Seq.toList frames)

let private getAssertCode (frame:StackFrame) =
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
// without DEBUG, code line numbers are incorrect
let wozAssert assertion =
    #if DEBUG
    if not assertion then
        let stack = getCallStack()
        let frame = stack |> getAssertFrame
        let file = Array.last (frame.GetFileName().Split('/')) // just linux paths for now
        let lineNum = frame.GetFileLineNumber()
        let assertCode = frame |> getAssertCode
        let assertionText = assertCode.Replace("wozAssert", "")
        let temp = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Red
        printfn $"{file}:{lineNum}: ASSERTION FAILED: {assertionText}"
        Console.ForegroundColor <- temp
        // failwith ""  // uncomment to break on failed assertion
        // printCallStack stack
    #endif
    ()
