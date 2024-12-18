open System

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
        let assertionText = assertCode.Replace("wozAssert", "")
        let temp = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Red
        printfn $"{file}:{lineNum}: ASSERTION FAILED: {assertionText}"
        Console.ForegroundColor <- temp
        // printCallStack stack
    #endif
    ()

let readStdin =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)

// I think this will only work in fs projects, not scripts
let isStdInFromPipe =
    Console.IsInputRedirected

let enumerate seq =
    seq |> Seq.mapi (fun i x -> (i, x))

// saving keystrokes:
let map fn = Seq.map fn
let el n (list:list<'a>) = list.[n]
let sort seq = Seq.sort seq
let len seq = Seq.length seq

let webFetch (url:string) =
    async {
        use client = new System.Net.Http.HttpClient()
        let! resp = client.GetStringAsync(url) |> Async.AwaitTask
        return resp
    }

// todo: add cookie with session token for this to work
let aocFetchInput year day =
    let input =
        webFetch $"https://adventofcode.com/{year}/day/{day}/input"
        |> Async.RunSynchronously
    System.IO.File.WriteAllText($"input/{day}.txt", input)
    input

let toLines (str:string) =
    str.Split('\n') |> Array.where ((<>) "")

let reMatches pattern str =
    let regex = System.Text.RegularExpressions.Regex(pattern)
    [ for matchObj in regex.Matches(str) -> matchObj.Value ]

let numbersInLine str =
    reMatches "\d+" str |> List.map int64

let linesAsNumbers lines =
    lines |> map numbersInLine
