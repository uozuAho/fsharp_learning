open System

let readStdin =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)

// not sure if this is 100% correct. Getting weird behaviour with stdin
let isStdInFromPipe =
    Console.IsInputRedirected

let enumerate seq =
    seq |> Seq.mapi (fun i x -> (i, x))
