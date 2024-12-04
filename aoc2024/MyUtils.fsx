open System

let readStdin =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)

// not sure if this is 100% correct, but it works
let isStdInFromPipe =
    Console.IsInputRedirected
