open System

let readStdin =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)
