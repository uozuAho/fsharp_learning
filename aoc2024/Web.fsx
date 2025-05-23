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
