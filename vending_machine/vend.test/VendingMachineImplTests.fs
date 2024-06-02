module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open vend
open vend.VendingMachineDomain

let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return List.item i xs }

[<Fact>]
let ``Coin return returns nothing when no coins entered`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let machine, coins = api.coinReturn machine
    Assert.Empty coins

[<Property>]
let ``Coin return: Get same coin back`` () =
    let n = chooseFromList [Nickel; Dime; Quarter; Dollar] |> Arb.fromGen
    Prop.forAll n (fun coin ->
        let api = VendingMachineImpl.api
        let machine = api.create
        let machine = api.insertMoney machine coin
        let machine, coins = api.coinReturn machine
        Assert.Equal(1, coins.Length)
        Assert.Equal(coin, coins.Head)
    )

[<Property>]
let ``Coin return: Get two coins back`` () =
    let n = chooseFromList [Nickel; Dime; Quarter; Dollar] |> Gen.two |> Arb.fromGen
    Prop.forAll n (fun (coin1, coin2) ->
        let api = VendingMachineImpl.api
        let machine = api.create
        let machine = api.insertMoney machine coin1
        let machine = api.insertMoney machine coin2
        let machine, coins = api.coinReturn machine
        Assert.Equal(2, coins.Length)
        let expectedCoins = [coin1; coin2]
        Assert.Equivalent(expectedCoins, coins)
    )

[<Fact>]
let ``Coin return returns nothing after coins returned`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let machine = api.insertMoney machine Dollar
    let machine, coins = api.coinReturn machine
    let machine, coins = api.coinReturn machine
    Assert.Empty coins

[<Fact>]
let ``buy item with exact change`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let machine = api.insertMoney machine Dollar
    let machine, item, change = api.getItem machine ItemA
    Assert.Equal(ItemA, item)
    Assert.Empty change

[<Property>]
let ``buy item with exact change prop`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let item = chooseFromList [ItemA; ItemB; ItemC]

    // choose item to buy
    // insert exact change
    // press item
    // assert got item and no change

// WIP: my version based on gpt example below
let makeCorrectChange (coins: seq<Coin>) (amount: decimal) =
    let rec pick remainingCoins remainingAmount picked =
        match remainingCoins with
        | [] -> if remainingAmount = 0 then Some(picked) else None
        | coin::rest ->
            if coin.Value <= remainingAmount then
                // todo: need to know coin value
                match pick (remainingCoins) (remainingAmount - coin.Value) (coin :: picked) with
                | Some(result) -> Some(result)
                | None -> pick (rest) (remainingAmount) (picked)
            else
                pick (rest) (remainingAmount) (picked)

    pick coins amount []


// example from chatGPT
let makeChange (coins: list<decimal>) (amount: decimal) : list<decimal> =
    let rec helper (sortedCoins: list<decimal>) (remaining: decimal) (acc: list<decimal>) =
        match sortedCoins with
        | [] -> if remaining = 0M then Some(acc) else None
        | coin::rest ->
            if coin <= remaining then
                match helper (sortedCoins) (remaining - coin) (coin :: acc) with
                | Some(result) -> Some(result)
                | None -> helper (rest) (remaining) (acc)
            else
                helper (rest) (remaining) (acc)
    
    let sortedCoins = List.sortDescending coins
    match helper sortedCoins amount [] with
    | Some(result) -> result
    | None -> failwith "No solution found"
