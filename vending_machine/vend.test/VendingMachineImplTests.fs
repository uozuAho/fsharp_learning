module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open vend
open vend.VendingMachineDomain

let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return List.item i xs }

// Just picks coins in the order they're given, so will fail
// to find correct change for certain coin orders.
let makeCorrectAmount coins amount =
    let rec pick (remainingCoins: Coin list) (remainingAmount: decimal) (picked: Coin list) =
        match remainingCoins with
        | [] -> if remainingAmount = 0m then Some(picked) else None
        | coin::rest ->
            if coin.Value <= remainingAmount then
                match pick (remainingCoins) (remainingAmount - coin.Value) (coin :: picked) with
                | Some(result) -> Some(result)
                | None -> pick (rest) (remainingAmount) (picked)
            else
                pick (rest) (remainingAmount) (picked)

    pick coins amount []

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
        let machine = api.insertCoin machine coin
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
        let machine = api.insertCoin machine coin1
        let machine = api.insertCoin machine coin2
        let machine, coins = api.coinReturn machine
        Assert.Equal(2, coins.Length)
        let expectedCoins = [coin1; coin2]
        Assert.Equivalent(expectedCoins, coins)
    )

[<Fact>]
let ``Coin return returns nothing after coins returned`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let machine = api.insertCoin machine Dollar
    let machine, coins = api.coinReturn machine
    let machine, coins = api.coinReturn machine
    Assert.Empty coins

[<Property>]
let ``buy item with exact change prop`` () =
    let api = VendingMachineImpl.api
    let item = chooseFromList [ItemA; ItemB; ItemC] |> Arb.fromGen
    let myMoney = List.replicate 50 Nickel

    Prop.forAll item (fun toBuy ->
        let machine = api.create
        let correctChange = makeCorrectAmount myMoney toBuy.Cost
        let toInsert =
            match correctChange with
            | Some(result) -> result
            | None -> failwith "couldn't make correct change"
        let machine = api.insertCoins machine toInsert

        let machine, gotItem, gotChange = api.getItem machine toBuy 

        Assert.Equal(toBuy, gotItem)
        Assert.Empty gotChange
    )
