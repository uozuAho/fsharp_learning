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
