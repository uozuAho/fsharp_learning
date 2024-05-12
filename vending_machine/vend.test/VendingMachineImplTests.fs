module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open vend
open vend.VendingMachineDomain

let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return List.item i xs }

[<Property>]
let ``Get same coin back`` () =
    let n = chooseFromList [Nickel; Dime; Quarter; Dollar] |> Arb.fromGen
    Prop.forAll n (fun coin ->
        let api = VendingMachineImpl.api
        let machine = api.create
        let machine = api.insertMoney machine coin
        let machine, coins = api.coinReturn machine
        Assert.Equal(1, coins.Length)
        Assert.Equal(coin, coins.Head)
    )
