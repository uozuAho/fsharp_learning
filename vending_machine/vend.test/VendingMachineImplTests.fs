module Tests

open Xunit
open vend
open vend.VendingMachineDomain

[<Fact>]
let ``Get same coin back`` () =
    let api = VendingMachineImpl.api
    let machine = api.create
    let machine = api.insertMoney machine Dollar
    let machine, coins = api.coinReturn machine
    Assert.Equal(1, coins.Length)
    Assert.Equal(Dollar, coins.Head)
