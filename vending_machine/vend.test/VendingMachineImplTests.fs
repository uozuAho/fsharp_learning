module Tests

open Xunit
open vend
open vend.VendingMachineDomain

[<Fact>]
let ``Get same coin back`` () =
    let machine = VendingMachineImpl.api
    machine.insertMoney Dollar
    let coins = machine.coinReturn
    Assert.Equal(1, coins.Length)
    Assert.Equal(Dollar, coins.Head)
