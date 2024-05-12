namespace vend

module VendingMachineDomain =
    type Money = Nickel | Dime | Quarter | Dollar
    type InsertMoney = Money -> Unit
    type Api = {
        insertMoney: InsertMoney
        coinReturn: List<Money>
    }


module VendingMachineImpl =
    open VendingMachineDomain
    let private insertMoney money = ()
    let private coinReturn = [Nickel]
    let api = {
        insertMoney = insertMoney
        coinReturn = coinReturn
    }
