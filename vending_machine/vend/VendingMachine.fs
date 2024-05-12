namespace vend

// Using the enterprise TTT pattern here:
// The domain describes what the vending machine can do, without
// coupling to the implementation.
module VendingMachineDomain =
    type Money = Nickel | Dime | Quarter | Dollar
    type InsertMoney<'MachineState> = 'MachineState -> Money -> 'MachineState
    type CoinReturn<'MachineState> = 'MachineState -> 'MachineState * Money list
    type Create<'MachineState> = 'MachineState
    type Api<'MachineState> = {
        create: Create<'MachineState>
        insertMoney: InsertMoney<'MachineState>
        coinReturn: CoinReturn<'MachineState>
    }


module VendingMachineImpl =
    open VendingMachineDomain

    // For now, the game state is visible to the Impl user.
    // In Enterprise TTT, the console app has access to the
    // game state, but it just passes the api to the console
    // UI, which only has API access. I don't want to repeat
    // that boilerplate here.
    type MachineState = {
        availableMoney: List<Money>
        customerMoney: List<Money>
    }

    let private insertMoney machineState money =
        let newMoney = machineState.customerMoney @ [money]
        { machineState with customerMoney = newMoney }

    let private coinReturn machineState =
        machineState, [Dollar]

    let private create =
        {
            availableMoney = List.Empty
            customerMoney = List.Empty
        }

    let api = {
        create = create
        insertMoney = insertMoney
        coinReturn = coinReturn
    }
