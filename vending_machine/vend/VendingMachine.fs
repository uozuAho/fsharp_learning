namespace vend

// Using the enterprise TTT pattern here:
// The domain describes what the vending machine can do, without
// coupling to the implementation.
module VendingMachineDomain =
    type Coin = Nickel | Dime | Quarter | Dollar
    type Create<'MachineState> = 'MachineState
    type InsertMoney<'MachineState> = 'MachineState -> Coin -> 'MachineState
    type CoinReturn<'MachineState> = 'MachineState -> 'MachineState * Coin list
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
        availableMoney: List<Coin>
        customerMoney: List<Coin>
    }

    let private insertMoney machineState money =
        let newMoney = machineState.customerMoney @ [money]
        { machineState with customerMoney = newMoney }

    let private coinReturn machineState =
        { machineState with customerMoney = [] }, machineState.customerMoney

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
