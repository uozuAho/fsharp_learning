namespace vend

// Using the enterprise TTT pattern here:
// The domain describes what the vending machine can do, without
// coupling to the implementation.
module VendingMachineDomain =
    // F# doesn't have literals with types, but I got this workaround from
    // https://github.com/fsharp/fslang-suggestions/issues/656
    type Coin =
        Nickel | Dime | Quarter | Dollar
        member t.Value =
            match t with
            | Nickel -> 0.05m
            | Dime -> 0.10m
            | Quarter -> 0.25m
            | Dollar -> 1.00m

    type Item =
        ItemA | ItemB | ItemC
        member t.Cost =
            match t with
            | ItemA -> 0.65m
            | ItemB -> 1.00m
            | ItemC -> 1.50m

    type Create<'MachineState> = 'MachineState
    type InsertMoney<'MachineState> = 'MachineState -> Coin -> 'MachineState
    type CoinReturn<'MachineState> = 'MachineState -> 'MachineState * Coin list
    type GetItem<'MachineState> = 'MachineState -> Item -> 'MachineState * Item * Coin list

    type Api<'MachineState> = {
        create: Create<'MachineState>
        insertMoney: InsertMoney<'MachineState>
        coinReturn: CoinReturn<'MachineState>
        getItem: GetItem<'MachineState>
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

    let private getItem machineState item =
        machineState, item, List.Empty

    let api = {
        create = create
        insertMoney = insertMoney
        coinReturn = coinReturn
        getItem = getItem
    }
