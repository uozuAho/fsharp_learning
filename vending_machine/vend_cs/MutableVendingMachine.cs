using System.Collections.Immutable;

namespace vend_cs;

public abstract class Coin { public abstract decimal Value { get; } }
public sealed class Nickel : Coin { public override decimal Value => 0.05m; }
public sealed class Dime : Coin { public override decimal Value => 0.05m; }
public sealed class Quarter : Coin { public override decimal Value => 0.05m; }
public sealed class Dollar : Coin { public override decimal Value => 0.05m; }

public abstract class Item { public abstract decimal Cost { get; } }
public sealed class ItemA : Item { public override decimal Cost => 0.65m; }
public sealed class ItemB : Item { public override decimal Cost => 1.00m; }
public sealed class ItemC : Item { public override decimal Cost => 1.50m; }

public interface IVendingMachine
{
    void InsertCoin(Coin coin);
    void InsertCoins(IEnumerable<Coin> coins);
    IEnumerable<Coin> CoinReturn();
    (Item? item, IEnumerable<Coin> change) GetItem(Item item);
}

public class MutableVendingMachine : IVendingMachine
{
    private List<Coin> _insertedCoins = new List<Coin>();

    public void InsertCoin(Coin coin)
    {
        _insertedCoins.Add(coin);
    }

    public void InsertCoins(IEnumerable<Coin> coins)
    {
        _insertedCoins.AddRange(coins);
    }

    public IEnumerable<Coin> CoinReturn()
    {
        var coins = _insertedCoins.ToArray();
        _insertedCoins.Clear();
        return coins;
    }

    public (Item? item, IEnumerable<Coin> change) GetItem(Item item)
    {
        return (item, Enumerable.Empty<Coin>());
    }
}

public interface IImmutableVendingMachine
{
    IImmutableVendingMachine InsertCoin(Coin coin);
    IImmutableVendingMachine InsertCoins(IEnumerable<Coin> coins);
    (IImmutableVendingMachine, IEnumerable<Coin>) CoinReturn();

    (IImmutableVendingMachine, Item? item, IEnumerable<Coin> change)
        GetItem(Item item);
}

public record ImmutableVendingMachine : IImmutableVendingMachine
{
    private ImmutableList<Coin> _insertedCoins = ImmutableList.Create<Coin>();

    public IImmutableVendingMachine InsertCoin(Coin coin)
    {
        return this with { _insertedCoins = _insertedCoins.Add(coin) };
    }

    public IImmutableVendingMachine InsertCoins(IEnumerable<Coin> coins)
    {
        return this with { _insertedCoins = _insertedCoins.AddRange(coins) };
    }

    public (IImmutableVendingMachine, IEnumerable<Coin>) CoinReturn()
    {
        return (
            this with { _insertedCoins = _insertedCoins.Clear() },
            _insertedCoins
        );
    }

    public (IImmutableVendingMachine,
        Item? item,
        IEnumerable<Coin> change
    )
        GetItem(Item item)
    {
        return (this, item, _insertedCoins);
    }
}
