# Vending machine kata

https://code.joejag.com/coding-dojo/vending-machine/

The goals for this implementation:
- hide implementation details from the user (user = tests)

I followed the 'Enterprise TTT' approach.

# C# version
I made a C# version in vend_cs. C# _can_ do most of what F# is doing, but it's
more verbose and harder to read. Type inference is great!

# todo
- get item:
    - if not enough money, do nothing
    - give item with change when not exact change given
    - if no items left, do nothing
    - if not enough stored coins to give change, return money
- does impl need an API? can it just expose functions that
  users need to use?
