# Vending machine kata

https://code.joejag.com/coding-dojo/vending-machine/

The goals for this implementation:
- hide implementation details from the user (user = tests)

I followed the 'Enterprise TTT' approach.

# todo
- get item: returns an item + change
    - if not enough money, do nothing
    - if no items left, do nothing
    - if not enough stored coins to give change, return money
    - if press item first then enter money, give item + change
      when enough money is entered
- does impl need an API? can it just expose functions that
  users need to use?
