# Vending machine kata

https://code.joejag.com/coding-dojo/vending-machine/

The goals for this implementation:
- hide implementation details from the user (user = tests)

I followed the 'Enterprise TTT' approach.

# todo
- WIP: property test for buying item with exact change
    - need to know coin value
    - need to know item value
- get item:
    - DONE: get item with exact change
    - add items b & c
        - item a = 0.65
        - item b = 1.00
        - item c = 1.50
    - if not enough money, do nothing
    - if no items left, do nothing
    - if not enough stored coins to give change, return money
    - if press item first then enter money, give item + change
      when enough money is entered
- does impl need an API? can it just expose functions that
  users need to use?
