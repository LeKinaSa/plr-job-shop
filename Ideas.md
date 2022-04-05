# Decisions

## What is a job?

1. All the items of a product
    - Problems
        - may cause more overtime than needed
2. One item at a time
    - Problems
        - there is a penalty for changing product

## How to represent time?

1. Weekly normal time, followed by weekly overtime
2. Weekly normal time (without overtime)

## What is the objective function?

- minimize overtime
- minimize difference of ending time between machines
  - it is not supposed to fill empty space in the middle
  - miniize difference in normal time between machines

## What are the constraints?

- each item
  - must be produced when its resources have arrived
  - should be produced before its delivery date
    - if this is impossible, it should minimize the delay
- only one item in each machine at a time

----

### Pre-basic
- 1 Product (time needed per unit)
- 1 Production Line (uptime minutes per day)
- Time period: 1 day.

### Basic
- Products and 2 Production Lines.