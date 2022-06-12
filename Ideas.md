# PLR-Project

## Factory Line Scheduling (Job-Shop with Load Balancing)

In a factory with multiple production lines and products,
some products can only be produced in one of the lines
while others can be produced by multiple lines.
Each product takes a different time to complete.

Weekly production is limited but overtime may be allocated as needed.
Resources arrival and product expedition dates may also be taken into consideration.

### Objective

- Minimize the difference between the end of production of all the production lines
- Minimize the overtime needed

### Information

- model (id, timeToProduce, productionLines(1/2/both), limitDeliveryWeek)
- productionLine (id, availableTimePerWeek, n_employees)
- resource(id, size, weekUntilOfArrival)
  - existing resources : week 0
- resourceNeededForModel(modelId, resourceId, amount)

- weeklyProduction(week, [(modelId, lineId, amount), ...], [(lineId, extraHours), ...])

## Input (Data)

- Excel with information regarding:
  - Model information: time and resources' amounts needed and production lines where it can be produced; number of units to be produced.
  - Production Lines: number of hours of uptime per week and number of employees
  - (Resources: stock and time of arrival)

## Output (Results)

- Optimal Schedule
- Delay per model (>=0)
- Resources Used / Remaining
- Number of overtime hours (>= 0)

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

## Presentation

### Next Steps

- overtime (modulation and possible solutions)
- possible alterations to implemented constraints

### Complexity Analysis

- Which input factor affects more / less the time complexity?
- What are the best proportions for Jobs / Tasks / AltTasks / Machines ?

### Analysis on 3 Axis

- method / strategy for variable selection
  - how to choose the next variable?
  - strategy used to choose the next variable
- method / strategy for value selection
  - how to choose the value for that variable?
  - strategy used to choose the value for the chosen variable
- dimensions of the problem
  - jobs
  - tasks
  - alt tasks
  - machines
  - percentage of alt tasks per machine
  - times(?)

### Topics

- input
- output
- constraints
- modelation
- demo
