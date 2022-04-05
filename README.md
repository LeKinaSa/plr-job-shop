# PLR-project

PLR Project - Factory Line Scheduling

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


## Group

| Group Member | Up Number |
| --- | --- |
| Clara Alves Martins | up201806528 |
| João António Cardoso Vieira e Basto de Sousa | up201806613 |
