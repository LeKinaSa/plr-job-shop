# PLR-project
PLR Project - Factory Line Scheduling

## Factory Line Scheduling
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

## Group
| Group Member | Up Number |
| --- | --- |
| Clara Alves Martins | up201806528 |
| João António Cardoso Vieira e Basto de Sousa | up201806613 |
