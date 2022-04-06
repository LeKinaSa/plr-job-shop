
import collections
from ortools.sat.python import cp_model

from data import get_data
from output import print_solution

def main():
    PRODUCTION_TIME = 'production_time'
    PRODUCTION_LINE = 'production_line'
    CAPACITY        = 'capacity'
    TOTAL           = 'total'

    # Get Data
    (models, lines) = get_data(PRODUCTION_TIME, PRODUCTION_LINE, CAPACITY, TOTAL)

    # Compute horizon (worst case scenario)
    horizon = sum(model[PRODUCTION_TIME]*model[TOTAL] for model in models.values())

    # Create the Model
    model = cp_model.CpModel()
    task_type = collections.namedtuple('task_type', 'start end interval')

    # Create Job Intervals (Decision Variables)
    all_tasks = {}
    machine_to_intervals = collections.defaultdict(list)
    for product in models:
        id = f'-{product}'
        duration = models[product][PRODUCTION_TIME]
        machine = 1 # TODO: machine = productionLine
        
        # Decision Variables
        start_var = model.NewIntVar(0, horizon, 'start' + id)
        end_var = model.NewIntVar(0, horizon, 'end' + id)
        interval_var = model.NewIntervalVar(start_var, duration, end_var, 'interval' + id)
        all_tasks[product] = task_type(start=start_var, end=end_var, interval=interval_var)
        machine_to_intervals[machine].append(interval_var)
    
    # Constraints
    # Two Jobs on the Same Machine can't overlap
    for machine in lines:
        model.AddNoOverlap(machine_to_intervals[machine])
    
    # # Precedence inside a Job
    # for job_id, job in models:
    #     for task_id in range(len(job) - 1):
    #         model.Add(all_tasks[job_id, task_id + 1].start >= all_tasks[job_id, task_id].end)
    
    # Objective Function
    obj_var = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(obj_var, [
        all_tasks[product].end for product in models
        # all_tasks[job_id, len(job - 1)].end
        # for job_id, job in enumerate(models)
    ]) # TODO: check what this is doing
    model.Minimize(obj_var)

    # Create the Solver and Solve
    solver = cp_model.CpSolver()
    status = solver.Solve(model)

    # Solver Statistics
    print('------------------------')
    print('       Statistics       ')
    print('------------------------')
    print(f'  - conflicts: {solver.NumConflicts()}')
    print(f'  - branches : {solver.NumBranches()}')
    print(f'  - wall time: {solver.WallTime()} s')
    print('------------------------')

    # Print the results
    if status == cp_model.OPTIMAL:
        print('Found a Solution')
        # Print Solution
        print_solution(solver, models, lines, all_tasks)
    else:
        print('No Solution Found')

if __name__ == '__main__':
    main()
