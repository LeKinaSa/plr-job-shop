
import collections
from ortools.sat.python import cp_model

from data import get_data
from output import print_solution

def main():
    # Get Data
    (models, productionLines, resources, resourcesNeededForModels, weeklyProduction) = get_data()

    # Compute horizon (worst case scenario)
    horizon = sum(model[1]['duration'] for model in models)

    # Create the Model
    model = cp_model.CpModel()
    task_type = collections.namedtuple('task_type', 'start end interval')

    # Create Job Intervals (Decision Variables)
    all_tasks = []
    machine_to_intervals = collections.defaultdict(list)
    for job_id, job in models:
        for task_id, task in enumerate(job):
            id = f'-{task_id}-model{job_id}'
            duration = task['duration']
            machine = None # TODO: machine = productionLine

            # Decision Variables
            start_var = model.NewIntVar(0, horizon, 'start' + id)
            end_var = model.NewIntVar(0, horizon, 'end' + id)
            interval_var = model.NewIntervalVar(start_var, duration, end_var, 'interval' + id)
            all_tasks[job_id, task_id] = task_type(start=start_var, end=end_var, interval=interval_var)
            machine_to_intervals[machine].append(interval_var)
    
    # Constraints
    # Two Jobs on the Same Machine can't overlap
    for machine in productionLines:
        model.AddNoOverlap(machine_to_intervals[machine])
    
    # Precedence inside a Job
    for job_id, job in models:
        for task_id in range(len(job) - 1):
            model.Add(all_tasks[job_id, task_id + 1].start >= all_tasks[job_id, task_id].end)
    
    # Objective Function
    obj_var = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(obj_var, [
        all_tasks[job_id, len(job - 1)].end
        for job_id, job in enumerate(models)
    ]) # TODO: check what this is doing
    model.Minimize(obj_var)

    # Create the Solver and Solve
    solver = cp_model.CpSolver()
    status = solver.Solve(model)

    # Solver Statistics
    print('\n Statistics')
    print('  - conflicts: %i' % solver.NumConflicts())
    print('  - branches : %i' % solver.NumBranches())
    print('  - wall time: %f s' % solver.WallTime())
    print()

    # Print the results
    if status == cp_model.OPTIMAL:
        print('Found a Solution')
        # Print Solution
        print_solution(solver, models, productionLines, all_tasks)
    else:
        print('No Solution Found')

if __name__ == '__main__':
    main()
