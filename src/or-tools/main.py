
from ortools.sat.python.cp_model import CpModel, CpSolver, OPTIMAL, FEASIBLE

from data import get_data
from output import IntermediateSolutionPrinter as SolutionPrinter, print_statistics, print_results
from constants import TASK, MIN_START, MAX_END

START_VAR     = 'start'
END_VAR       = 'end'
DURATION_VAR  = 'duration'

def jobshop():
    (jobs, horizon) = get_data()

    # Create Model
    model = CpModel()

    # Decision Variables
    for job in jobs:
        jobs[job][   START_VAR] = model.NewIntVar(0, horizon,    f'start_{job}')
        jobs[job][     END_VAR] = model.NewIntVar(0, horizon,      f'end_{job}')
        jobs[job][DURATION_VAR] = model.NewIntVar(0, horizon, f'duration_{job}')

    # Task Interval
    for job in jobs:
        model.Add(jobs[job][START_VAR] >= jobs[job][MIN_START])
        model.Add(jobs[job][  END_VAR] >= jobs[job][MIN_START])
        model.Add(jobs[job][START_VAR] <= jobs[job][MAX_END  ])
        model.Add(jobs[job][  END_VAR] <= jobs[job][MAX_END  ])

    # Only 1 Chosen Alternative Task
    intervals_per_machines = {}
    for job, info in jobs.items():
        task = info[TASK]
        presences = []
        for machine_id, alt_duration in task:
            alt_start = model.NewIntVar(0, horizon, '')
            alt_end   = model.NewIntVar(0, horizon, '')
            alt_present = model.NewBoolVar('')

            model.Add(jobs[job][   START_VAR] == alt_start   ).OnlyEnforceIf(alt_present)
            model.Add(jobs[job][     END_VAR] == alt_end     ).OnlyEnforceIf(alt_present)
            model.Add(jobs[job][DURATION_VAR] == alt_duration).OnlyEnforceIf(alt_present)

            presences.append(alt_present)
            
            # Task Duration
            alt_interval = model.NewOptionalIntervalVar(alt_start, alt_duration, alt_end, alt_present, '')

            # Machines Intervals for Constraint: Only 1 Task Per Machine at a Time
            intervals_per_machines[machine_id] = intervals_per_machines.get(machine_id, [])
            intervals_per_machines[machine_id].append(alt_interval)

        model.AddExactlyOne(presences)

    # Only 1 Task Per Machine at a Time
    for intervals in intervals_per_machines.values():
        model.AddNoOverlap(intervals)

    # Objective Function
    ends = []
    for info in jobs.values():
        ends.append(info[END_VAR])
    makespan = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(makespan, ends)
    model.Minimize(makespan)

    # Create Solver and Solve
    solver = CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.Solve(model, solution_printer)
    
    # Print Results
    print_statistics(solver, status)
    #print_results(solver, status, jobs, starts, presences)

if __name__ == '__main__':
    jobshop()
