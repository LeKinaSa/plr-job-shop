
from ortools.sat.python.cp_model import CpModel, CpSolver

from data import get_data, get_overtime_intervals
from output import IntermediateSolutionPrinter as SolutionPrinter, print_statistics, print_results
from constants import TASK, MIN_START, MAX_END, START_VAR, END_VAR, DURATION_VAR, PRESENCES_VAR

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
        jobs[job][PRESENCES_VAR] = []
        for machine_id, min_duration in task:
            alt_start    = model.NewIntVar(0, horizon, '')
            alt_end      = model.NewIntVar(0, horizon, '')
            alt_duration = model.NewIntVar(min_duration, horizon, '')
            alt_present  = model.NewBoolVar('')

            model.Add(jobs[job][   START_VAR] == alt_start   ).OnlyEnforceIf(alt_present)
            model.Add(jobs[job][     END_VAR] == alt_end     ).OnlyEnforceIf(alt_present)
            model.Add(jobs[job][DURATION_VAR] == min_duration).OnlyEnforceIf(alt_present)

            jobs[job][PRESENCES_VAR].append(alt_present)
            
            # Task Duration
            alt_interval = model.NewOptionalIntervalVar(alt_start, alt_duration, alt_end, alt_present, '')

            # Machines Intervals for Constraint: Only 1 Task Per Machine at a Time
            intervals_per_machines[machine_id] = intervals_per_machines.get(machine_id, [])
            intervals_per_machines[machine_id].append(alt_interval)

        model.AddExactlyOne(jobs[job][PRESENCES_VAR])

    # Only 1 Task Per Machine at a Time
    for intervals in intervals_per_machines.values():
        model.AddNoOverlap(intervals)

    # Objective Function
    # overtime = get_overtime(model, horizon, jobs)
    (starts, ends) = ([], [])
    for info in jobs.values():
        ends  .append(info[  END_VAR])
        starts.append(info[START_VAR])
    # Total Time
    start_times = model.NewIntVar(0, horizon*(len(jobs)+1), '')
    end_times   = model.NewIntVar(0, horizon*(len(jobs)+1), '')
    total_time  = model.NewIntVar(0, horizon, 'total_time')
    model.Add(sum(ends) == end_times)
    model.Add(sum(starts) == start_times)
    model.Add(start_times + total_time == end_times)
    
    makespan = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(makespan, ends)
    model.Minimize(makespan)

    # Create Solver and Solve
    solver = CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.Solve(model, solution_printer)
    
    # Print Results
    print_statistics(solver, status)
    print_results(solver, status, jobs, makespan)
    print(solver.Value(start_times), solver.Value(end_times))

def get_overtime(model: CpModel, horizon: int, jobs: dict):
    overtime_intervals = get_overtime_intervals(horizon)
    used_overtimes = map(lambda x : get_used_overtime(model, horizon, jobs, x, overtime_intervals), jobs)
    overtime = model.NewIntVar(0, horizon, 'overtime')
    model.Add(sum(used_overtimes) == overtime)
    return overtime

def get_used_overtime(model: CpModel, horizon: int, jobs: dict, job: int, overtime_intervals: list):
    (start, end, duration) = (jobs[job][START_VAR], jobs[job][END_VAR], jobs[job][DURATION_VAR])
    time_stopped = model.NewIntVar(0, horizon, '')
    model.Add(time_stopped == end - start + duration)
    
    overlaps = get_overlaps(model, horizon, jobs, job, overtime_intervals)
    overtime = model.NewIntVar(0, horizon, '')
    model.Add(sum(overlaps) == overtime)
    
    return overtime - time_stopped

def get_overlaps(model: CpModel, horizon: int, jobs: dict, job: int, overtime_intervals: list):
    (task_start, task_end) = (jobs[job][START_VAR], jobs[job][END_VAR])
    overlaps = []
    for overtime_start, overtime_end in overtime_intervals:
        start = model.NewIntVar(0, horizon, '')
        end   = model.NewIntVar(0, horizon, '')
        model.AddMaxEquality(start, [task_start, overtime_start])
        model.AddMinEquality(end  , [task_end  , overtime_end  ])
        
        overlapped = model.NewIntVar(0, horizon, '')
        model.AddMaxEquality(overlapped, [end - start, 0])
        overlaps.append(overlapped)
    return overlaps

if __name__ == '__main__':
    jobshop()
