
from ortools.sat.python.cp_model import CpModel, CpSolver, IntVar

from data import get_data
from output import IntermediateSolutionPrinter as SolutionPrinter, print_statistics, print_results, print_value
from constants import TASK, MIN_START, MAX_END, START_VAR, END_VAR, DURATION_VAR, PRESENCES_VAR

NORMAL_TIME = 1920
OVER_TIME   = 384
WORK_WEEK   = NORMAL_TIME + OVER_TIME

def jobshop(filename: str = 'data/fab.json', log: bool = True) -> tuple:
    (jobs, horizon) = get_data(filename)

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
    (starts, ends) = ([], [])
    for info in jobs.values():
        ends  .append(info[  END_VAR])
        starts.append(info[START_VAR])
    # Total Time
    max_total_time = horizon * (len(jobs) + 1)
    start_times = model.NewIntVar(0, max_total_time, '')
    end_times   = model.NewIntVar(0, max_total_time, '')
    total_time  = model.NewIntVar(0, max_total_time, 'total_time')
    model.Add(sum(ends) == end_times)
    model.Add(sum(starts) == start_times)
    model.Add(start_times + total_time == end_times)
    
    makespan = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(makespan, ends)
    # model.Minimize(makespan)

    overtime = get_overtime(model, jobs, horizon, max_total_time)
    model.Minimize(overtime)

    # Create Solver and Solve
    solver = CpSolver()
    solution_printer = SolutionPrinter() if log else None
    status = solver.Solve(model, solution_printer)
    
    # Print Results
    if log:
        print_statistics(solver, status)
        print_results(solver, status, jobs, makespan)
        print_value(solver, status, overtime)
    else:
        print(solver.StatusName(status))
    return (solver, status)

def get_overtime(model: CpModel, jobs: dict, horizon: int, max_total_time: dict) -> IntVar:
    max_total_time += 2 * WORK_WEEK
    used_overtimes = []
    for job in jobs:
        start    = jobs[job][START_VAR]
        end      = jobs[job][  END_VAR]
        duration = jobs[job][DURATION_VAR]
        
        # Unused Overtime
        unused_overtime = model.NewIntVar(0, max_total_time, '')
        model.Add(unused_overtime == end - start - duration)
        
        # Total Overtime Calculation
        total_overtime = model.NewIntVar(0, max_total_time, '')
        start_mod = model.NewIntVar(0, WORK_WEEK, '')
        end_mod   = model.NewIntVar(0, WORK_WEEK, '')
        model.AddModuloEquality(start_mod, start, WORK_WEEK)
        model.AddModuloEquality(  end_mod,   end, WORK_WEEK)
        align_start = model.NewIntVar(0, horizon, '')
        align_end   = model.NewIntVar(0, horizon + WORK_WEEK, '')
        model.Add(align_start == start - start_mod)
        model.Add(align_end   ==  end  -   end_mod + WORK_WEEK)
        
        # Overtime found in weeks between the start and end of the task
        overtime_middle = model.NewIntVar(0, max_total_time, '')
        minutes_weeks = model.NewIntVar(0, max_total_time, '')
        n_weeks = model.NewIntVar(0, max_total_time // WORK_WEEK + 1, '')
        model.Add(align_end - align_start == minutes_weeks)
        model.AddDivisionEquality(n_weeks, minutes_weeks, WORK_WEEK)
        model.AddMultiplicationEquality(overtime_middle, n_weeks, OVER_TIME)
        
        # Overtime not used on task start
        overtime_start  = model.NewIntVar(0, OVER_TIME, '')
        model.AddMaxEquality(overtime_start, [start_mod - NORMAL_TIME, 0])
        
        # Overtime not used on task end
        overtime_end    = model.NewIntVar(0, OVER_TIME, '')
        model.AddMinEquality(overtime_end, [WORK_WEEK - end_mod, OVER_TIME])
        
        # Total Overtime
        model.Add(total_overtime == overtime_middle - overtime_end - overtime_start)
        
        # Used Overtime
        used_overtime = model.NewIntVar(0, max_total_time, '')
        model.Add(used_overtime + unused_overtime == total_overtime)
        used_overtimes.append(used_overtime)
    
    overtime = model.NewIntVar(0, max_total_time, 'overtime')
    model.Add(sum(used_overtimes) == overtime)
    return overtime

if __name__ == '__main__':
    jobshop()
