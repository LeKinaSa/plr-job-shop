
import collections
from ortools.sat.python import cp_model

from data import get_data
from output import IntermediateSolutionPrinter as SolutionPrinter, print_statistics, print_results
from constants import TASK_DURATION, TASK_MACHINE, DataDifficulty

def jobshop():
    # Get Data
    jobs = get_data(DataDifficulty.EASY)

    # Compute Horizon (worst case scenario)
    horizon = 0
    for job in jobs.values():
        for task in job:
            max_task_duration = max([alt_task[TASK_DURATION] for alt_task in task])
            horizon += max_task_duration

    # Create the Model
    model = cp_model.CpModel()

    # Global Storage of Variables
    intervals_per_resources = collections.defaultdict(list)
    starts    = {}  # indexed by (job_id, task_id)
    presences = {}  # indexed by (job_id, task_id, alt_id)
    job_ends  = []

    # Create Job Intervals (Decision Variables)
    for job_id, job in jobs.items():
        previous_end = None

        for task_id, task in enumerate(job):
            min_duration = min([alt_task[TASK_DURATION] for alt_task in task])
            max_duration = max([alt_task[TASK_DURATION] for alt_task in task])

            # Create Main Task Intervals for each Task
            start    = model.NewIntVar(0           , horizon     , f'start_j{job_id}_t{task_id}')
            duration = model.NewIntVar(min_duration, max_duration, f'duration_j{job_id}_t{task_id}')
            end      = model.NewIntVar(0           , horizon     , f'end_j{job_id}_t{task_id}')
            interval = model.NewIntervalVar( start, duration, end, f'interval_j{job_id}_t{task_id}')

            # Store the Start
            starts[(job_id, task_id)] = start

            # Constraints: Add Job Precedence
            if previous_end is not None:
                model.Add(start >= previous_end)
            previous_end = end

            # Create Alternative Intervals for the Task
            if len(task) > 1:
                l_presences = []
                for alt_id, alt_task in enumerate(task):
                    l_duration = alt_task[TASK_DURATION]
                    l_presence = model.NewBoolVar(            f'presence_j{job_id}_t{task_id}_a{alt_id}')
                    l_start    = model.NewIntVar (0, horizon, f'start_j{job_id}_t{task_id}_a{alt_id}')
                    l_end      = model.NewIntVar (0, horizon, f'end_j{job_id}_t{task_id}_a{alt_id}')
                    l_interval = model.NewOptionalIntervalVar(
                        l_start, l_duration, l_end, l_presence, f'interval_j{job_id}_t{task_id}_a{alt_id}')

                    l_presences.append(l_presence)

                    # Link the Main Variables with the Alternative Ones
                    model.Add(start    == l_start   ).OnlyEnforceIf(l_presence)
                    model.Add(duration == l_duration).OnlyEnforceIf(l_presence)
                    model.Add(end      == l_end     ).OnlyEnforceIf(l_presence)

                    # Add the Alternative Interval to the Correct Machine
                    intervals_per_resources[alt_task[TASK_MACHINE]].append(l_interval)

                    # Store the Presence
                    presences[(job_id, task_id, alt_id)] = l_presence

                # Only Select 1 Alternative per Task
                model.AddExactlyOne(l_presences)
            else:
                # Only 1 Possible Alternative
                alt_task = task[0]
                intervals_per_resources[alt_task[TASK_MACHINE]].append(interval)
                presences[(job_id, task_id, 0)] = model.NewConstant(1)

        # Store the End
        job_ends.append(previous_end)

    # Constraints: Two Jobs on the Same Machine can't overlap
    for intervals in intervals_per_resources.values():
        if len(intervals) > 1:
            model.AddNoOverlap(intervals)

    # Objective Function
    makespan = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(makespan, job_ends)
    model.Minimize(makespan)

    # Create the Solver and Solve
    solver = cp_model.CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.Solve(model, solution_printer)
    
    # Print Results
    print_statistics(solver, status)
    print_results(solver, status, jobs, starts, presences)

if __name__ == '__main__':
    jobshop()
