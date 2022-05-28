
from ortools.sat.python.cp_model import CpModel, CpSolver, OPTIMAL, FEASIBLE
from math import floor
from copy import deepcopy
from random import Random

from prepare import save_data

TASK_MACHINE  = 0
TASK_DURATION = 1
MIN_START = 'min_start'
MAX_END   = 'max_end'
TASK      = 'task'
START_VAR     = 'start'
END_VAR       = 'end'
DURATION_VAR  = 'duration'
PRESENCES_VAR = 'intervals'

# Data Variables
#  - Number of Jobs (default: 74)
#  - Number of Alternatives per Job (default: 2)
#  - (Percentage of) Jobs with Alternatives (default: 6)
#  - (Ratio between Number of Alternatives per Jobs and) Number of Machines (default: 2)
#  - Ratio between Normal Time and Over Time (default: 1920-384)
#  - Horizon (default: [58547, 62587] ~845*n_jobs)
#  - Job Durations (default: [12, 8036])

random_seed = 0

def generator():
    global random_seed
    normal_time_hours = 40
    for n_jobs in [5, 10, 20, 40, 60, 80, 100, 125, 150]:
        for percent_alt_jobs in range(0, 110, 10):
            jobs_with_alts = floor(n_jobs * percent_alt_jobs / 100)
            
            for n_machines in range(1, 11, 1):
                for percent_alt_machines in range(0, 110, 10):
                    n_alts_per_job = floor(n_machines * percent_alt_machines / 100)
            
                    for medium_size_task in [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500]:
                        horizon = n_jobs * medium_size_task
                        
                        for production_range in range(2, 22, 2):
                            for time_usage in range(50, 110, 10):
                                normal_time = normal_time_hours * 60 * time_usage / 100
                                for over_time_hours in range(0, 11, 1):
                                    over_time = over_time_hours * 60 * time_usage / 100
                                    
                                    jobs = generate_jobs(n_jobs, jobs_with_alts, n_alts_per_job, n_machines, medium_size_task, production_range, horizon)
                                    while not solvable(deepcopy(jobs), horizon):
                                        print(f'Repeated: {n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{medium_size_task}-{production_range}-{time_usage}-{over_time_hours}')
                                        random_seed += 1
                                        jobs = generate_jobs(n_jobs, jobs_with_alts, n_alts_per_job, n_machines, medium_size_task, production_range, horizon)
                                    
                                    save(jobs, n_machines, normal_time, over_time, horizon, f'{n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{medium_size_task}-{production_range}-{time_usage}-{over_time_hours}')

def generate_jobs(n_jobs, jobs_with_alts, n_alts_per_job, n_machines, medium_size_task, production_range, horizon):
    global random_seed
    random = Random()
    random.seed(random_seed)
    
    available_machines = list(range(1, n_machines + 1))
    standard_deviation = medium_size_task / 4
    
    jobs = {} # (130, {'task': [(2, 8036)], 'min_start': 0, 'max_end': 62587})
    for job in range(n_jobs):
        jobs[job] = {TASK: []}
        
        if jobs_with_alts > 0:
            alt_machines = random.sample(available_machines, n_alts_per_job)
            for alt_machine in alt_machines:
                alt_duration = max(1, (int) (random.gauss(medium_size_task, standard_deviation)))
                t = [0, 0]
                (t[TASK_MACHINE], t[TASK_DURATION]) = (alt_machine, alt_duration)
                jobs[job][TASK].append(tuple(t))
            jobs_with_alts -= 1
        else:
            alt_machine = random.choice(available_machines)
            alt_duration = max(1, (int)(random.gauss(medium_size_task, standard_deviation)))
            t = [0, 0]
            (t[TASK_MACHINE], t[TASK_DURATION]) = (alt_machine, alt_duration)
            jobs[job][TASK].append(tuple(t))
        
        max_duration = max(map(lambda x : x[TASK_DURATION], jobs[job][TASK]))
        production_interval = max_duration * production_range
        
        if (horizon <= production_interval):
            (jobs[job][MIN_START], jobs[job][MAX_END]) = (0, horizon)
        else:
            max_start = horizon - production_interval
            start     = random.randrange(  0   , max_start)
            min_end   =  start  + production_interval
            end       = random.randrange(min_end,  horizon)
            
            (jobs[job][MIN_START], jobs[job][MAX_END]) = (start, end)
    
    return jobs

def solvable(jobs, horizon):
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

    # Objective Function: Simplified (Only takes into account makespan)
    ends = [info[END_VAR] for info in jobs.values()]
    makespan = model.NewIntVar(0, horizon, 'makespan')
    model.AddMaxEquality(makespan, ends)
    model.Minimize(makespan)

    # Create Solver and Solve
    solver = CpSolver()
    status = solver.Solve(model)
    return status == OPTIMAL or status == FEASIBLE

def save(jobs, machines, normal_time, over_time, horizon, file):
    # TODO: use normal_time, over_time and horizon
    save_data(jobs, machines, 'data/' + file + '.py', 'data/' + file + '.pl', 'data/' + file + '.dat')

if __name__ == '__main__':
    generator()
