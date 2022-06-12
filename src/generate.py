
from ortools.sat.python.cp_model import CpModel, CpSolver, OPTIMAL, FEASIBLE

from os.path import exists
from os import makedirs
from math import floor
from copy import deepcopy
from random import Random

from prepare import save_data, get_data, get_jobs

TASK_MACHINE  = 0
TASK_DURATION = 1
MIN_START = 'min_start'
MAX_END   = 'max_end'
TASK      = 'task'
START_VAR     = 'start'
END_VAR       = 'end'
DURATION_VAR  = 'duration'
PRESENCES_VAR = 'intervals'

random_seed = 0

def generator():
    global random_seed
    
    for n_jobs in [2, 5, 10, 20, 40, 60, 80, 100, 125, 150]: # default: 75
        generate(n_jobs=n_jobs)
    for percent_alt_jobs in range(0, 101, 10): # default: 50
        generate(percent_alt_jobs=percent_alt_jobs)
    
    for n_machines in range(1, 11, 1): # default: 4
        generate(n_machines=n_machines)
    for percent_alt_machines in range(0, 101, 10): # default: 75
        generate(percent_alt_machines=percent_alt_machines)
    
    for average_size_task in [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000, 2500]: # default: 50
        generate(average_size_task=average_size_task)
    for production_range in [1, 2, 3, 4, 5] + list(range(6, 21, 2)): # default: 2
        generate(production_range=production_range)
    
    for time_usage in range(50, 110, 10): # default: 80
        generate(time_usage=time_usage)
    for over_time_hours in range(0, 11, 1): # default: 8
        generate(over_time_hours=over_time_hours)

def generate(n_jobs: int = 75, percent_alt_jobs: int = 50,
            n_machines: int = 4, percent_alt_machines: int = 75,
            average_size_task: int = 50, production_range: int = 2,
            time_usage: int = 80, over_time_hours: int = 8, normal_time_hours: int = 40) -> None:
    global random_seed

    jobs_with_alts =        floor(n_jobs     * percent_alt_jobs     / 100)
    n_alts_per_job = max(1, floor(n_machines * percent_alt_machines / 100))
    horizon        = n_jobs * average_size_task
    normal_time    = (int) (normal_time_hours * 60 * time_usage / 100)
    over_time      = (int) (over_time_hours   * 60 * time_usage / 100)
    
    jobs = generate_jobs(n_jobs, jobs_with_alts, n_alts_per_job, n_machines, average_size_task, production_range, horizon)
    while not solvable(deepcopy(jobs), horizon):
        print(f'Repeated: {n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{average_size_task}-{production_range}-{time_usage}-{over_time_hours}')
        random_seed += 1
        jobs = generate_jobs(n_jobs, jobs_with_alts, n_alts_per_job, n_machines, average_size_task, production_range, horizon)
    
    save(jobs, n_machines, normal_time, over_time, horizon, f'{n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{average_size_task}-{production_range}-{time_usage}-{over_time_hours}')

def generate_jobs(n_jobs: int, jobs_with_alts: int, n_alts_per_job: int, n_machines: int,
                  average_size_task: int, production_range: int, horizon: int) -> dict:
    global random_seed
    random = Random()
    random.seed(random_seed)
    
    available_machines = list(range(1, n_machines + 1))
    standard_deviation = average_size_task / 4
    
    jobs = {} # (130, {'task': [(2, 8036)], 'min_start': 0, 'max_end': 62587})
    for job in range(n_jobs):
        jobs[job] = {TASK: []}
        
        if jobs_with_alts > 0:
            alt_machines = random.sample(available_machines, n_alts_per_job)
            for alt_machine in alt_machines:
                alt_duration = max(1, (int) (random.gauss(average_size_task, standard_deviation)))
                t = [0, 0]
                (t[TASK_MACHINE], t[TASK_DURATION]) = (alt_machine, alt_duration)
                jobs[job][TASK].append(tuple(t))
            jobs_with_alts -= 1
        else:
            alt_machine = random.choice(available_machines)
            alt_duration = max(1, (int)(random.gauss(average_size_task, standard_deviation)))
            t = [0, 0]
            (t[TASK_MACHINE], t[TASK_DURATION]) = (alt_machine, alt_duration)
            jobs[job][TASK].append(tuple(t))
        
        l = list(map(lambda x : x[TASK_DURATION], jobs[job][TASK]))
        if len(l) == 0:
            print(jobs[job], n_alts_per_job, jobs_with_alts)
            return
        max_duration = max(l)
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

def solvable(jobs: dict, horizon: int) -> bool:
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

def save(jobs: dict, machines: int, normal_time: int, over_time: int, horizon: int, file: str):
    file = 'data/simulated/' + file
    save_data(jobs, machines, horizon, normal_time, over_time, file + '.json', file + '.pl', file + '.dat')

if __name__ == '__main__':
    ### Real Data
    # Get Data
    (models, lines) = get_data()
    jobs = get_jobs(models, lines)

    # Save Data
    save_data(jobs, len(lines))

    ### Generate Simulated Data
    if not exists('data/simulated'):
        makedirs('data/simulated')
    generator()
