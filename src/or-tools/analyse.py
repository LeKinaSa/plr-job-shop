
from ortools.sat.python.cp_model import CpSolver, OPTIMAL, FEASIBLE

from os.path import exists
from os import makedirs

from main import jobshop

files = ['conflits', 'branches', 'wall_time', 'status', 'obj_value']

def analyser():
    start_files()
    
    for n_jobs in [5, 10, 20, 40, 60, 80, 100, 125, 150]: # default: 75
        analyse(n_jobs=n_jobs)
    enter_files()
    for percent_alt_jobs in range(0, 101, 10): # default: 50
        analyse(percent_alt_jobs=percent_alt_jobs)
    enter_files()
    
    for n_machines in range(1, 11, 1): # default: 4
        analyse(n_machines=n_machines)
    enter_files()
    for percent_alt_machines in range(0, 101, 10): # default: 75
        analyse(percent_alt_machines=percent_alt_machines)
    enter_files()
    
    for medium_size_task in [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000, 2500]: # default: 50
        analyse(medium_size_task=medium_size_task)
    enter_files()
    for production_range in [1, 2, 3, 4, 5] + list(range(6, 21, 2)): # default: 2
        analyse(production_range=production_range)
    enter_files()
    
    for time_usage in range(50, 110, 10): # default: 80
        analyse(time_usage=time_usage)
    enter_files()
    for over_time_hours in range(0, 11, 1): # default: 8
        analyse(over_time_hours=over_time_hours)

def analyse(n_jobs: int = 75, percent_alt_jobs: int = 50,
            n_machines: int = 4, percent_alt_machines: int = 75,
            medium_size_task: int = 50, production_range: int = 2,
            time_usage: int = 80, over_time_hours: int = 8):

    filename = f'{n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{medium_size_task}-{production_range}-{time_usage}-{over_time_hours}'
    (solver, status) = jobshop(f'data/simulated/{filename}.json', False)
    if status == OPTIMAL or status == FEASIBLE:
        save_files(solver)
    else:
        print(f'Problem! File {filename}')

def start_files() -> None:
    global files
    for filename in files:
        with open('data/statistics/' + filename + '.txt', 'w+') as file:
            file.truncate(0)

def enter_files() -> None:
    global files
    for filename in files:
        with open('data/statistics/' + filename + '.txt', 'a') as file:
            file.write('\n')

def save_files(solver: CpSolver) -> None:
    global files
    solver_information = [solver.NumConflicts(), solver.NumBranches(), solver.WallTime(), solver.StatusName(), solver.ObjectiveValue()]
    for (filename, info) in zip(files, solver_information):
        with open('data/statistics/' + filename + '.txt', 'a') as file:
            file.write(f', {info}')

if __name__ == '__main__':
    if not exists('data/statistics'):
        makedirs ('data/statistics')
    if exists('data/simulated'):
        # analyser()
        analyse(5, 50, 4, 75, 50, 2, 80, 8)
    else:
        print('No files to analyse!')
