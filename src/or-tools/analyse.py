from os.path import exists
from os import makedirs

from main import jobshop
from model import SolverType

files = ['conflicts', 'branches', 'wall_time', 'status', 'obj_value']
ORTOOLS_PATH = 'data/statistics/ortools/'
SPROLOG_PATH = 'data/statistics/prolog/'
DOCPLEX_PATH = 'data/statistics/docplex/'

def analyser(solver_type: int = 0):
    for n_jobs in [2, 5, 10, 20, 40, 60, 80, 100, 125, 150]: # default: 75
        analyse(n_jobs=n_jobs, solver_type=solver_type)
    enter_files()
    for percent_alt_jobs in range(0, 101, 10): # default: 50
        analyse(percent_alt_jobs=percent_alt_jobs, solver_type=solver_type)
    enter_files()
    
    for n_machines in range(1, 11, 1): # default: 4
        analyse(n_machines=n_machines, solver_type=solver_type)
    enter_files()
    for percent_alt_machines in range(0, 101, 10): # default: 75
        analyse(percent_alt_machines=percent_alt_machines, solver_type=solver_type)
    enter_files()
    
    for average_size_task in [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000, 2500]: # default: 50
        analyse(average_size_task=average_size_task, solver_type=solver_type)
    enter_files()
    for production_range in [1, 2, 3, 4, 5] + list(range(6, 21, 2)): # default: 2
        analyse(production_range=production_range, solver_type=solver_type)
    enter_files()
    
    for time_usage in range(50, 110, 10): # default: 80
        analyse(time_usage=time_usage, solver_type=solver_type)
    enter_files()
    for over_time_hours in range(0, 11, 1): # default: 8
        analyse(over_time_hours=over_time_hours, solver_type=solver_type)
    enter_files()

    for time_out in range(1, 301):
        analyse(time_out=time_out, solver_type=solver_type)

def analyse(n_jobs: int = 75, percent_alt_jobs: int = 50,
            n_machines: int = 4, percent_alt_machines: int = 75,
            average_size_task: int = 50, production_range: int = 2,
            time_usage: int = 80, over_time_hours: int = 8,
            time_out: int = 15, solver_type: int = 0):

    filename = f'{n_jobs}-{percent_alt_jobs}-{n_machines}-{percent_alt_machines}-{average_size_task}-{production_range}-{time_usage}-{over_time_hours}'
    (solver, status) = jobshop(solver_type, f'data/simulated/{filename}.json', time_out, False, True)
    s = SolverType(solver_type)
    if status == s.OPTIMAL() or status == s.FEASIBLE():
        save_files(solver, status, solver_type)
    else:
        s = 'OR-Tools' if solver_type == 0 else 'DOcplex'
        print(f'Problem! File {filename} with solver {s}')

def start_files(path: str) -> None:
    global files
    for filename in files:
        with open(path + filename + '.txt', 'w+') as file:
            file.truncate(0)

def enter_files() -> None:
    global files
    for filename in files:
        for path in [ORTOOLS_PATH, DOCPLEX_PATH]:
            with open(path + filename + '.txt', 'a') as file:
                file.write('\n')

def save_files(solver: SolverType, status: int, solver_type: int) -> None:
    global files
    path = ORTOOLS_PATH if solver_type == 0 else DOCPLEX_PATH
    solver_information = [solver.NumConflicts(), solver.NumBranches(), solver.WallTime(), solver.StatusName(status), solver.ObjectiveValue()]
    for (filename, info) in zip(files, solver_information):
        with open(path + filename + '.txt', 'a') as file:
            file.write(f', {info}')

if __name__ == '__main__':
    if not exists(SPROLOG_PATH):
        makedirs(SPROLOG_PATH)
        start_files(SPROLOG_PATH)
    else:
        if not exists(ORTOOLS_PATH):
            makedirs (ORTOOLS_PATH)
        start_files(ORTOOLS_PATH)

        if exists('data/simulated'):
            analyser(0) # OR-Tools
        else:
            print('No files to analyse!')

        if not exists(DOCPLEX_PATH):
            makedirs (DOCPLEX_PATH)
        start_files(DOCPLEX_PATH)
        
        if exists('data/simulated'):
            analyser(1) # DOcplex
        else:
            print('No files to analyse!')
