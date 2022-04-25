
import openpyxl as excel
import json, math
from copy import deepcopy

INFINITE_MACHINE = 0
PRODUCTION_TASK = 0 # TODO: modify to 1 when introducing resource arrival date

TASK_MACHINE  = 0
TASK_DURATION = 1

PRODUCTION_TIME = 'production_time'
PRODUCTION_LINE = 'production_line'
CAPACITY        = 'capacity'
MODEL_TOTALS    = 'total'

ORTOOLS_DATA_FILE = 'data/fab.json'
PROLOG_DATA_FILE  = 'data/fab.pl'
CPLEX_DATA_FILE   = 'data/fab.dat'

ORTOOLS_EASY_DATA_FILE = 'data/fab-easy.json'
PROLOG_EASY_DATA_FILE  = 'data/fab-easy.pl'
CPLEX_EASY_DATA_FILE   = 'data/fab-easy.dat'

ORTOOLS_EXAMPLE_DATA_FILE = 'data/example.json'
PROLOG_EXAMPLE_DATA_FILE  = 'data/example.pl'
CPLEX_EXAMPLE_DATA_FILE   = 'data/example.dat'

LOG = True

######################### Real Data #########################

def get_data():
    # Data Constants
    FILE = 'data/raw/oi_22_23.xlsx'
    TIMES_SHEET = 'times'
    TIMES_MODEL_COLUMN = 1
    TIMES_TIME_COLUMN  = 2
    LINES_SHEET = 'lines'
    LINES_LINE_COLUMN     = 1
    LINES_CAPACITY_COLUMN = 2
    LINES_MODELS_COLUMN   = 3
    TOTAL_SHEET = 'total'
    TOTAL_MODELS_COLUMN = 1
    TOTAL_TOTALS_COLUMN = 3
    
    # Prepare Data
    workbook = excel.load_workbook(FILE)
    models = {}
    lines = {}
    lines_aux = {}

    # Times
    sheet  = workbook[TIMES_SHEET]
    n_rows = sheet.max_row
    for row in range(2, n_rows + 1):
        model      = sheet.cell(row, TIMES_MODEL_COLUMN).value
        time_taken = sheet.cell(row, TIMES_TIME_COLUMN ).value
        models[model] = { PRODUCTION_TIME : time_taken, MODEL_TOTALS: 0}
    
    # Lines
    sheet = workbook[LINES_SHEET]
    n_rows = sheet.max_row
    for row in range(2, n_rows + 1):
        line        = sheet.cell(row, LINES_LINE_COLUMN    ).value
        capacity    = sheet.cell(row, LINES_CAPACITY_COLUMN).value
        line_models = sheet.cell(row, LINES_MODELS_COLUMN  ).value
        line_models = line_models.split(', ')
        min_max_models = []
        for line_model in line_models:
            values = line_model.split('-')
            min_max_models.append((int(values[0]), int(values[1])))
        lines[line] = { CAPACITY : capacity }
        lines_aux[line] = min_max_models
    
    # Models
    for model in models:
        models[model][PRODUCTION_LINE] = []
        for line in lines:
            for (min_model, max_model) in lines_aux[line]:
                if min_model <= model <= max_model:
                    models[model][PRODUCTION_LINE].append(line)
    
    # Totals
    sheet = workbook[TOTAL_SHEET]
    n_rows = sheet.max_row
    for row in range(2, n_rows + 1):
        model = sheet.cell(row, TOTAL_MODELS_COLUMN).value
        total = sheet.cell(row, TOTAL_TOTALS_COLUMN).value
        if model in models:
            models[model][MODEL_TOTALS] += total
        else:
            if LOG:
                print(f'Model {model} doesn\'t exist')
    
    # Remove Models that don't have any pieces to produce
    del_models = []
    for model_id, model in models.items():
        if model[MODEL_TOTALS] == 0:
            del_models.append(model_id)
    for del_model in del_models:
        if LOG:
            print(f'Model {del_model} doesn\'t have any pieces to produce')
        del models[del_model]
    
    # Finished
    workbook.close()
    return (models, lines)

def get_jobs(models, lines):
    # Each Job has 3 Tasks
    # Task 0 - before the resources arrive
    # Task 1 - production (may have alternative tasks in different machines)
    # Task 2 - delivery deadline

    jobs = {}
    for model_id, model in models.items():
        production_time = model[PRODUCTION_TIME] * model[MODEL_TOTALS]
        tasks = []
        alternative_tasks = []
    
        for production_line in model[PRODUCTION_LINE]:
            t = [0, 0]
            t[TASK_MACHINE ] = production_line
            t[TASK_DURATION] = math.ceil(production_time / lines[production_line][CAPACITY])
            alternative_tasks.append(tuple(t))

        tasks.append(alternative_tasks)
        
        jobs[model_id] = tasks
    return jobs

######################### Easy Data #########################

def get_easy_jobs():
    jobs = {  # task = (machine_id, processing_time)
        0: [  # Job 0
            [(1, 1), (2, 5), (3, 3)],  # task 0 with 3 alternatives
            [(1, 4), (2, 6), (3, 2)],  # task 1 with 3 alternatives
            [(1, 3), (2, 1), (3, 2)],  # task 2 with 3 alternatives
        ],
        1: [  # Job 1
            [(1, 3), (2, 4), (3, 2)],
            [(1, 5), (2, 4), (3, 1)],
            [(1, 1), (2, 4), (3, 2)],
        ],
        2: [  # Job 2
            [(1, 1), (2, 4), (3, 2)],
            [(1, 3), (2, 4), (3, 2)],
            [(1, 1), (2, 5), (3, 3)],
        ]
    }
    return jobs

def save_easy_data():
    jobs = get_easy_jobs()
    save_ortools(jobs, ORTOOLS_EASY_DATA_FILE)
    save_prolog (jobs,  PROLOG_EASY_DATA_FILE)
    save_cplex  (jobs,   CPLEX_EASY_DATA_FILE)

def example_data():
    jobs = {
        1: [
            [(1, 1), (2, 2)],
            [(1, 2), (2, 1)]
        ],
        2: [
            [(1, 3), (2, 2)],
            [(1, 1), (2, 1)]
        ]
    }
    
    save_ortools(jobs, ORTOOLS_EXAMPLE_DATA_FILE)
    save_prolog (jobs,  PROLOG_EXAMPLE_DATA_FILE)
    save_cplex  (jobs,   CPLEX_EXAMPLE_DATA_FILE)
    return

######################### Data to Files #########################

def save_data(jobs):
    save_ortools(jobs)
    save_prolog(jobs)
    save_cplex(jobs)

def save_ortools(jobs, ortools_file=ORTOOLS_DATA_FILE):
    with open(ortools_file, 'w') as file:
        json.dump(jobs, file)
    return

def save_prolog(jobs, prolog_file=PROLOG_DATA_FILE):
    lines = get_prolog_lines(deepcopy(jobs))
    
    with open(prolog_file, 'w') as file:
        file.writelines(lines)
    return

def get_prolog_lines(jobs):
    # Each Job has 3 Tasks
    # Task 0 - before the resources arrive
    # Task 1 - production (may have alternative tasks in different machines)
    # Task 2 - delivery deadline
    
    lines = [
        '% job(+JobId, +Tasks) | Tasks = [Task] | Task = [AltTask] | AltTask = MachineId-Duration\n'
    ]

    for model_id, tasks in jobs.items():
        for task in tasks:
            for alt_task_id, alt_task in enumerate(task):
                task[alt_task_id] = f'{alt_task[TASK_MACHINE]}-{alt_task[TASK_DURATION]}'
        
        line = f'job({model_id}, {tasks}).\n'
        line = line.replace('\'', '')
        lines.append(line)
    return lines

def save_cplex(jobs, cplex_file=CPLEX_DATA_FILE):
    lines = get_cplex_lines(jobs)
    
    with open(cplex_file, 'w') as file:
        file.writelines(lines)
    return

def get_cplex_lines(jobs):
    ops   = [ 'Ops = {   // OperationID, JobID, JobPosition\n'      ]
    modes = [ 'Modes = { // OperationID, Machine, ProcessingTime\n' ]
    
    for job_id, tasks in jobs.items():
        for task_id, task in enumerate(tasks):
            operation_id = job_id * 10 + task_id
            ops.append(f'  <{operation_id}, {job_id}, {task_id}>,\n')
            for alt_task in task:
                modes.append(f'  <{operation_id}, {alt_task[TASK_MACHINE]}, {alt_task[TASK_DURATION]}>,\n')
    
    ops  .append('};\n\n')
    modes.append('};\n')
    
    lines = ['Params = <3, 3>;\n\n'] # TODO
    lines.extend(ops  )
    lines.extend(modes)
    return lines

######################### Real Data Statistics #########################
def get_duration(job_tasks):
        job_tasks = job_tasks[0]
        return list(map(lambda x: x[TASK_DURATION], job_tasks))

def statistics(jobs, models):
    def n_production_lines(job_tasks):
        tasks = job_tasks[1]
        production_tasks = tasks[PRODUCTION_TASK]
        return len(production_tasks), models[job_tasks[0]][MODEL_TOTALS]

    job_tasks     = list(map(n_production_lines, jobs.items()))
    only_one_line = list(map(lambda x: x[1], filter(lambda job: job[0] == 1, job_tasks)))
    both_lines    = list(map(lambda x: x[1], filter(lambda job: job[0] == 2, job_tasks)))

    print()
    print(f'Number of Different Products: {len(job_tasks)}')
    print(f'Number of Products that can only be produced in 1 line: {len(only_one_line)} - {sum(only_one_line)}')
    print(f'Number of Products that can be produced in both lines:  {len(both_lines   )} - {sum(both_lines   )}')

    min_units = min(min(only_one_line), min(both_lines))
    max_units = max(max(only_one_line), max(both_lines))
    print(f'Units per Product: [{min_units}, {max_units}]')
    
    task_durations = map(get_duration, jobs.values())
    durations = [alt_task_duration for alt_task_durations in task_durations for alt_task_duration in alt_task_durations]
    
    print(f'Product Duration : [{min(durations)}, {max(durations)}]')

def model_statistics(jobs, models, id):
    print()
    print(f'Model {id}')
    print(f'  Production Time per Unit: {models[id][PRODUCTION_TIME]}')
    print(f'  Number of Units:          {models[id][MODEL_TOTALS]}')
    print(f'  Production Lines:         {models[id][PRODUCTION_LINE]}')
    print(f'  Duration:                 {get_duration(jobs[id])}')
    return

if __name__ == '__main__':
    # Get Data
    (models, lines) = get_data()
    jobs = get_jobs(models, lines)
    
    # Save Data
    save_data(jobs)
    
    # Show Statistics
    if LOG:
        statistics(jobs, models)
        model_statistics(jobs, models, 128)
        model_statistics(jobs, models, 818)
    
    # Save Easy Data
    save_easy_data()
    example_data()
    
    # Test
    print('Done.')
