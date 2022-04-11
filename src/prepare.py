
import openpyxl as excel
import json, math

TASK_MACHINE  = 0
TASK_DURATION = 1

PRODUCTION_TIME = 'production_time'
PRODUCTION_LINE = 'production_line'
CAPACITY        = 'capacity'
MODEL_TOTALS    = 'total'

ORTOOLS_DATA_FILE = 'data/fab.json'
PROLOG_DATA_FILE = 'data/fab.pl'

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
            print(f'Model {model} doesn\'t exist')
    
    # Remove Models that don't have any pieces to produce
    del_models = []
    for model_id, model in models.items():
        if model[MODEL_TOTALS] == 0:
            del_models.append(model_id)
    for del_model in del_models:
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
        tasks = []
        alternative_tasks = []
    
        for production_line in model[PRODUCTION_LINE]:
            t = [0, 0]
            task_duration = model[MODEL_TOTALS] * model[PRODUCTION_TIME] / lines[production_line][CAPACITY]
            t[TASK_MACHINE ] = production_line
            t[TASK_DURATION] = math.ceil(task_duration)
            alternative_tasks.append(tuple(t))

        tasks.append(alternative_tasks)
        
        jobs[model_id] = tasks
    return jobs

def save_data(jobs):
    save_ortools(jobs)
    save_prolog(jobs)

def save_ortools(jobs):
    with open(ORTOOLS_DATA_FILE, 'w') as file:
        json.dump(jobs, file)
    return

def save_prolog(jobs):
    lines = get_prolog_lines(jobs)
    
    with open(PROLOG_DATA_FILE, 'w') as file:
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

if __name__ == '__main__':
    # Get Data
    (models, lines) = get_data()
    jobs = get_jobs(models, lines)
    save_data(jobs)
    
    # Test
    print('Done.')
