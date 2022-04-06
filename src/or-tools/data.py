
import json

from constants import TASK_DURATION, TASK_MACHINE, DataDifficulty

PRODUCTION_TIME = 'production_time'
PRODUCTION_LINE = 'production_line'
CAPACITY        = 'capacity'
MODEL_TOTALS    = 'total'

def get_data(data_difficulty=DataDifficulty.EASY):
    if data_difficulty == DataDifficulty.EASY:
        return get_data_easy()
    if data_difficulty == DataDifficulty.HARD:
        return get_data_hard()

def get_data_easy():
    jobs = {  # task = (machine_id, processing_time)
        0: [  # Job 0
            [(0, 3), (1, 1), (2, 5)],  # task 0 with 3 alternatives
            [(0, 2), (1, 4), (2, 6)],  # task 1 with 3 alternatives
            [(0, 2), (1, 3), (2, 1)],  # task 2 with 3 alternatives
        ],
        1: [  # Job 1
            [(0, 2), (1, 3), (2, 4)],
            [(0, 1), (1, 5), (2, 4)],
            [(0, 2), (1, 1), (2, 4)],
        ],
        2: [  # Job 2
            [(0, 2), (1, 1), (2, 4)],
            [(0, 2), (1, 3), (2, 4)],
            [(0, 3), (1, 1), (2, 5)],
        ]
    }
    return jobs

def get_data_hard():
    with open('data/models.json', 'r') as file:
        models = json.load(file)
    del_models = []
    for model_id, model in models.items():
        if model[MODEL_TOTALS] == 0:
            del_models.append(model_id)
    for del_model in del_models:
        del models[del_model]
    
    with open('data/lines.json', 'r') as file:
        lines_raw = json.load(file)
    lines = {}
    for line_id, line in lines_raw.items():
        lines[int(line_id)] = line
    
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
            t[TASK_MACHINE ] = production_line
            t[TASK_DURATION] = model[MODEL_TOTALS] * model[PRODUCTION_TIME] / lines[production_line][CAPACITY]
            alternative_tasks.append(tuple(t))

        tasks.append(alternative_tasks)
        
        jobs[model_id] = tasks
    return jobs

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.HARD)
    print(jobs)
