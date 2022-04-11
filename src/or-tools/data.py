
import json

from constants import TASK_MACHINE, TASK_DURATION, INFINITE_MACHINE, DataDifficulty

DATA = 'data/fab.json'

def get_data(data_difficulty=DataDifficulty.EASY):
    if data_difficulty == DataDifficulty.EASY:
        return get_data_easy()
    if data_difficulty == DataDifficulty.MEDIUM:
        return get_data_medium()
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

def get_data_medium():
    with open(DATA, 'r') as file:
        jobs = json.load(file)
    
    for (size, tasks) in jobs.values():
        for task in tasks:
            for alt_task in task:
                if alt_task[TASK_MACHINE] != INFINITE_MACHINE:
                    alt_task[TASK_DURATION] = alt_task[TASK_DURATION] * size
    return jobs

def get_data_hard():
    with open(DATA, 'r') as file:
        jobs = json.load(file)
    
    for job_id, (size, tasks) in jobs.items():
        new_tasks = get_new_tasks(size, tasks)
        jobs[job_id] = new_tasks
        
    return jobs

def get_new_tasks(size, tasks):
    new_tasks = []
    for task in tasks:
        if len(task) == 1 and task[0][TASK_MACHINE] == INFINITE_MACHINE:
            new_tasks.append(task)
        else:
            for _ in range(size):
                new_tasks.append(task)
    return new_tasks

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.HARD)
    print(jobs)
