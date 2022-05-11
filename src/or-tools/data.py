
import json

from constants import DataDifficulty, TASK_DURATION

EXAMPLE_DATA = 'data/example.json'
EASY_DATA    = 'data/fab-easy.json'
MEDIUM_DATA  = 'data/fab.json'

def get_data(data_difficulty=DataDifficulty.EXAMPLE):
    data_file = EXAMPLE_DATA
    if data_difficulty == DataDifficulty.EASY:
        data_file = EASY_DATA
    if data_difficulty == DataDifficulty.MEDIUM:
        data_file = MEDIUM_DATA
    if data_difficulty == DataDifficulty.HARD:
        data_file = EXAMPLE_DATA
    
    with open(data_file, 'r') as file:
        jobs = json.load(file)
    return jobs

def get_horizon(jobs):
    horizon = 0
    for job in jobs.values():
        for task in job:
            max_task_duration = min([alt_task[TASK_DURATION] for alt_task in task])
            horizon += max_task_duration
    return horizon

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.MEDIUM)
    print(list(jobs.items())[0])
