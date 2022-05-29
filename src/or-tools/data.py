
import json

from constants import TASK_DURATION, TASK

def get_data(filename: str = 'data/fab.json') -> tuple:
    with open(filename, 'r') as file:
        data = json.load(file)
    return data

def get_horizon(jobs: dict) -> int:
    # Compute Horizon (worst case scenario)
    horizon = 0
    for job in jobs.values():
        task = job[TASK]
        max_task_duration = max([alt_task[TASK_DURATION] for alt_task in task])
        horizon += max_task_duration
    return horizon

def get_strict_horizon(jobs: dict) -> int:
    # Compute Horizon (worst case scenario)
    horizon = 0
    for job in jobs.values():
        task = job[TASK]
        max_task_duration = min([alt_task[TASK_DURATION] for alt_task in task])
        horizon += max_task_duration
    return horizon

if __name__ == '__main__':
    # Get Data
    (jobs, horizon) = get_data()
    print(list(jobs.items())[0])
    print(f'Horizon: [{get_strict_horizon(jobs)}, {get_horizon(jobs)}] → {horizon}')
