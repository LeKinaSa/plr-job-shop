
import json

from constants import DataDifficulty, TASK_DURATION, TASK

EXAMPLE_DATA = 'data/example.json'
EASY_DATA    = 'data/fab-easy.json'
MEDIUM_DATA  = 'data/fab.json'

def get_data(data_difficulty=DataDifficulty.MEDIUM):
    data_file = EXAMPLE_DATA
    if data_difficulty == DataDifficulty.EASY:
        data_file = EASY_DATA
    if data_difficulty == DataDifficulty.MEDIUM:
        data_file = MEDIUM_DATA
    if data_difficulty == DataDifficulty.HARD:
        data_file = EXAMPLE_DATA
    
    with open(data_file, 'r') as file:
        jobs = json.load(file)
    return jobs, get_horizon(jobs)

def get_horizon(jobs):
    # Compute Horizon (worst case scenario)
    horizon = 0
    for job in jobs.values():
        task = job[TASK]
        max_task_duration = min([alt_task[TASK_DURATION] for alt_task in task])
        horizon += max_task_duration
    return horizon

def get_overtime_intervals(horizon, normal_time=1920, over_time=384):
    intervals = [(normal_time, normal_time + over_time)]
    offset = normal_time + over_time
    while intervals[-1][0] + offset < horizon:
        last_interval = intervals[-1]
        intervals.append((last_interval[0] + offset, last_interval[1] + offset))
    return intervals

if __name__ == '__main__':
    # Get Data
    (jobs, _) = get_data()
    print(list(jobs.items())[0])
    print()
    print(get_overtime_intervals(58547))
