
import json

from constants import TASK_MACHINE, TASK_DURATION, INFINITE_MACHINE, DataDifficulty

EASY_DATA   = 'data/fab-easy.json'
MEDIUM_DATA = 'data/fab.json'

def get_data(data_difficulty=DataDifficulty.EASY):
    if data_difficulty == DataDifficulty.EASY:
        return get_data_easy()
    if data_difficulty == DataDifficulty.MEDIUM:
        return get_data_medium()
    if data_difficulty == DataDifficulty.HARD:
        return get_data_hard()

def get_data_easy():
    with open(EASY_DATA, 'r') as file:
        jobs = json.load(file)
    return jobs

def get_data_medium():
    with open(MEDIUM_DATA, 'r') as file:
        jobs = json.load(file)
    return jobs

def get_data_hard():
    return get_data_medium()

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.MEDIUM)
    print(list(jobs.items())[0])
