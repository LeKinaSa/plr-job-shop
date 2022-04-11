
import json

from constants import DataDifficulty

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
    return jobs

def get_data_hard():
    return {}

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.HARD)
    print(jobs)
