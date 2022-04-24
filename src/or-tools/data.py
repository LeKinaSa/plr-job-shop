
import json

from constants import DataDifficulty

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

if __name__ == '__main__':
    # Get Data
    jobs = get_data(DataDifficulty.MEDIUM)
    print(list(jobs.items())[0])
