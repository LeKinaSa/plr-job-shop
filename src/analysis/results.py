import pandas as pd

filenames = ['conflicts', 'branches', 'wall_time', 'status', 'obj_value']
ORTOOLS_PATH = 'data/statistics/ortools/'
SPROLOG_PATH = 'data/statistics/prolog/'
DOCPLEX_PATH = 'data/statistics/docplex/'

ortools_files = [open(ORTOOLS_PATH + filename + '.txt') for filename in filenames]
sprolog_files = [open(SPROLOG_PATH + filename + '.txt') for filename in filenames]
docplex_files = [open(DOCPLEX_PATH + filename + '.txt') for filename in filenames]

tests = ['n_jobs', 'percent_alt_jobs',
         'n_machines', 'percent_alt_machines',
         'avg_task_size', 'production_range',
         'time_usage', 'over_time_hours',
         'time_out']

test_info = {}
for test in tests:
    test_info[test] = {}

for ortools_file, sprolog_file, docplex_file, filename in zip(ortools_files, sprolog_files, docplex_files, filenames):
    for test in tests:
        test_info[test]['ortools_' + filename] = ortools_file.readline().strip('\n').split(',')[1:]
        test_info[test]['sprolog_' + filename] = sprolog_file.readline().strip('\n').split(',')[1:]
        test_info[test]['docplex_' + filename] = docplex_file.readline().strip('\n').split(',')[1:]

for f in ortools_files + sprolog_files + docplex_files:
    f.close()

for test in tests:
    test_info[test] = pd.DataFrame.from_dict(test_info[test])

print(test_info['n_jobs'])
