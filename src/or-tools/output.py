
from ortools.sat.python.cp_model import CpSolverSolutionCallback, CpSolver, IntVar, OPTIMAL, FEASIBLE

import matplotlib.pyplot as plt
import matplotlib as mpl
import pandas as pd

from constants import TASK_MACHINE, TASK_DURATION, TASK, START_VAR, END_VAR, PRESENCES_VAR

class IntermediateSolutionPrinter(CpSolverSolutionCallback):
    """Print intermediate solutions"""
    def __init__(self):
        CpSolverSolutionCallback.__init__(self)
        self.__solution_count = 0

    def on_solution_callback(self):
        """Called at each new solution"""
        print(f'Solution {self.__solution_count}, time = {self.WallTime()} s, objective = {self.ObjectiveValue()}')
        self.__solution_count += 1

def print_statistics(solver: CpSolver, status: int) -> None:
    # Solver Statistics
    print('--------------------------------')
    print('           Statistics           ')
    print('--------------------------------')
    print(f'  - conflicts: {solver.NumConflicts()}')
    print(f'  - branches : {solver.NumBranches()}')
    print(f'  - wall time: {solver.WallTime():8f} s')
    print('--------------------------------')
    print(f'  Solve status: {solver.StatusName(status)}')
    print(f'  Optimal objective value: {solver.ObjectiveValue()}')
    print('--------------------------------')
    
def print_results(solver: CpSolver, status: int, jobs: dict, obj_func: IntVar) -> None:
    # Print the results
    if status == OPTIMAL or status == FEASIBLE:
        print('Found a Solution')
        # Print Solution
        print_optimal_solution(solver, jobs, obj_func)
    else:
        print('No Solution Found')

def print_optimal_solution(solver: CpSolver, jobs: dict, obj_func: IntVar) -> None:
    # Print Final Solution
    for job, info in jobs.items():
        task      = info[         TASK]
        start     = info[    START_VAR]
        end       = info[      END_VAR]
        presences = info[PRESENCES_VAR]

        (machine, duration) = (-1, -1)
        for (alt_task, presence) in zip(task, presences):
            if solver.Value(presence):
                duration = alt_task[TASK_DURATION]
                machine  = alt_task[TASK_MACHINE ]
                break
        real_duration = solver.Value(end) - solver.Value(start)
        print(f'Job {job}: starts at {solver.Value(start)} (machine {machine}, duration {duration}, non-used overtime {real_duration - duration})')
    
    print(f'Objective Function: {solver.Value(obj_func)}')

def print_value(solver: CpSolver, status: int, value: IntVar) -> None:
    if status == OPTIMAL or status == FEASIBLE:
        print(f'{value}: {solver.Value(value)}')

def visualize(solver, jobs, intervals_per_machines, makespan, overtime, horizon):
    results = []
    for job in jobs:
        job_info = {}
        job_info['Job'] = job
        job_info['Start' ] = solver.Value(jobs[job][START_VAR])
        job_info['Finish'] = solver.Value(jobs[job][  END_VAR])
        for (alt_task, presence) in zip(jobs[job][TASK], jobs[job][PRESENCES_VAR]):
            if solver.Value(presence):
                job_info['Duration'] = alt_task[TASK_DURATION]
                job_info['Machine' ] = alt_task[TASK_MACHINE ]
                break
        
        results.append(job_info)
    
    schedule = pd.DataFrame(results)
    jobs     = sorted(jobs.keys())
    machines = sorted(intervals_per_machines.keys())
    makespan = solver.Value(makespan)
    overtime = solver.Value(overtime)
    
    schedule.sort_values(by=['Job', 'Start'])
    schedule.set_index(['Job', 'Machine'], inplace=True)

    colors = mpl.cm.Dark2.colors
    print(len(colors), len(colors[0]))
    
    figure, plot = plt.subplots()
    plot.grid(True)
    
    plot.set_title('Schedule')
    plot.set_xlabel('Time')
    plot.set_ylabel('Machine')
    
    plot.set_xlim(0, horizon)
    plot.set_ylim(0.5, len(machines) + 0.5)
    plot.set_yticks(machines)
    plot.set_yticklabels(machines)

    for job_index, job in enumerate(jobs):
        for machine_index, machine in enumerate(machines, 1):
            if (job, machine) in schedule.index:
                x_start  = schedule.loc[(job, machine), 'Start']
                x_finish = schedule.loc[(job, machine), 'Finish']
                plot.broken_barh([(x_start, x_finish)], (machine_index - 0.4, 0.8), facecolors=colors[job_index % (len(colors) - 1)])

    figure.tight_layout()
    plt.show()
