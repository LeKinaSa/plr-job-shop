
from ortools.sat.python.cp_model import CpSolverSolutionCallback, CpSolver, IntVar, OPTIMAL, FEASIBLE

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
