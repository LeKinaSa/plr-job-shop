
from ortools.sat.python.cp_model import CpSolverSolutionCallback, OPTIMAL as OPTIMAL_SOLUTION

from constants import TASK_MACHINE, TASK_DURATION

class IntermediateSolutionPrinter(CpSolverSolutionCallback):
    """Print intermediate solutions"""
    def __init__(self):
        CpSolverSolutionCallback.__init__(self)
        self.__solution_count = 0

    def on_solution_callback(self):
        """Called at each new solution"""
        print(f'Solution {self.__solution_count}, time = {self.WallTime()} s, objective = {self.ObjectiveValue()}')
        self.__solution_count += 1

def print_statistics(solver, status):
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
    
def print_results(solver, status, jobs, starts, presences):
    # Print the results
    if status == OPTIMAL_SOLUTION:
        print('Found a Solution')
        # Print Solution
        print_optimal_solution(solver, jobs, starts, presences)
    else:
        print('No Solution Found')

def print_optimal_solution(solver, jobs, starts, presences):
    machine_ends = {}
    
    # Print Final Solution
    for job_id, job in jobs.items():
        print(f'Job {job_id}:')
        for task_id, task in enumerate(job):
            start_value = solver.Value(starts[(job_id, task_id)])
            (machine, duration, selected) = (-1, -1, -1)
            
            for alt_id, alt_task in enumerate(task):
                if solver.Value(presences[(job_id, task_id, alt_id)]):
                    selected = alt_id
                    duration = alt_task[TASK_DURATION]
                    machine  = alt_task[TASK_MACHINE ]
                    break
            
            print(f'  task_{task_id} starts at {start_value} (alt {selected}, machine {machine}, duration {duration})')
            machine_ends[machine] = max(machine_ends.get(machine, 0), start_value + duration)
    
    print(machine_ends)

if __name__ == '__main__':
    print('Configured')
