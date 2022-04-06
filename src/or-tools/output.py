
import collections

PRODUCTION_TIME = 'production_time'
PRODUCTION_LINE = 'production_line'
CAPACITY        = 'capacity'
MODEL_TOTALS    = 'total'

def print_solution(solver, models, production_lines, all_tasks):
    print_objective_function(solver)
    print_decision_variables(solver, models, production_lines, all_tasks)

def print_objective_function(solver):
    print(f'Optimal Schedule Length: {solver.ObjectiveValue()}')

def print_decision_variables(solver, models, production_lines, all_tasks):
    assigned_task_type = collections.namedtuple('assigned_task_type',
                                                'start job index duration')
    assigned_jobs_per_machine = collections.defaultdict(list)

    # Create the list of assigned tasks per machine
    for model in models:
        machine = 1 # TODO: machine = productionLine
        
        assigned_jobs_per_machine[machine].append(
            assigned_task_type(start=solver.Value(all_tasks[model].start),
                                job=model,
                                index=0,
                                duration=models[model][PRODUCTION_TIME]
            )
        )

    # Create per machine output lines
    output = ''
    for machine in production_lines:
        # Sort by starting time
        assigned_jobs_per_machine[machine].sort()
        sol_line_tasks = 'Machine ' + str(machine) + ': '
        sol_line = '           '

        for assigned_task in assigned_jobs_per_machine[machine]:
            name = 'job_%i_task_%i' % (assigned_task.job,
                                        assigned_task.index)
            # Add spaces to output to align columns
            sol_line_tasks += '%-15s' % name

            start = assigned_task.start
            duration = assigned_task.duration
            sol_tmp = '[%i,%i]' % (start, start + duration)
            # Add spaces to output to align columns
            sol_line += '%-15s' % sol_tmp

        sol_line += '\n'
        sol_line_tasks += '\n'
        output += sol_line_tasks
        output += sol_line
    
    # Print Decision Variables
    print(output)