from constants import TASK_DURATION

def get_decision_variables(model, jobs, horizon):
    # Decision Variables (indexed by job_id, task_id)
    starts = {}
    ends   = {}
    chosen = {}
    for job_id, job in jobs.items():
        for task_id, task in enumerate(job):
            start    = model.NewIntVar(0, horizon      ,  f'start_j{job_id}_t{task_id}')
            end      = model.NewIntVar(0, horizon      ,    f'end_j{job_id}_t{task_id}')
            chosen   = model.NewIntVar(0, len(task) - 1, f'chosen_j{job_id}_t{task_id}')
            duration = task[chosen]
            starts[(job_id, task_id)] = start
            ends  [(job_id, task_id)] = end

# Constraint: Only 1 Alternative Task is Chosen per Task


# Contraint: Task Duration is Respected


# Constraint: Two Jobs on the Same Machine can't overlap
def no_overlap(model, intervals_per_machines):
    for intervals in intervals_per_machines.values():
        if len(intervals) > 1:
            model.AddNoOverlap(intervals)

# Constraint: Job Precedence → Production must be between Resource Arrival and Product Departure
def job_precedence(model, jobs, starts, ends):
    for job_id, job in jobs.items():
        previous_end = None
        for task_id, task in enumerate(job):
            start = starts[(job_id, task_id)]
            end   = ends[(job_id, task_id)]
            if previous_end is not None:
                model.Add(start >= previous_end)
            previous_end = end
