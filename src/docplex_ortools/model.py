from constants import TASK, MIN_START, MAX_END, START_VAR, END_VAR, DURATION_VAR, PRESENCES_VAR

from ortools.sat.python.cp_model import CpModel, CpSolver as OR_CpModel, OR_CpSolver

from docplex.cp.model import CpoModel as DOC_CpModel
from docplex.cp.model import interval_var, binary_var, no_overlap, sum, add_constraint

DOCPLEX = "DOcplex"
ORTOOLS = "OR-Tools"


class Model:
    def __init__(self, solver) -> None:
        if solver == 1:
            self.solver = DOCPLEX
        elif solver == 2:
            self.solver = ORTOOLS
        else:
            raise Exception("Unknown solver")

    # Create model
    def Model(self):
        if self.solver == DOCPLEX:
            self.model = DOC_CpModel()
            return 
        else: # OR-Tools
            self.model = OR_CpModel()
            return 

    # Int Var
    def NewIntVar(self, min, max, id):
        if self.solver == DOCPLEX:
            return interval_var(min, max, id) # check arguments
        else: # OR-Tools
            return self.model.NewIntVar(min, max, id)

    # Add
    # TODO
    def Add(self):
        if self.solver == DOCPLEX:
            return 
        else: # OR-Tools
            return 

    # Only Enforce If
    # TODO
    def OnlyEnforceIf(self, alt_present):
        if self.solver == DOCPLEX:
            return 
        else: # OR-Tools
            return self.model.OnlyEnforceIf(alt_present)

    # Bool Var
    def NewBoolVar(self, name = ''):
        if self.solver == DOCPLEX:
            return binary_var(name)
        else: # OR-Tools
            return self.model.NewBoolVar(name)

    # Optional Interval Var
    def NewOptionalIntervalVar(self, alt_start, alt_duration, alt_end, alt_present, name = ''):
        if self.solver == DOCPLEX:
            return interval_var(alt_start, alt_end, alt_duration, optional=True, name=name)
        else: # OR-Tools
            return self.model.NewOptionalIntervalVar(alt_start, alt_duration, alt_end, alt_present, name)

    # Add Exactly One
    # most likely not working
    def AddExactlyOne(self, jobs, job):
        if self.solver == DOCPLEX:
            return add_constraint(sum(jobs[job][PRESENCES_VAR]), "=", 1)
        else: # OR-Tools
            return self.model.AddExactlyOne(jobs[job][PRESENCES_VAR])

    # Add No Overlap
    def AddNoOverlap(self, intervals):
        if self.solver == DOCPLEX:
            return no_overlap(intervals) # might not work
        else: # OR-Tools
            return self.model.AddNoOverlap(intervals)

    # Add Max Equality


    # Maximize

    # Minimize

    # Solve
    