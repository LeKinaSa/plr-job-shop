from constants import TASK, MIN_START, MAX_END, START_VAR, END_VAR, DURATION_VAR, PRESENCES_VAR

from ortools.sat.python.cp_model import CpModel as OR_CpModel, CpSolver as OR_CpSolver
from docplex.cp.model import CpoModel as DOC_CpModel, CpoSolver as DOC_CpSolver, CpoParameters as DOC_CpoParameters, CpoSolverInfos

DOCPLEX = "DOcplex"
ORTOOLS = "OR-Tools"

class SolverType:
    def __init__(self, solver_type=0) -> None:
        if solver_type == 1:
            self.solver_type = DOCPLEX
        else:
            self.solver_type = ORTOOLS

    # Create model
    def CpModel(self):
        if self.solver_type == DOCPLEX:
            self.model = DOC_CpModel()
        else: # OR-Tools
            self.model = OR_CpModel()
        return self

    def CpSolver(self):
        if self.solver_type == DOCPLEX:
            self.solver = DOC_CpSolver(self.model)
        else:
            self.solver = OR_CpSolver()
        return self

    # Int Var
    def NewIntVar(self, min, max, id):
        if self.solver_type == DOCPLEX:
            return self.model.integer_var(min, max, id) # check arguments
        else: # OR-Tools
            return self.model.NewIntVar(min, max, id)

    # Bool Var
    def NewBoolVar(self, name = ''):
        if self.solver_type == DOCPLEX:
            return self.model.binary_var(name)
        else: # OR-Tools
            return self.model.NewBoolVar(name)

    # Optional Interval Var
    def NewOptionalIntervalVar(self, alt_start, alt_duration, alt_end, alt_present, name = ''):
        if self.solver_type == DOCPLEX:
            interval = self.model.interval_var(optional=True, name=name)
            self.model.add(self.model.equal(self.model.start_of  (interval), alt_start   ))
            self.model.add(self.model.equal(self.model.end_of    (interval), alt_end     ))
            self.model.add(self.model.equal(self.model.length_of (interval), alt_duration))
            self.model.add(interval.is_present() == alt_present)
            return interval
        else: # OR-Tools
            return self.model.NewOptionalIntervalVar(alt_start, alt_duration, alt_end, alt_present, name)

    # Add
    def Add(self, constraint):
        if self.solver_type == DOCPLEX:
            return self.model.add(constraint)
        else: # OR-Tools
            return self.model.Add(constraint)

    # Only Enforce If
    def OnlyEnforceIf(self, constraint, alt_present):
        if self.solver_type == DOCPLEX:
            self.model = DOC_CpModel()
            return self.model.if_then(alt_present == 1, constraint) # TODO check
        else: # OR-Tools
            return self.model.Add(constraint).OnlyEnforceIf(alt_present)

    # Add Exactly One
    def AddExactlyOne(self, l):
        if self.solver_type == DOCPLEX:
            return self.model.add_constraint(self.model.equal(self.model.sum(l), 1))
        else: # OR-Tools
            return self.model.AddExactlyOne(l)

    # Add No Overlap
    def AddNoOverlap(self, intervals):
        if self.solver_type == DOCPLEX:
            return self.model.no_overlap(intervals)
        else: # OR-Tools
            return self.model.AddNoOverlap(intervals)

    # Add Max Equality
    def AddMaxEquality(self, target, values):
        if self.solver_type == DOCPLEX:
            return self.model.add(target == self.model.max_of(values))
        else:
            return self.model.AddMaxEquality(target, values)

    def AddMinEquality(self, target, values):
        if self.solver_type == DOCPLEX:
            return self.model.add(target == self.model.min_of(values))
        else:
            return self.model.AddMinEquality(target, values)

    # Add Modulo Equality
    def AddModuloEquality(self, target, variable, modulo):
        if self.solver_type == DOCPLEX:
            return self.model.add(self.model.equal(target, self.model.mod(variable, modulo)))
        else:
            return self.model.AddModuloEquality(target, variable, modulo)
    
    def AddDivisionEquality(self, target, variable, denominator):
        if self.solver_type == DOCPLEX:
            return self.model.add(self.model.equal(target, self.model.int_div(variable, denominator)))
        else:
            return self.model.AddDivisionEquality(target, variable, denominator)
    
    def AddMultiplicationEquality(self, target, variable, coefficient):
        if self.solver_type == DOCPLEX:
            return self.model.add(self.model.equal(target, self.model.times(variable, coefficient)))
        else:
            return self.model.AddMultiplicationEquality(target, variable, coefficient)
    
    # Maximize
    def Maximize(self, maximize_goal):
        if self.solver_type == DOCPLEX:
            return self.model.maximize(maximize_goal)
        else:
            return self.model.Maximize(maximize_goal)

    # Minimize
    def Minimize(self, minimize_goal):
        if self.solver_type == DOCPLEX:
            return self.model.minimize(minimize_goal)
        else:
            return self.model.Minimize(minimize_goal)

    # Solve
    def Solve(self, model, solution_printer=None):
        if self.solver_type == DOCPLEX:
            self.info = CpoSolverInfos()
            return self.solver.solve(self.info)
        else:
            return self.solver.Solve(model.model, solution_printer)
    
    def NumConflicts(self):
        if self.solver_type == DOCPLEX:
            return self.info.get_number_of_fails()
        else:
            return self.solver.NumConflicts()

    def NumBranches(self):
        if self.solver_type == DOCPLEX:
            return self.info.get_number_of_branches()
        else:
            return self.solver.NumBranches()

    def WallTime(self):
        if self.solver_type == DOCPLEX:
            return self.solver.get_solve_time()
        else:
            return self.solver.WallTime()
    
    def StatusName(self, status):
        if self.solver_type == DOCPLEX:
            return self.solver.get_solve_status()
        else:
            return self.solver.StatusName(status)
    
    def ObjectiveValue(self):
        if self.solver_type == DOCPLEX:
            return self.solution.get_objective_value()
        else:
            return self.solver.ObjectiveValue()
    
    def Value(self, variable):
        if self.solver_type == DOCPLEX:
            return self.solver.get_var_solution(variable) # TODO: verify
        else:
            return self.solver.Value(variable)

    def MaxTimeInSeconds(self, time_out_in_seconds):
        if self.solver_type == DOCPLEX:
            self.model.set_parameters(DOC_CpoParameters(TimeLimit=time_out_in_seconds))
        else:
            self.solver.parameters.max_time_in_seconds = time_out_in_seconds
        return