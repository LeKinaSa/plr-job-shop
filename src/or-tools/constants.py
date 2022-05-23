from enum import Enum, auto

INFINITE_MACHINE = 0

TASK_MACHINE  = 0
TASK_DURATION = 1

MIN_START = 'min_start'
MAX_END   = 'max_end'
TASK      = 'task'

class DataDifficulty(Enum):
    EXAMPLE = auto()
    EASY    = auto()
    MEDIUM  = auto()
    HARD    = auto()
