from enum import Enum, auto

INFINITE_MACHINE = 0

TASK_MACHINE  = 0
TASK_DURATION = 1

class DataDifficulty(Enum):
    EXAMPLE = auto()
    EASY    = auto()
    MEDIUM  = auto()
    HARD    = auto()
