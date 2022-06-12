# Flexible Job Shop Problem with Overtime Constraints

PLR-project

## Group 02

| Group Member | 02 |
| --- | --- |
| Clara Alves Martins | up201806528 |
| João António Cardoso Vieira e Basto de Sousa | up201806613 |

## Instructions

1. Make sure that the IBM Decision Optimization CPLEX is installed and correctly set up to work with python.
2. Make sure that SICStus Prolog is installed.
3. Install thew following python packages: `openpyxl`, `docplex`, `ortools`.
4. Make sure the file `oi_22_23.xlsx` is inside the `data/raw` directory.
5. Run the command `python src/generate.py` from the project's directory to parse the real data and generate simulated data for python and prolog.
6. For Prolog:
    a. To run the real world problem, consult the `main.pl` file and run the command `j.` or `jobshop.`.
    b. To run the tests presented in the report, consult the `analyse.pl` file and run `analyser(WorkingDirectory).`, where the working directory is the absolute path to the folder `prolog` inside the `src` folder.
7. For OR-Tools and DOcplex:
    a. To run the real world problem, execute the command `python src/python/main.py solver`, where solver is 0 when refering to OR-Tools and 1 when refering to DOcplex. If the argument is incorrect or omitted, the problem will be solved using the OR-Tools solver.
    b. To run the tests presented in the report, execute the command `python src/python/analyse.py`. This will run all the OR-Tools tests followed by all the DOcplex tests.
