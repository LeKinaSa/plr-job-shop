
import json

PRODUCTION_TIME = 'production_time'
PRODUCTION_LINE = 'production_line'
CAPACITY        = 'capacity'
MODEL_TOTALS    = 'total'

def get_data(production_time = PRODUCTION_TIME,
             production_line = PRODUCTION_LINE,
             capacity        = CAPACITY,
             model_totals    = MODEL_TOTALS):

    with open('data/models.json', 'r') as file:
        models_raw = json.load(file)
    with open('data/lines.json', 'r') as file:
        lines_raw = json.load(file)

    if PRODUCTION_TIME == production_time and \
       PRODUCTION_LINE == production_line and \
       CAPACITY        == capacity        and \
       MODEL_TOTALS    == model_totals:
           return (models_raw, lines_raw)
    
    models = {}
    for model, description in models_raw:
        models[model] = {
            production_time : description[PRODUCTION_TIME],
            production_line : description[PRODUCTION_LINE],
            model_totals    : description[ MODEL_TOTALS  ]
        }

    lines = {}
    for line, description in lines_raw:
        lines[line] = {
            capacity        : description[   CAPACITY    ]
        }
    
    return (models, lines)

if __name__ == '__main__':    
    # Constants
    PRODUCTION_TIME = 'production_time'
    PRODUCTION_LINE = 'production_line'
    CAPACITY        = 'capacity'
    TOTAL           = 'total'
    
    # Get Data
    (models, lines) = get_data()
    
    # Test
    a = sum(model[PRODUCTION_TIME]*model[TOTAL] for model in models.values())
    print(a)
