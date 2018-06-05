Formulation
================

**Let:**

*x*<sub>1</sub>, *x*<sub>2</sub>...*x*<sub>*m*</sub>= amount (in grams) of each food item in the USDA rolled-up list

*α*<sub>*i**j*</sub>= amount (in grams) of nutrient *j* in food item *i*

*L*<sub>*j*</sub>= lower nutritional bound for all nutrients *j* = 1...32

*U*<sub>*j*</sub>= upper nutritional bound for all nutrients *j* = 1...32

**Linear Programming Model:**

*For low-carb diet:*

minimize: $z = \\sum\_{i=1}^{i = m}x\_{ij\_1}$ where *j*<sub>1</sub> = carbohydrates

*For low-carb, low-sodium, low-cholesterol diet:*

minimize: $z = \\sum\_{i=1}^{i = m}x\_{ij\_1} + x\_{ij\_2} + x\_{ij\_3}$ where *j*<sub>1</sub> = carbohydrates, *j*<sub>2</sub> = sodium, *j*<sub>3</sub> = cholesterol

**Subject to:**

*x*<sub>1</sub>, *x*<sub>2</sub>...*x*<sub>*m*</sub> ≥ 0 non-negativity constraint

$\\sum\_{i=1}^{i = m}{\\alpha}\_{ij} x\_{ij} \\geq L\_{ij}$, *j* = 1...32 sum of nutrient j for all foods i must meet the minimum nutritional requirement *L*

$\\sum\_{i=1}^{i = m}{\\alpha}\_{ij} x\_{ij} \\leq U\_{ij}$, *j* = 1...32 sum of nutrient j for all foods i must not exceed the maximum nutritional requirement *U*
