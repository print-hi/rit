# Retirement Income Toolkit

Toolkit of R modules for researchers to use in modelling retirement and age care risks, simulating cash flows for a range of retirement and long-term care products including Australian government support for Age Pensions and Aged Care based on means-testing, pricing and quantifying risk for a range of finance and insurance products to finance retirement and aged care risks.

## Installation

1. Clone repository via command line or download ZIP 
2. In RStudio:
```
File > Open project > {path-to-repo}/rit.Rproj
```
4. In RStudio Console: 
```
install.packages("devtools")
library("devtools")
load_all(export_all = FALSE)
```

## Documentation

The documentation is available [here](https://print-hi.github.io/toolkit-live/).

## ToDo

- All: Implement seed parameter to allow for better control over simulations
- Pricing: Provide algorithm/details for each product in documentation
