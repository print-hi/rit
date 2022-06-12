# Retirement Income Toolkit

Toolkit of R modules for researchers to use in modelling retirement and age care risks, simulating cash flows for a range of retirement and long-term care products including Australian government support for Age Pensions and Aged Care based on means-testing, pricing and quantifying risk for a range of finance and insurance products to finance retirement and aged care risks.

## Installation

1. Clone repository or download ZIP 
2. In RStudio:
```
File > Open project > /rit-main
```
4. In RStudio Console: 
```
install.packages("devtools")
library("devtools")
load_all(export_all = FALSE)
```

## Documentation

The documentation is available [here](https://print-hi.github.io/toolkit-live/).

## Issues

Pricing is functional, but outputs for following products need further testing: Account Based Pension, Reverse Mortgage, Variable Annuity, Pooled Annuity

- Account Based Pension shows trends in convergence plot.
- Reverse Mortgage has large outliers in some simulations.
- Variable Annuity and Pooled Annuity are priced incorrectly.
