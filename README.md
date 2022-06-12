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
library(devtools)
load_all(export_all = FALSE)
```

## Documentation

The documentation is available [here](https://print-hi.github.io/toolkit-live/).

## Issues

Pricing:
- Reverse Mortgage has large outliers in some simulations.
- Variable Annuity and Pooled Annuity are not ready, priced incorrectly.

## Toolkit Modules

### Aggregate Mortality Simulation
This module will produce future cohort mortality scenarios for a specified age group for input into cash flow simulation and valuation modules. Input will be the model selected and parameter estimates derived from other available R packages (StMoMo). 
A range of aggregate mortality models will be included so users can select the preferred model:
* Lee-Carter
* Age-Period-Cohort
* Renshaw-Haberman, CBD
* Gaussian affine (2 and 3-factor models)

Output will include:
* Future cohort mortality rates and survival probability curves at future dates for a specified age cohort (starting from age 50 to age 110) 
* Initial age risk-adjusted survival probabilities using specified risk adjustment methods (with input parameters for risk adjustment) 
* Sharpe ratio
* Wang transform
* Price of risk for factor models
* Distributions of future lifetimes along (with summary statistics including expected future lifetime, standard deviation of future lifetime, impact of uncertainty in mortality rates versus expected mortality rates and survival probabilities)

---- 
### Health State Modelling 

This module will produce transition probabilities for health state and functional disability models for a specified initial age (from 50 to 65 up to age 110) and health state based on the calibrated parametrization of models. Models to include static models, improvement trends in transition rates and stochastic transition rates (latent factor). 

Models will include functional disability based on:
* Various ADL’s (2+, 3+, cognitive decline) with recovery
* Joint health status 
* Disability states based on core activities for Australian applications

Output will include:
* Probability transition matrices for different starting ages derived from transition rates, with trends
* Expected transition rates and simulated future transition rates for stochastic simulations

Models will be included to forecast the Australian prevalence of disabled populations based on functional disability models.

----
### Economic Scenario Generator
Outputs are joint simulated scenarios over specified horizons (age 50 to 110 – 60 years) for economic variables, distributions of simulated variables at differing horizons (10 years, 20 years, 50 years, 60 years). 

The R code will input the parameters for a selected model (VAR) and generate: 
* Simulated (correlated) paths for all the variables to use in cash flow modelling + computing expected values
* Valuation of cash flows in the cash flow module
* Distributions of the variables at different horizons – 5 years, 10 years, 30 years
* Distributions of values of assets in real terms (deflated by GDP, CPI) over different horizons to show the impact of real returns and uncertainty in returns on other assets (equities, cash, housing, rolling bonds).

----
### Cash Flow Simulation + Pricing
This module takes input aggregate mortality scenarios, functional disability/health status scenarios, economic scenarios to simulate cash flows on retirement products.

Retirement products include:
* Account-based pensions
* Life annuities
* Deferred life annuities
* Pooled annuities
* Long-term care insurance
* Care annuities
* Variable annuities with GMWB
* Reverse mortgages with NNEG

Risk-adjusted expected cash flows are computed, and prices are determined based on specified assumptions. Models use Australian data for Australian applications initially. The module includes government age pension and aged care based on current means-testing.

----
