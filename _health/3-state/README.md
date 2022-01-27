# Three State Health Module 

## Purpose

Tshm is a package that conducts survival analysis using fitted Cox Regression models from previous
studies (and their estimated parameters). The underlying model that is fitted is a 3 state 
model with the healthy state, disabled state, and dead state. Transitions between healthy and disabled
are permitted in this model. 

Functionality includes:

* deriving transition probability matrices from parameters provided for a certain set 
of individual characteristics

* creating life table for a type of individual

* simulating lifetime paths 

* returning key statistics regarding an individuals lifetime

Uses:

* mortality studies

* application in insurance and pricing

## Overview

![flowchart](https://github.com/print-hi/retirement-toolkit/blob/healthy-state-3state/_health/3-state/inst/images/3_state_flowchart.jpeg)

## Statistic Output functions

The package has several functions to output key mortality statistics for specified individual 
characteristics. The names of the functions below are for the static and trend model. Simply add 
an 'F' to the end for frailty version of the functions (ie. afl -> aflF):

* afl: average future lifetime of individual 

* hfl: healthy future lifetime

* afld: average future lifetime of individual in disabled state

* time_to_disabled: average time at onset of disability (given the individual becomes disabled)

* survival_stats: outputs all of the above statistics

## Included studies

Currently the parameters from the following studies are included in the package:

1. US Health and Retirement Study (US_HRS)

2. China Chinese Longitudinal Healthy Longevity Survey (china_CLHLS) 


