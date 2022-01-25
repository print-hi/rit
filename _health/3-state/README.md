# Three State Health Module 

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


![alt text](https://github.com/print-hi/retirement-toolkit/tree/healthy-state-3state/_health/3-state/inst/images/3_state_flowchart.png?raw=true)
