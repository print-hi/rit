# Economic Scenario Module 

Generates simulated trajectories for Australian economic and financial variables, with user-specified time horizon, frequency, and number of paths. They can be used for analysing economic conditions and investment return distributions. There are two types of generators: 

* The discrete time economic scenario generator (ESG) is based on a fitted Vector Autoregression model of ten variables

+ The R function `get_var_simulation` returns the simulated trajectories for (1) Australia 3-month zero-coupon yields (in %), (2) Australia 10-year zero-coupon spread (in %), (3) New South Wales houses value index, (4) New South Wales houses rental yields, (5) Australian GDP, (6) Australian CPI, (7) S&P/ASX200 closing price, (8) Australian dollar trade-weighted index, (9) Australia mortgage rate, (10) New South Wales unemployment rate (in %). 

+ The R function `get_sdf_simulation` returns the simulated trajectories for the stochastic discount factors. 

* The continuous time ESG is based on an Arbitrage-Free Nelson Siegel model. 

+ The R function `get_afns_simulation` returns the simulated trajectories for zero-coupon bond term structure.

+ The R function `get_gafns_simulation` returns the simulated trajectories for zero-coupon bond term structure, as well as the NSW house value indexes and S&P/ASX200 closing prices. 

