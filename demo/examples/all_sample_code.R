# ---------------------------------------------------------------------------- #
# ---- Aggregate Mortality

# Generate simulated rates with 'StMoMo' using Lee Carter Model
# Install and load 'StMoMo' if necessary
AUS_StMoMo <- StMoMoData(mortality_AUS_data, series = "male")
LC <- lc(link = "logit")
AUS_Male_Ini_Data <- central2initial(AUS_StMoMo)
ages_fit <- 55:89
wxy <- genWeightMat(ages = ages_fit, years = AUS_Male_Ini_Data$years, clip = 3)
LC_fit <- fit(LC, data = AUS_Male_Ini_Data, ages.fit = ages_fit, wxt = wxy)

set.seed(1234)
n_sim <- 10
LC_sim <- simulate(LC_fit, nsim = n_sim, h = 100)

# Mortality Rate Completion with Kannisto Method
young_ages <- LC_sim$ages # 55:89
old_ages <- 90:130
ages <- c(young_ages, old_ages)

rates_hist <- mortality_AUS_data$rate$male[as.character(young_ages), ]
years_hist <- as.numeric(colnames(rates_hist))
years_sim <- LC_sim$years
years <- c(years_hist, years_sim)

kannisto_sim <- complete_old_age(rates = LC_sim$rates, ages = young_ages,
                                 old_ages = old_ages, fitted_ages = 80:89,
                                 method = "kannisto", type = "central")
kannisto_hist <- complete_old_age(rates = rates_hist, ages = young_ages,
                                  old_ages = old_ages, fitted_ages = 80:89,
                                  method = "kannisto", type = "central")

# Combine Historical and Simulated Rates
kannisto_55_period <- combine_hist_sim(rates_hist = kannisto_hist,
                                       rates_sim = kannisto_sim)

# Convert to cohort rates for cohort starting from age 55
kannisto_55 <- period2cohort(period_rates = kannisto_55_period, ages = ages)

# Create survival function
surv_55_P <- rate2survival(kannisto_55, ages, from = "central")

# Risk Neutral Probability
surv_55_Q <- survivalP2Q(surv_55_P, method = "wang", lambda = 1.5)

# Survival function summary statistics (consider cohort aged 55 in 2030)
psurv(surv_55_Q[, "2017", 1], 10)
qsurv(surv_55_Q[, "2017", 1], 0.95)
plot_surv_sim(surv_55_Q, 55, 2030)

# Convert back to 1-yr death probabilities
kannisto_55_q_Q <- survival2rate(surv_55_Q, ages)

# Calculate expected curtate future lifetime
exp_cfl_55 <- exp_cfl(qx = kannisto_55_q_Q, ages = ages)

# Expected curtate future lifetime can only be computed for
# the earlier (complete) cohorts
exp_cfl_55 <- exp_cfl_55[, as.character(1970:2043)]
plot_exp_cfl(exp_cfl_rates = exp_cfl_55, years = 1970:2043)

# ---------------------------------------------------------------------------- #
# ---- Health State: 3 State

# Transition Probability Matrix
trans_probs <- get_trans_probs('T', US_HRS, 65, 0, 2021)

# Life Table Generation
life_table <- create_life_table(trans_probs, 65, 0)

# Simulating Life Paths
SP <- simulate_path(65, 0, trans_probs)

# Statistics
afl(65, init_state = 0, trans_probs)
aflF(65, init_state = 0, female = 0, 2022, US_HRS)
hfl(65, init_state = 0, trans_probs)
hflF(65, init_state = 0, female = 0, 2022, US_HRS)
afld(65, init_state = 0, trans_probs)
afldF(65, init_state = 0, female = 0, 2022, US_HRS)
time_to_disabled(65, trans_probs)
time_to_disabledF(65, female = 0, 2022, US_HRS)
survival_stats(65, init_state = 0, trans_probs)
survival_statsF(65, init_state = 0, female = 0, 2022, US_HRS)


# ---------------------------------------------------------------------------- #
# ---- Health State: 5 State

library(devtools)
load_all(export_all=FALSE)

# read data
# params_5_static <- US_HRS_5[1:3,3:ncol(US_HRS_5)] # static model
# params_5_trend <- US_HRS_5[4:7,3:ncol(US_HRS_5)] # trend model
# params_5_frailty <- US_HRS_5[8:12,3:ncol(US_HRS_5)] # frailty model

# select the parameter and model
# model=1 no frailty model
# model=2 no frailty model with trend
# model=3 frailty model
params=US_HRS_5
model='F'

# input characteristics of the individual at time 0
init_age=65
gender=0
i=8 # wave index
latent=0 # initial value of latent factor

##### test frailty model #####

# Transition Rates
transition_rates=health5_get_trans_rates(model='F', params, init_age, gender, i, latent)

# Transition Probability Matrix
transition_probabilities=health5_get_trans_probs(model='F',params, init_age, gender, i, latent)

# Full list of Transition Probability Matrices from Age 65 to 110
trans_prob_matrix_age65to110=health5_get_list_trans_prob_matrix(model='F',params, init_age=65, gender, i)

# Life Table Generation Using Frailty Model
simulated_lifetable=health5_get_life_table(model='F',params,init_age=65,gender,i,latent,initial_state=0, n_sim=100) #model='F in this case because this function is for simulating the latent factor, otherwise it will produce 100 same simulations, use n_sim=1 for model='S' or model='T'

# Simulating Life Paths Given Initially in H State
simulated_individual_path=health5_simulate_individual_path(model='F',init_age=65, init_state=0, params, gender, i, cohort = 10000)

# Statistics
## First Time Leaving The H State
first_time_H=health5_first_time_stats(simulated_individual_path, 0)

### Statistics of First Time Leaving The H State
stats_first_time_H=health5_stats_produce(first_time_H)
stats_first_time_H

## Total Time Alive Given Initially in H State
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)

## Statistics of Total Time Alive Given Initially in H State
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive

## Total Time Spend in H M D MD States Respectively
total_time_H=health5_total_time_stats(simulated_individual_path, 0)
total_time_M=health5_total_time_stats(simulated_individual_path, 1)
total_time_D=health5_total_time_stats(simulated_individual_path, 2)
total_time_MD=health5_total_time_stats(simulated_individual_path, 3)

### Statistics of Total Time in Different States Given Initially in H State
#### H
stats_total_time_H=health5_stats_produce(total_time_H)
stats_total_time_H
#### M
stats_total_time_M=health5_stats_produce(total_time_M)
stats_total_time_M
#### D
stats_total_time_D=health5_stats_produce(total_time_D)
stats_total_time_D
#### MD
stats_total_time_MD=health5_stats_produce(total_time_MD)
stats_total_time_MD

## Calculate Total Time Alive Given Initially in M State
### Simulate the Life Paths, init_state=1 Indicating Initial State is M
simulated_individual_path=health5_simulate_individual_path(model='F',init_age=65, init_state=1, params, gender, i, cohort = 10000)

### Total Time Alive Given Initially in M State
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive

# ---------------------------------------------------------------------------- #
# ---- Economic Scenario Generator

# Discrete Generator
sim <- get_var_simulations(num_years = 10, num_paths = 100, frequency = 'year')
sim$zcp3m_yield
sim$zcp3m_yield$trajectory_83

# Continuous Generator
sim <- get_afns_simulation(num_years = 10, num_paths = 100, frequency = 'year', model = "interest_house_stock")
sim$maturity_40qtrs$trajectory_83
sim$house_index$trajectory_83
sim$stock_price$trajectory_83

# ---------------------------------------------------------------------------- #
# ---- Policy Valuation

# Creating Policy Object
ap <- create_policy_AP(1000000, 50000)
# Simulating Cashflows
cf <- simulate_cf(policy = ap, age = 65, sex = "M", n = 1000)
# Pricing / Valuation
v <- value_policy(ap, cf)

#### Other policies
# Account-Based Pension
ap <- create_policy_AP(1000000, 50000)
# Life Annuity
la <- create_policy_LA(60000, 5, 0, 0.01)
# Pooled Annuity
pa <- create_policy_PA(60000, 10000, 0.05, 0.01)
# Care Annuity
ca <- create_policy_CA(c(60000, 1200), c(0, 0.04), c(5, 0), c(0.04, 0.01))
# Variable Annuity (GMWB)
va <- create_policy_VA(1000000, 30, 0.1, 0.01)
# Reverse Mortgage
rm <- create_policy_RM(600000, 0.64, 0.01, 0.04)


