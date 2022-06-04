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
SP <- simulate_paths(65, 0, trans_probs)

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

# Transition Probability Matrix
trans_probs <- get_full_trans_prob_matrixfunction(params, init_age=65, gender=0, i, model=2)

# Life Table Generation
life_table <- simulate_life_table(params,init_age=65,gender,i,latent, initial_state=0, n_sim=100, model=3)

# Simulating Life Paths
simulated_path <- function(init_age=65, init_state=0, params, gender=0, i, cohort = 10000, model=3)

# Statistics
simulated_path <- simulate_individual_path(65, 0, params, 0, 19, model = 2)
time_to_1 <- first_time_stats(simulated_path, 1)
print(mean(time_to_1, na.rm = TRUE))

# ---------------------------------------------------------------------------- #
# ---- Economic Scenario Generator

# Discrete Generator
sim <- get_var_simulations(num_years = 10, num_paths = 10000, frequency = 'year')
sim$zcp3m_yield
sim$zcp3m_yield$trajectory_103

# Continuous Generator
sim <- get_zcp_simulation(num_years = 10, num_paths = 10000, frequency = 'year')

# ---------------------------------------------------------------------------- #
# ---- Policy Valuation

# Creating Policy Object
ap <- create_policy_AP(400000, 60000)

# Simulating Cashflows
cf <- simulate_cf(policy = ap, age = 65, sex = "M", n = 1000)

# Pricing / Valuation
v <- value_policy(ap, cf)

# Other policies
rm <- create_policy_RM(100000, 0.4, 0.01, 0.05)
la <- create_policy_LA(60000, 5, 0.04, 0.05)
pa <- create_policy_PA(60000, 1000, 0.04, 0.05)
ca <- create_policy_CA(c(60000, 1200), c(0, 0.04), c(8, 0), c(0.04, 0.05))
va <- create_policy_VA(100000, 40, 0.4, 0.02, 0.02)
