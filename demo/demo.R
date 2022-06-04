# ---------------------------------------------------------------------------- #
# ---- Aggregate Mortality

# Mortality Rate Completion
ages <- 30:90
old_ages <- 91:130
fitted_ages <- 76:90

completed_rates <- coale_kisker(mortality_rates, ages, old_ages)
completed_qx <- denuit_goderniaux(mortality_rates, ages, old_ages)
completed_rates <- kannisto(mortality_rates, ages, old_ages, fitted_ages)

# Rate Changing
new_ages <- 30:130
def_new_ages <- 50:130

surv_func <- rate2survival(completed_rates, new_ages, from = 'central')
mortality_rates <- survival2rate(surv_func, def_new_ages, to = 'prob')

# Risk Neutral Probability
rn_surv_prob <- survivalP2Q(completed_qx, 'wang', 0.34)

# Summary Statistics
psurv(surv_func, 10)
qsurv(surv_func, 0.95)
plot_surv_sim(surv_func, 50, 2022)

ecfl_historical <- exp_cfl(q_hist, ages)
ecfl_sim <- exp_cfl(q_sim, ages)
plot_exp_cfl(ecfl_historical, years_hist, ecfl_sim, years_sim)

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

# Transition Probability Matrix
trans_probs <- transition_rate_5(params, age=65, gender=0, i, model=2)

# Life Table Generation
life_table <- simulate_life_table(params,init_age=65,gender,i,latent, initial_state=0, n_sim=100, model=3)

# Simulating Life Paths
simulated_path <- simulate_individual_path(65, 0, params, 0, 19, model = 2)

# Statistics
time_to_1 <- first_time_stats(simulated_path, 1)
print(mean(time_to_1, na.rm = TRUE))

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
va <- create_policy_VA(100000, 40, 0.4, 0.02)
