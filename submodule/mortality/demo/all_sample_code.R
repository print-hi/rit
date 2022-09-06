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

