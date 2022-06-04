#setwd("~\rit\R")
library(expm)
library(MASS)
library(zoo)
source("economic_AFNS.R"); source("economic_Model.R"); source("economic_VAR.R");

##############
# parameters #
##############

num_years = 10
num_paths = 50
freq = "quarter" # please change it to "year", "quarter", or "month" 

#### test discrete-time model ####

discrete_sim = get_var_simulations(num_years = num_years, 
                                   num_paths = num_paths, 
                                   frequency = freq, 
                                   perc_change = F, 
                                   return_sdf = T)
sum(is.na(discrete_sim)) # check if there're NA's 
discrete_sim$zcp3m_yield$trajectory_37[1:10]
discrete_sim$GDP$trajectory_37[1:10]
discrete_sim$discount_factors$trajectory_37[1:10]

matplot(discrete_sim$zcp3m_yield, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(discrete_sim$zcp10y_spread, type = "l", ylab = "", main = "10-year zero-coupon spreads")
matplot(discrete_sim$home_index, type = "l", ylab = "", main = "NSW home value indexes")
matplot(discrete_sim$rental_yield, type = "l", ylab = "", main = "NSW rental yields")
matplot(discrete_sim$GDP, type = "l", ylab = "", main = "Australia GDP")
matplot(discrete_sim$CPI, type = "l", ylab = "", main = "Australia CPI")
matplot(discrete_sim$ASX200, type = "l", ylab = "", main = "S&P/ASX200 prices")
matplot(discrete_sim$AUD, type = "l", ylab = "", main = "AUD trade indexes")
matplot(discrete_sim$mortgage_rate, type = "l", ylab = "", main = "Mortgage rates")
matplot(discrete_sim$unemployment_rate, type = "l", ylab = "", main = "NSW unemployment rates")
matplot(discrete_sim$discount_factors, type = "l", ylab = "", main = "Australia stochastic discount factors")

#### test discrete-time model: percentage change as outputs ####

discrete_sim = get_var_simulations(num_years = num_years, 
                                   num_paths = num_paths, 
                                   frequency = freq, 
                                   perc_change = T, 
                                   return_sdf = T)
sum(is.na(discrete_sim)) # check if there're NA's 
discrete_sim$zcp3m_yield$trajectory_37[1:10]
discrete_sim$GDP$trajectory_37[1:10]
discrete_sim$discount_factors$trajectory_37[1:10]

matplot(discrete_sim$zcp3m_yield, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(discrete_sim$zcp10y_spread, type = "l", ylab = "", main = "10-year zero-coupon spreads")
matplot(discrete_sim$home_index, type = "l", ylab = "", main = "NSW home value indexes")
matplot(discrete_sim$rental_yield, type = "l", ylab = "", main = "NSW rental yields")
matplot(discrete_sim$GDP, type = "l", ylab = "", main = "Australia GDP")
matplot(discrete_sim$CPI, type = "l", ylab = "", main = "Australia CPI")
matplot(discrete_sim$ASX200, type = "l", ylab = "", main = "S&P/ASX200 prices")
matplot(discrete_sim$AUD, type = "l", ylab = "", main = "AUD trade indexes")
matplot(discrete_sim$mortgage_rate, type = "l", ylab = "", main = "Mortgage rates")
matplot(discrete_sim$unemployment_rate, type = "l", ylab = "", main = "NSW unemployment rates")
matplot(discrete_sim$discount_factors, type = "l", ylab = "", main = "Australia stochastic discount factors")

#### test cont-time model: independent-factor interest rate term structure ####

cts_sim = get_afns_simulation(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "independent", 
                              model = "interest_rate")
sum(is.na(cts_sim)) # check if there're NA's 
cts_sim$maturity_1qtrs$trajectory_46[1:10]
cts_sim$maturity_20qtrs$trajectory_46[1:10]
cts_sim$maturity_40qtrs$trajectory_46[1:10]

matplot(cts_sim$maturity_1qtrs, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(cts_sim$maturity_2qtrs, type = "l", ylab = "", main = "6-month zero-coupon yields")
matplot(cts_sim$maturity_4qtrs, type = "l", ylab = "", main = "1-year zero-coupon yields")
matplot(cts_sim$maturity_20qtrs, type = "l", ylab = "", main = "5-year zero-coupon yields")
matplot(cts_sim$maturity_40qtrs, type = "l", ylab = "", main = "10-year zero-coupon yields")

#### test cont-time model: correlated-factor interest rate term structure ####

cts_sim = get_afns_simulation(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "correlated", 
                              model = "interest_rate")
sum(is.na(cts_sim)) # check if there're NA's 
cts_sim$maturity_1qtrs$trajectory_46[1:10]
cts_sim$maturity_20qtrs$trajectory_46[1:10]
cts_sim$maturity_40qtrs$trajectory_46[1:10]

matplot(cts_sim$maturity_1qtrs, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(cts_sim$maturity_2qtrs, type = "l", ylab = "", main = "6-month zero-coupon yields")
matplot(cts_sim$maturity_4qtrs, type = "l", ylab = "", main = "1-year zero-coupon yields")
matplot(cts_sim$maturity_20qtrs, type = "l", ylab = "", main = "5-year zero-coupon yields")
matplot(cts_sim$maturity_40qtrs, type = "l", ylab = "", main = "10-year zero-coupon yields")

#### test cont-time model: independent-factor term structure, house index, stock price ####

cts_sim = get_afns_simulation(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "independent", 
                              model = "interest_house_stock")
sum(is.na(cts_sim)) # check if there're NA's 
cts_sim$maturity_1qtrs$trajectory_46[1:10]
cts_sim$maturity_40qtrs$trajectory_46[1:10]
log(cts_sim$house_index$trajectory_46[1:10])
log(cts_sim$stock_price$trajectory_46[1:10])

matplot(cts_sim$maturity_1qtrs, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(cts_sim$maturity_2qtrs, type = "l", ylab = "", main = "6-month zero-coupon yields")
matplot(cts_sim$maturity_4qtrs, type = "l", ylab = "", main = "1-year zero-coupon yields")
matplot(cts_sim$maturity_20qtrs, type = "l", ylab = "", main = "5-year zero-coupon yields")
matplot(cts_sim$maturity_40qtrs, type = "l", ylab = "", main = "10-year zero-coupon yields")
matplot(log(cts_sim$house_index), type = "l", ylab = "", main = "NSW log house value indexes")
matplot(log(cts_sim$stock_price), type = "l", ylab = "", main = "log S&P/ASX200 prices")

#### test cont-time model: correlated-factor term structure, house index, stock price ####

cts_sim = get_afns_simulation(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "correlated", 
                              model = "interest_house_stock")
sum(is.na(cts_sim)) # check if there're NA's 
cts_sim$maturity_1qtrs$trajectory_46[1:10]
cts_sim$maturity_40qtrs$trajectory_46[1:10]
log(cts_sim$house_index$trajectory_46[1:10])
log(cts_sim$stock_price$trajectory_46[1:10])

matplot(cts_sim$maturity_1qtrs, type = "l", ylab = "", main = "3-month zero-coupon yields")
matplot(cts_sim$maturity_2qtrs, type = "l", ylab = "", main = "6-month zero-coupon yields")
matplot(cts_sim$maturity_4qtrs, type = "l", ylab = "", main = "1-year zero-coupon yields")
matplot(cts_sim$maturity_20qtrs, type = "l", ylab = "", main = "5-year zero-coupon yields")
matplot(cts_sim$maturity_40qtrs, type = "l", ylab = "", main = "10-year zero-coupon yields")
matplot(log(cts_sim$house_index), type = "l", ylab = "", main = "NSW log house value indexes")
matplot(log(cts_sim$stock_price), type = "l", ylab = "", main = "log S&P/ASX200 prices")

