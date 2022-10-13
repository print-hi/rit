#setwd("~\rit\R")
library(expm)
library(MASS)
library(zoo)
source("economic_AFNS.R"); source("economic_Model.R"); source("economic_VAR.R"); source("economic_summary.R")

##############
# parameters #
##############

num_years = 10
num_paths = 50
freq = "year" # please change it to "year", "quarter", or "month" 

#### test discrete-time model ####

discrete_sim = esg_var_simulator(num_years = num_years, 
                                   num_paths = num_paths, 
                                   frequency = "month", 
                                   perc_change = F, 
                                   return_sdf = T)
sum(is.na(discrete_sim)) # check if there're NA's 
colnames(discrete_sim$zcp3m_yield)[1:10] # dates 
discrete_sim$zcp3m_yield[37,1:10]
discrete_sim$GDP[37,1:10]
discrete_sim$discount_factors[37,1:10]

matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$zcp3m_yield), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$zcp10y_spread), type = "l", ylab = "", main = "10-year zero-coupon spreads", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$home_index), type = "l", ylab = "", main = "NSW home value indexes", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$rental_yield), type = "l", ylab = "", main = "NSW rental yields", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$GDP), type = "l", ylab = "", main = "Australia GDP", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$CPI), type = "l", ylab = "", main = "Australia CPI", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$ASX200), type = "l", ylab = "", main = "S&P/ASX200 prices", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$AUD), type = "l", ylab = "", main = "AUD trade indexes", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$mortgage_rate), type = "l", ylab = "", main = "Mortgage rates", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$unemployment_rate), type = "l", ylab = "", main = "NSW unemployment rates", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)),t(discrete_sim$discount_factors), type = "l", ylab = "", main = "Australia stochastic discount factors", xlab="")

discrete_summ = esg_summary(discrete_sim)
sum(is.na(discrete_summ))
colnames(discrete_summ$zcp3m_yield)
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$zcp3m_yield[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$zcp10y_spread[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "10-year zero-coupon spreads", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$home_index[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "NSW home value indexes", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$rental_yield[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "NSW rental yields", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$GDP[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "Australia GDP", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$CPI[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "Australia CPI", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$ASX200[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "S&P/ASX200 prices", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$AUD[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "AUD trade indexes", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$mortgage_rate[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "Mortgage rates", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$unemployment_rate[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "NSW unemployment rates", xlab="")
matplot(as.Date(colnames(discrete_summ$zcp3m_yield)),t(discrete_summ$discount_factors[-nrow(discrete_summ$zcp3m_yield),]), type = "l", ylab = "", main = "Australia stochastic discount factors", xlab="")


#### test discrete-time model: percentage change as outputs ####
# comments: interest rate at Q1 2020 was very low due to COVID, so the 
# percentage change at the start could blow up. 

discrete_sim = esg_var_simulator(num_years = num_years, 
                                   num_paths = num_paths, 
                                   frequency = freq, 
                                   perc_change = T, 
                                   return_sdf = T)
sum(is.na(discrete_sim)) # check if there're NA's 
colnames(discrete_sim$zcp3m_yield)
discrete_sim$zcp3m_yield[37,1:9]
discrete_sim$GDP[37,1:9]
discrete_sim$discount_factors[37,1:9]

matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$zcp3m_yield[,-1]), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$zcp10y_spread[,-1]), type = "l", ylab = "", main = "10-year zero-coupon spreads", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$home_index[,-1]), type = "l", ylab = "", main = "NSW home value indexes", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$rental_yield[,-1]), type = "l", ylab = "", main = "NSW rental yields", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$GDP[,-1]),type = "l", ylab = "", main = "Australia GDP", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$CPI[,-1]), type = "l", ylab = "", main = "Australia CPI", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$ASX200[,-1]), type = "l", ylab = "", main = "S&P/ASX200 prices", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$AUD[,-1]), type = "l", ylab = "", main = "AUD trade indexes", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$mortgage_rate[,-1]), type = "l", ylab = "", main = "Mortgage rates", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$unemployment_rate[,-1]), type = "l", ylab = "", main = "NSW unemployment rates", xlab="")
matplot(as.Date(colnames(discrete_sim$zcp3m_yield)[-1]),t(discrete_sim$discount_factors[,-1]), type = "l", ylab = "", main = "Australia stochastic discount factors", xlab="")

#### test cont-time model: independent-factor interest rate term structure ####

cts_sim = esg_afns_simulator(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = "month", 
                              type = "independent", 
                              model = "interest_rate")
sum(is.na(cts_sim)) # check if there're NA's 
colnames(cts_sim$maturity_1qtrs) # dates
cts_sim$maturity_1qtrs[46,1:10]
cts_sim$maturity_20qtrs[46,1:10]
cts_sim$maturity_40qtrs[46,1:10]

matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_1qtrs), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_2qtrs), type = "l", ylab = "", main = "6-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_4qtrs), type = "l", ylab = "", main = "1-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_20qtrs), type = "l", ylab = "", main = "5-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_40qtrs), type = "l", ylab = "", main = "10-year zero-coupon yields", xlab = "")

#### test cont-time model: correlated-factor interest rate term structure ####

cts_sim = esg_afns_simulator(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "correlated", 
                              model = "interest_rate",
                             perc_change = T)
sum(is.na(cts_sim)) # check if there're NA's 
colnames(cts_sim$maturity_1qtrs) 
cts_sim$maturity_1qtrs[46,1:10]
cts_sim$maturity_20qtrs[46,1:10]
cts_sim$maturity_40qtrs[46,1:10]

matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_1qtrs[,-1]), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_2qtrs[,-1]), type = "l", ylab = "", main = "6-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_4qtrs[,-1]), type = "l", ylab = "", main = "1-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_20qtrs[,-1]), type = "l", ylab = "", main = "5-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_40qtrs[,-1]), type = "l", ylab = "", main = "10-year zero-coupon yields", xlab = "")

cts_summ = esg_summary(cts_sim$maturity_38qtrs, na.rm = T)
matplot(t(cts_summ))

#### test cont-time model: independent-factor term structure, house index, stock price ####

cts_sim = esg_afns_simulator(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "independent", 
                              model = "interest_house_stock",
                              perc_change = T)
sum(is.na(cts_sim)) # check if there're NA's 
colnames(cts_sim$maturity_1qtrs)
cts_sim$maturity_1qtrs[46,1:10]
cts_sim$maturity_40qtrs[46,1:10]
cts_sim$house_index[46,1:10]
cts_sim$stock_price[46,1:10]

matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_1qtrs[,-1]), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_2qtrs[,-1]), type = "l", ylab = "", main = "6-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_4qtrs[,-1]), type = "l", ylab = "", main = "1-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_20qtrs[,-1]), type = "l", ylab = "", main = "5-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$maturity_40qtrs[,-1]), type = "l", ylab = "", main = "10-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$house_index[,-1]), type = "l", ylab = "", main = "NSW log house value indexes", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)[-1]),t(cts_sim$stock_price[,-1]), type = "l", ylab = "", main = "log S&P/ASX200 prices", xlab = "")

cts_summ = esg_summary(cts_sim, probs = seq(0,1,0.2))
sum(is.na(cts_summ))
colnames(cts_summ$maturity_1qtrs)
cts_summ$maturity_4qtrs

#### test cont-time model: correlated-factor term structure, house index, stock price ####

cts_sim = esg_afns_simulator(num_years = num_years, 
                              num_paths = num_paths, 
                              frequency = freq, 
                              type = "correlated", 
                              model = "interest_house_stock",
                              perc_change = F)
sum(is.na(cts_sim)) # check if there're NA's 
colnames(cts_sim$maturity_1qtrs)
cts_sim$maturity_1qtrs[46,1:10]
cts_sim$maturity_40qtrs[46,1:10]
cts_sim$house_index[46,1:10]
cts_sim$stock_price[46,1:10]

matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_1qtrs), type = "l", ylab = "", main = "3-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_2qtrs), type = "l", ylab = "", main = "6-month zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_4qtrs), type = "l", ylab = "", main = "1-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_20qtrs), type = "l", ylab = "", main = "5-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$maturity_40qtrs), type = "l", ylab = "", main = "10-year zero-coupon yields", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$house_index), type = "l", ylab = "", main = "NSW log house value indexes", xlab = "")
matplot(as.Date(colnames(cts_sim$maturity_1qtrs)),t(cts_sim$stock_price), type = "l", ylab = "", main = "log S&P/ASX200 prices", xlab = "")

cts_summ = esg_summary(cts_sim)
matplot(cts_summ$maturity_5qtrs)

# please change frequency at line 13 to "year", "quarter", or "month" 

#### test esg_summary

test_data = t(as.data.frame(matrix(1:100, nrow = 4, byrow = T)))
test_data2 = t(as.data.frame(matrix(1001:2000, nrow = 40, byrow = T)))
test_list = list(test_data, test_data2)
esg_summary(test_list)

