library(doParallel)

############
# simulation 
start.time <- Sys.time()

cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(1)
var_simulated_paths = get_discrete_simulations(100, 10000, "year")
stopCluster(cl)
Sys.time() - start.time

##############
# save objects 
save(var_simulated_paths, file = "generated\\var_simulated_paths.RData")
# 3-month zero-coupon yield
var_simulated_zcp3m_yield = as.data.frame(t(var_simulated_paths$zcp3m_yield))
write.csv(var_simulated_zcp3m_yield, file = "generated\\var_simulated_zcp3m_yield.csv")

# 10-year zero-coupon spread
var_simulated_zcp10y_spread = as.data.frame(t(var_simulated_paths$zcp10y_spread))
write.csv(var_simulated_zcp10y_spread, "generated\\var_simulated_zcp10y_spread.csv")

# NSW home index 
var_simulated_home_index = as.data.frame(t(var_simulated_paths$home_index))
write.csv(var_simulated_home_index, file = "generated\\var_simulated_home_index.csv")

# NSW rental yields 
var_simulated_rental_yield = as.data.frame(t(var_simulated_paths$rental_yield))
write.csv(var_simulated_rental_yield, "generated\\var_simulated_rental_yield.csv")

# GDP
var_simulated_gdp = as.data.frame(t(var_simulated_paths$GDP))
write.csv(var_simulated_gdp, "generated\\var_simulated_gdp.csv")

# CPI 
var_simulated_cpi = as.data.frame(t(var_simulated_paths$CPI))
write.csv(var_simulated_cpi, "generated\\var_simulated_cpi.csv")

# Closing price S&P/ASX200
var_simulated_asx200 = as.data.frame(t(var_simulated_paths$ASX200))
write.csv(var_simulated_asx200, "generated\\var_simulated_asx200.csv")

# AUD trade weighted index 
var_simulated_aud_index = as.data.frame(t(var_simulated_paths$AUD))
write.csv(var_simulated_aud_index, "generated\\var_simulated_aud_index.csv")

# mortgage rate 
var_simulated_mortgage_rate = as.data.frame(t(var_simulated_paths$mortgage_rate))
write.csv(var_simulated_mortgage_rate, "generated\\var_simulated_mortgage_rate.csv")

# NSW unemployment rate 
var_simulated_unemployment_rate = as.data.frame(t(var_simulated_paths$unemployment_rate))
write.csv(var_simulated_unemployment_rate, "generated\\var_simulated_unemployment_rate.csv")