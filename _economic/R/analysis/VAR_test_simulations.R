# Tests get_var_simulations()

all_var_names = c("zcp3m", "zcp10y", "home_index", "rental_yield", "GDP", "CPI", "ASX200", "forex") # those to be kept 
all_data_original = as.data.frame(matrix(NA, nrow = 112, ncol = 1+length(all_var_names)))
colnames(all_data_original) = c("Date", all_var_names)
start_pc_date = as.Date("1993-04-01")
start_index_date = as.Date("1993-1-01") # indexes are converted to percentages, one period short
end_date = as.Date("2021-03-31")

################################################
# ZERO-COUPON INTEREST RATES - ANALYTICAL SERIES
# import - 3 months 
zcp_old = read_excel("data/ZCP_till2008.xls")[-c(1:10), c(1,3)]
zcp_new = read_excel("data/ZCP_from2008.xls")[-c(1:10), c(1,3)]
names(zcp_old) = c("Date", "ZCP3month")
names(zcp_new) = c("Date", "ZCP3month")
zcp3m = rbind(zcp_old, zcp_new)

# import - 10 years 
zcp_old = read_excel("data/ZCP_till2008.xls")[-c(1:10), c(1,42)]
zcp_new = read_excel("data/ZCP_from2008.xls")[-c(1:10), c(1,42)]
names(zcp_old) = c("Date", "ZCP10year")
names(zcp_new) = c("Date", "ZCP10year")
zcp10y = rbind(zcp_old, zcp_new)
rm(zcp_new, zcp_old)

# cleaning
zcp3m = zcp3m %>% mutate_if(is.character, as.numeric)
zcp10y = zcp10y %>% mutate_if(is.character, as.numeric)
zcp3m$Date = as.Date(zcp3m$Date, origin = "1899-12-30")
zcp10y$Date = as.Date(zcp10y$Date, origin = "1899-12-30")

# convert 10 year CZP to spread 
zcp10y$ZCP10y_spread = zcp10y$ZCP10year - zcp3m$ZCP3month

# match the time window 
zcp3m = zcp3m %>% filter(Date <= end_date & Date >= start_pc_date)
zcp10y = zcp10y %>% filter(Date <= end_date & Date >= start_pc_date)

# match the frequency 
zcp3m = zcp3m %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(ZCP3month_qtr_rate = mean(ZCP3month)) 
zcp10y = zcp10y %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(ZCP10y_rate_qtr_rate = mean(ZCP10year), ZCP10y_spread_qtr_rate = mean(ZCP10y_spread)) 

all_data_original$Date = zcp10y$Qtr
all_data_original$zcp3m = zcp3m$ZCP3month_qtr_rate
all_data_original$zcp10y = zcp10y$ZCP10y_rate_qtr_rate

# match the unit 

# declare as a ts object 
zcp3m = ts(zcp3m$ZCP3month_qtr_rate)
zcp10y_spread = ts(zcp10y$ZCP10y_spread_qtr_rate)

###############
# Mortgage rate
lending = read_excel("data/Mortgage-RBA.xls", sheet = "Data")[-c(1:10), c(1,4)]
names(lending) = c("Date", "mortgage_rate")
lending = lending %>% mutate_if(is.character, as.numeric)
lending$Date = as.Date(lending$Date, origin = "1899-12-30")

# match the time window
lending = lending %>% filter(Date <= end_date & Date >= start_pc_date)

# match the frequency
lending = lending %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(mortgage_rate_qtr = mean(mortgage_rate))

# declare as a ts object
lending = ts(lending$mortgage_rate_qtr)

######################################
# House price dynamics & rental yields 
# import & cleaning 
properties = read.csv("data/Market_Trends_Data.csv")
properties = clean_names(properties)
properties = properties %>% mutate_if(is.character, as.factor)
properties$value_at_date = as.Date(properties$value_at_date, format =  "%d/%m/%Y")

# comparing Sydney vs NSW 
########
# sydney 
home_value_houses_syd = properties %>% 
    filter(str_detect(sa4_name16, "Sydney"), property_type == "Houses") %>% 
    group_by(value_at_date) %>% 
    summarise(index = mean(hedonic_home_value_index, na.rm = T))
rental_houses_syd = properties %>% 
    filter(str_detect(sa4_name16, "Sydney"), property_type == "Houses") %>% 
    group_by(value_at_date) %>% 
    summarise(yield = mean(hedonic_rental_yields, na.rm = T))

# match the time window 
home_value_houses_syd = home_value_houses_syd %>% 
    filter(value_at_date <= end_date & value_at_date >= start_index_date) %>% 
    mutate(Qtr = as.Date(as.yearqtr(value_at_date)))
rental_houses_syd = rental_houses_syd %>% 
    filter(value_at_date <= end_date & value_at_date >= start_pc_date) %>% 
    mutate(Qtr = as.Date(as.yearqtr(value_at_date)))

#####
# NSW 
home_value_houses_nsw = properties %>% 
    filter(state == "NSW", property_type == "Houses") %>% 
    group_by(value_at_date) %>% 
    summarise(index = mean(hedonic_home_value_index, na.rm = T))
rental_houses_nsw = properties %>% 
    filter(state == "NSW", property_type == "Houses") %>% 
    group_by(value_at_date) %>% 
    summarise(yield = mean(hedonic_rental_yields, na.rm = T))
home_value_houses_nsw = home_value_houses_nsw %>% 
    filter(value_at_date <= end_date & value_at_date >= start_index_date) %>% 
    mutate(Qtr = as.Date(as.yearqtr(value_at_date)))
rental_houses_nsw = rental_houses_nsw %>% 
    filter(value_at_date <= end_date & value_at_date >= start_pc_date) %>% 
    mutate(Qtr = as.Date(as.yearqtr(value_at_date)))

# match the frequency 
home_value_houses_nsw = home_value_houses_nsw %>% 
    group_by(Qtr) %>% 
    summarise(home_val_qtr = mean(index)) 
rental_houses_nsw = rental_houses_nsw %>% 
    group_by(Qtr) %>% 
    summarise(rental_yields_qtr_rate = mean(yield)) 

all_data_original$home_index = home_value_houses_nsw$home_val_qtr[-1]
all_data_original$rental_yield = rental_houses_nsw$rental_yields_qtr_rate

# match the unit 
home_value_houses_nsw = home_value_houses_nsw %>% mutate(home_value_qtr_rate = log(home_val_qtr) - log(dplyr::lag(home_val_qtr, n = 1))) 
home_value_houses_nsw = home_value_houses_nsw[-1,]

# declares variables as time_series objects 
home_value_houses_nsw = ts(home_value_houses_nsw$home_value_qtr_rate)
rental_houses_nsw = ts(rental_houses_nsw$rental_yields_qtr_rate)

####################################################################
# Gross domestic product: Chain volume measures; seasonally adjusted
# import 
gdp = read_excel("data/GDP-ABS.xls", sheet = "Data1")[-c(1:9), c(1,54)]
names(gdp) = c("Date", "GDP")
gdp = gdp %>% mutate_if(is.character, as.numeric)
gdp$Date = as.Date(gdp$Date, origin = "1899-12-30")

# match the time window 
gdp = gdp %>% filter(Date <= end_date & Date >= start_index_date)

# match the frequency 
gdp = gdp %>% mutate(Qtr = as.yearqtr(Date)) %>% dplyr::select(-Date)
all_data_original$GDP = gdp$GDP[-1]

# match the unit 
gdp = gdp %>% mutate(gdp_rates = log(GDP) - log(dplyr::lag(GDP, n = 1))) 
gdp = gdp[-1,]

# declare as a ts object 
gdp = ts(gdp$gdp_rates)

#####################################
# CPI: Australia; seasonally adjusted
# import 
cpi = read_excel("data/CPI-ABS.xls", sheet = "Data1")[-c(1:9), c(1,10)]
names(cpi) = c("Date", "CPI")
cpi = cpi %>% mutate_if(is.character, as.numeric)
cpi$Date = as.Date(cpi$Date, origin = "1899-12-30")

# match the time window
cpi = cpi %>% filter(Date <= end_date & Date >= start_index_date)

# match the frequency
cpi = cpi %>% mutate(Qtr = as.yearqtr(Date)) %>% dplyr::select(-Date)
all_data_original$CPI = cpi$CPI[-1]

# match the unit
cpi = cpi %>% mutate(cpi_rate = log(CPI) - log(dplyr::lag(CPI, n = 1)))
cpi = cpi[-1,]

# declare as a ts object
cpi = ts(cpi$cpi_rate)

##################################################################
# Non-financial corporate BBB bonds - yield - 10 year target tenor
# import 
corpBBB_10y_spread = read_excel("data/corporate_bonds-RBA.xls", sheet = "Data")[-c(1:10), c(1,36)]
names(corpBBB_10y_spread) = c("Date", "yield")
corpBBB_10y_spread = corpBBB_10y_spread %>% mutate_if(is.character, as.numeric)
corpBBB_10y_spread$Date = as.Date(corpBBB_10y_spread$Date, origin = "1899-12-30")

# match the time window
corpBBB_10y_spread = corpBBB_10y_spread %>% filter(Date <= end_date & Date >= start_pc_date)

# match the frequency
corpBBB_10y_spread = corpBBB_10y_spread %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(yield_qtr = mean(yield))

# match the unit
corpBBB_10y_spread$spread_qtr = corpBBB_10y_spread$yield_qtr - zcp3m

# declare as a ts object
corpBBB_10y_spread = ts(corpBBB_10y_spread$spread_qtr)

#################################
# S&P/ASX200: daily closing price
# import 
asx200_new = read_excel("data/ASX200_from2001.xlsx")[, 1:2]
asx200_new = clean_names(asx200_new)

asx200_oldest = read_excel("data/ASX200_till2001Q2.xlsx")[,c(1,6)]
names(asx200_oldest) = c("exchange_date", "close")
asx200_oldest = asx200_oldest %>% mutate_if(is.character, as.numeric)
asx200_oldest$exchange_date = as.Date(asx200_oldest$exchange_date, origin = "1899-12-30")
asx200_oldest = asx200_oldest[order(asx200_oldest$exchange_date),]

asx200_old = read_excel("data/ASX200_till2001Nov19.xlsx")[,c(1,6)]
names(asx200_old) = c("exchange_date", "close")
asx200_old = asx200_old %>% mutate_if(is.character, as.numeric)
asx200_old$exchange_date = as.Date(asx200_old$exchange_date, origin = "1899-12-30")
asx200_old = asx200_old[order(asx200_old$exchange_date),]

asx200 = rbind(asx200_oldest, asx200_old, asx200_new)

# match the time window 
asx200 = asx200 %>% filter(exchange_date <= end_date & exchange_date >= start_index_date)

# match the frequency 
asx200 = asx200 %>% mutate(Qtr = as.yearqtr(exchange_date)) %>% group_by(Qtr) %>% summarise(close_qtr_price = mean(close, na.rm = T)) 
all_data_original$ASX200 = asx200$close_qtr_price[-1]

# match the unit 
asx200 = asx200 %>% mutate(close_qtr_rate = log(close_qtr_price) - log(dplyr::lag(close_qtr_price, n = 1))) 
asx200 = asx200[-1,]

# declare as a ts object 
asx200 = ts(asx200$close_qtr_rate)

############################################
# unemployment rate NSW; seasonally adjusted
# import
unemploy_nsw = read_excel("data/Unemployment NSW-ABS.xls", sheet = "Data1")[-c(1:9), c(1, 48)]
names(unemploy_nsw) = c("Date", "Unemployment_rate")
unemploy_nsw = unemploy_nsw %>% mutate_if(is.character, as.numeric)
unemploy_nsw$Date = as.Date(unemploy_nsw$Date, origin = "1899-12-30")

# match the time window 
unemploy_nsw = unemploy_nsw %>% filter(Date <= end_date & Date >= start_pc_date)

# match the frequency 
unemploy_nsw = unemploy_nsw %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(unemploy_qtr_rate = mean(Unemployment_rate)) 
all_data_original$unemploy = unemploy_nsw$unemploy_qtr_rate

# match the unit 

# declare as a ts object 
unemploy_nsw = ts(unemploy_nsw$unemploy_qtr_rate)

###############################################
# forex: Australian Dollar Trade-weighted Index
# import 
forex1 = read_excel("data/Forex_till2010.xls")[-c(1:20), c(1,15)]
forex2 = read_excel("data/Forex_from2010.xls")[-c(1:10), c(1,3)]
names(forex1) = c("Date", "AUD_index")
names(forex2) = c("Date", "AUD_index")
forex = rbind(forex1, forex2)
forex = forex %>% mutate_if(is.character, as.numeric)
forex$Date = as.Date(forex$Date, origin = "1899-12-30")
rm(forex1, forex2)

# match the time window 
forex = forex %>% filter(Date <= end_date & Date >= start_index_date)

# match the frequency 
forex = forex %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(AUD_qtr_index = mean(AUD_index)) 
all_data_original$forex = forex$AUD_qtr_index[-1]

# match the unit 
forex = forex %>% mutate(AUD_qtr_rate = log(AUD_qtr_index) - log(dplyr::lag(AUD_qtr_index, n = 1))) 
forex = forex[-1,]

# declare as a ts object 
forex = ts(forex$AUD_qtr_rate)

all_data = cbind(zcp3m, zcp10y_spread, home_value_houses_nsw, rental_houses_nsw, gdp, asx200, forex)
##############
# stationarity
zcp3m_stationary = zcp3m - dplyr::lag(as.vector(zcp3m), n = 1)
zcp3m_stationary = ts(zcp3m_stationary[-1])
rental_houses_nsw_stationary = rental_houses_nsw - dplyr::lag(as.vector(rental_houses_nsw), n = 1)
rental_houses_nsw_stationary = ts(rental_houses_nsw_stationary[-1])
zcp10y_spread = ts(zcp10y_spread[-1])
home_value_houses_nsw = ts(home_value_houses_nsw[-1])
gdp = ts(gdp[-1])
cpi = ts(cpi[-1])
asx200 = ts(asx200[-1])
forex = ts(forex[-1])
all_data_stationary = cbind(zcp3m_stationary, zcp10y_spread, home_value_houses_nsw, rental_houses_nsw_stationary, gdp, cpi, asx200, forex)

#########################################################
# convert forecasted variables back to its original units 
# given the initial value 
# returns series starting from the obs after initial value 
zcp3m_inv = function (x, init) {
    # x: from forecast
    # init: initial value from original series 
    
    # 1 it was differenced once 
    diffinv(x, xi = init)[-1]
}
zcp10y_inv = function (x, zcp3) {
    # x: from forecast
    # init: initial yield from original series
    # zcp3: original series 
    
    # 1 spread over zcp3m 
    x + zcp3
}
home_value_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 index to growth rate 
    # 2 shortened once 
    output = rep(NA, length(x))
    for (i in 1:length(x)) {
        output[i] = init * exp(x[i])
        init = output[i]
    } 
    return (output)
}
rental_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 differenced once 
    diffinv(x, xi = init)[-1]
}
gdp_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 index to growth rate 
    # 2 shortened once 
    output = rep(NA, length(x))
    for (i in 1:length(x)) {
        output[i] = init * exp(x[i])
        init = output[i]
    } 
    return (output)
}
cpi_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 index to growth rate 
    # 2 shortened once 
    output = rep(NA, length(x))
    for (i in 1:length(x)) {
        output[i] = init * exp(x[i])
        init = output[i]
    } 
    return (output)
}
asx_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 index to growth rate 
    # 2 shortened once 
    output = rep(NA, length(x))
    for (i in 1:length(x)) {
        output[i] = init * exp(x[i])
        init = output[i]
    } 
    return (output)
}
forex_inv = function (x, init) {
    # x: from forecast
    # init: initial index from original series 
    
    # 1 index to growth rate 
    # 2 shortened once 
    output = rep(NA, length(x))
    for (i in 1:length(x)) {
        output[i] = init * exp(x[i])
        init = output[i]
    } 
    return (output)
}


##########
# forecast 
num_years = 10
num_paths = 10
source("../VAR.R")

############################
# qtrly data ###############

forecast_date_qtr = c(all_data_original$Date, as.yearqtr(seq(as.Date("2021-04-01"), by="quarter", length.out = num_years*4-1)))
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(1)
sim_paths_qtr = get_var_simulations(num_years, num_paths, "quarter", return_sdf = T)
stopCluster(cl)
sim_paths_qtr = lapply(sim_paths_qtr, function (x) {x = x[-1,]})

############
# zcp3 month
# simulations 
plot_data_forecast = c(all_data_original$zcp3m, sim_paths_qtr$zcp3m[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(-5, 10), main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$zcp3m, sim_paths_qtr$zcp3m[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(all_data_original$zcp10y, sim_paths_qtr$zcp10y[,1] *100 + sim_paths_qtr$zcp3m_yield[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(-2.5, 10), main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$zcp10y, sim_paths_qtr$zcp10y[,i] *100+ sim_paths_qtr$zcp3m[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(all_data[,2], sim_paths_qtr$zcp10y[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(-2.5, 5), main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data[,2], sim_paths_qtr$zcp10y[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(all_data_original$home_index, sim_paths_qtr$home_index[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(25, 275), main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$home_index, sim_paths_qtr$home_index[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(all_data_original$rental_yield*100, sim_paths_qtr$rental_yield[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(-2.5, 15), main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$rental_yield*100, sim_paths_qtr$rental_yield[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(all_data_original$GDP, sim_paths_qtr$GDP[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(200000, 625000), main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$GDP, sim_paths_qtr$GDP[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(all_data_original$CPI, sim_paths_qtr$CPI[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(60, 150), main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$CPI, sim_paths_qtr$CPI[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(all_data_original$ASX200, sim_paths_qtr$ASX200[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(2000, 10000), main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$ASX200, sim_paths_qtr$ASX200[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(all_data_original$forex, sim_paths_qtr$AUD[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(48, 100), main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$forex, sim_paths_qtr$AUD[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(lending, sim_paths_qtr$mortgage_rate[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(-1, 15), main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(lending, sim_paths_qtr$mortgage_rate[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# unemploy
# simulations 
plot_data_forecast = c(unemploy_nsw, sim_paths_qtr$unemployment_rate[,1]*100)
plot(forecast_date_qtr, plot_data_forecast, "l", ylim = c(2, 20), main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(unemploy_nsw, sim_paths_qtr$unemployment_rate[,i]*100)
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}


#######################################################################################################
#######################################################
# perc change 
forecast_date_qtr = c(as.yearqtr(seq(as.Date("2021-04-01"), by="quarter", length.out = num_years*4-1)))
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start.time <- Sys.time()
set.seed(1)
sim_paths_qtr = get_var_simulations(num_years, num_paths, "quarter",perc_change =T)

Sys.time() - start.time
stopCluster(cl)
sim_paths_qtr = lapply(sim_paths_qtr, function (x) {x = x[-nrow(x),]})

############
# zcp3 month
# simulations 
plot_data_forecast = c(sim_paths_qtr$zcp3m[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$zcp3m[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(sim_paths_qtr$zcp10y[,1] + sim_paths_qtr$zcp3m_yield[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$zcp10y[,i] + sim_paths_qtr$zcp3m[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(sim_paths_qtr$zcp10y[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$zcp10y[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(sim_paths_qtr$home_index[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$home_index[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(sim_paths_qtr$rental_yield[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$rental_yield[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(sim_paths_qtr$GDP[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$GDP[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(sim_paths_qtr$CPI[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$CPI[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(sim_paths_qtr$ASX200[,1])
plot(forecast_date_qtr, plot_data_forecast, "l",  main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$ASX200[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(sim_paths_qtr$AUD[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$AUD[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(sim_paths_qtr$mortgage_rate[,1])
plot(forecast_date_qtr, plot_data_forecast, "l",  main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$mortgage_rate[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}
###########
# unemploy
# simulations 
plot_data_forecast = c(sim_paths_qtr$unemployment_rate[,1])
plot(forecast_date_qtr, plot_data_forecast, "l", main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_qtr$unemployment_rate[,i])
    lines(forecast_date_qtr, plot_data_forecast, col = "grey")
}


###############
# annual data #
all_data_original_year = cbind(all_data_original, lending, unemploy_nsw)
all_data_original_year = all_data_original_year[-c(1:3,nrow(all_data_original)), -1] # start from Q1 1994
all_data_original_year = apply (all_data_original_year, 2, function (y) {colMeans(matrix(y, nrow=4))})
all_data_original_year = as.data.frame(all_data_original_year)

forecast_date_year = seq(from = as.Date("1994-01-01"), length.out = num_years + 27, by = "year")
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start.time <- Sys.time()
set.seed(1)
sim_paths_year = get_var_simulations(num_years, num_paths, "year", F)
Sys.time() - start.time
stopCluster(cl)

############
# zcp3 month
# simulations 
plot_data_forecast = c(all_data_original_year$zcp3m, sim_paths_year$zcp3m[,1]*100)
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(-5, 10), main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$zcp3m, sim_paths_year$zcp3m[,i]*100)
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(all_data_original_year$zcp10y, sim_paths_year$zcp10y[,1]*100 + sim_paths_year$zcp3m_yield[,1]*100)
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(-2.5, 10), main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$zcp10y, sim_paths_year$zcp10y[,i]*100 + sim_paths_year$zcp3m[,i]*100)
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
# plot_data_forecast = c(all_data[,2], sim_paths_year$zcp10y[,1]*100)
# plot(forecast_date_year, plot_data_forecast, "l", ylim = c(-2.5, 5), main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
# for (i in 2:(num_paths - 1)) {
#     plot_data_forecast = c(all_data[,2], sim_paths_year$zcp10y[,i]*100)
#     lines(forecast_date_year, plot_data_forecast, col = "grey")
# }
############
# home index 
# simulations 
plot_data_forecast = c(all_data_original_year$home_index, sim_paths_year$home_index[,1])
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(25, 275), main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$home_index, sim_paths_year$home_index[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(all_data_original_year$rental_yield*100, sim_paths_year$rental_yield[,1]*100)
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(-2.5, 15), main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$rental_yield*100, sim_paths_year$rental_yield[,i]*100)
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(all_data_original_year$GDP, sim_paths_year$GDP[,1])
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(200000, 625000), main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$GDP, sim_paths_year$GDP[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(all_data_original_year$CPI, sim_paths_year$CPI[,1])
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(60, 150), main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$CPI, sim_paths_year$CPI[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(all_data_original_year$ASX200, sim_paths_year$ASX200[,1])
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(2000, 10000), main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$ASX200, sim_paths_year$ASX200[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(all_data_original_year$forex, sim_paths_year$AUD[,1])
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(48, 100), main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$forex, sim_paths_year$AUD[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(all_data_original_year$lending, sim_paths_year$mortgage_rate[,1]*100)
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(-1, 15), main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$lending, sim_paths_year$mortgage_rate[,i]*100)
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# unemploy 
# simulations 
plot_data_forecast = c(all_data_original_year$unemploy_nsw, sim_paths_year$unemployment_rate[,1]*100)
plot(forecast_date_year, plot_data_forecast, "l", ylim = c(2, 20), main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_year$unemploy_nsw, sim_paths_year$unemployment_rate[,i]*100)
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}

#########################################################################################################################
# annual data############################################################################################################
all_data_original_year = cbind(all_data_original, lending, unemploy_nsw)
all_data_original_year = all_data_original_year[-c(1:3,nrow(all_data_original)), -1] # start from Q1 1994
all_data_original_year = apply (all_data_original_year, 2, function (y) {colMeans(matrix(y, nrow=4))})
all_data_original_year = as.data.frame(all_data_original_year)

forecast_date_year = seq(from = as.Date("2022-01-01"), length.out = num_years - 1, by = "year")
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start.time <- Sys.time()
set.seed(1)
sim_paths_year = get_var_simulations(num_years, num_paths, "year", T)
Sys.time() - start.time
stopCluster(cl)

############
# zcp3 month
# simulations 
plot_data_forecast = c(sim_paths_year$zcp3m[,1])
plot(forecast_date_year, plot_data_forecast, "l",  main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$zcp3m[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(sim_paths_year$zcp10y[,1] + sim_paths_year$zcp3m_yield[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$zcp10y[,i] + sim_paths_year$zcp3m[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(sim_paths_year$zcp10y[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$zcp10y[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(sim_paths_year$home_index[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$home_index[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(sim_paths_year$rental_yield[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$rental_yield[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(sim_paths_year$GDP[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$GDP[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(sim_paths_year$CPI[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$CPI[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(sim_paths_year$ASX200[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$ASX200[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(sim_paths_year$AUD[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$AUD[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(sim_paths_year$mortgage_rate[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$mortgage_rate[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}
###########
# unemploy 
# simulations 
plot_data_forecast = c(sim_paths_year$unemployment_rate[,1])
plot(forecast_date_year, plot_data_forecast, "l", main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_year$unemployment_rate[,i])
    lines(forecast_date_year, plot_data_forecast, col = "grey")
}

###################################################################################################################
# monthly frequency################################################################################################
qtr2month = function (x) {
    # transforms quarterly data to monthly data 
    qtr_data = zoo(x, as.Date(all_data_original$Date))
    time_index_month = seq(from = as.Date("1993-04-01"), to = as.Date("2021-01-01"), by = "month")
    month_data = zoo (NA, time_index_month)
    data = merge (qtr_data, month_data)
    data$month_data = na.approx(data$qtr_data, rule=12)
    return (as.vector(data$month_data))
}
all_data_original_month = cbind(all_data_original, lending, unemploy_nsw)[,-1]
all_data_original_month = apply(all_data_original_month, 2, qtr2month)
all_data_original_month = as.data.frame(all_data_original_month)

forecast_date_month = seq(from = as.Date("1993-04-01"), length.out = 9+(num_years + 27) * 12, by = "month")
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start.time <- Sys.time()
set.seed(1)
sim_paths_month = get_var_simulations(num_years, num_paths, "month")
Sys.time() - start.time
stopCluster(cl)
sim_paths_month = lapply(sim_paths_month, function (x) {x = x[-c(1,nrow(x)),]})

############
# zcp3 month
# simulations 
plot_data_forecast = c(all_data_original_month$zcp3m, sim_paths_month$zcp3m[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(-5, 10), main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$zcp3m, sim_paths_month$zcp3m[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(all_data_original_month$zcp10y, sim_paths_month$zcp10y[,1] + sim_paths_month$zcp3m_yield[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(-2.5, 10), main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$zcp10y, sim_paths_month$zcp10y[,i] + sim_paths_month$zcp3m[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(all_data[,2], sim_paths_month$zcp10y[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(-2.5, 5), main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data[,2], sim_paths_month$zcp10y[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(all_data_original_month$home_index, sim_paths_month$home_index[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(25, 275), main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$home_index, sim_paths_month$home_index[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(all_data_original_month$rental_yield*100, sim_paths_month$rental_yield[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(-2.5, 15), main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$rental_yield*100, sim_paths_month$rental_yield[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(all_data_original_month$GDP, sim_paths_month$GDP[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(200000, 625000), main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$GDP, sim_paths_month$GDP[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(all_data_original_month$CPI, sim_paths_month$CPI[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(60, 150), main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$CPI, sim_paths_month$CPI[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(all_data_original_month$ASX200, sim_paths_month$ASX200[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(2000, 10000), main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$ASX200, sim_paths_month$ASX200[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(all_data_original_month$forex, sim_paths_month$AUD[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(48, 100), main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$forex, sim_paths_month$AUD[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(all_data_original_month$lending, sim_paths_month$mortgage_rate[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(-1, 15), main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$lending, sim_paths_month$mortgage_rate[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(all_data_original_month$unemploy_nsw, sim_paths_month$unemployment_rate[,1])
plot(forecast_date_month, plot_data_forecast, "l", ylim = c(2, 20), main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original_month$unemploy_nsw, sim_paths_month$unemployment_rate[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}

###################################################################################################################
# monthly frequency################################################################################################
all_data_original_month = cbind(all_data_original, lending, unemploy_nsw)[,-1]
all_data_original_month = apply(all_data_original_month, 2, qtr2month)
all_data_original_month = as.data.frame(all_data_original_month)

forecast_date_month = seq(from = as.Date("2021-02-01"), length.out = (num_years) * 12 - 1, by = "month")
cl <- makeCluster(detectCores())
registerDoParallel(cl)

start.time <- Sys.time()
set.seed(1)
sim_paths_month = get_var_simulations(num_years, num_paths, "month", perc_change = T, return_noise = F)
Sys.time() - start.time
stopCluster(cl)
sim_paths_month = lapply(sim_paths_month, function (x) {x = x[-nrow(x),]})

############
# zcp3 month
# simulations 
plot_data_forecast = c(sim_paths_month$zcp3m[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$zcp3m[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(sim_paths_month$zcp10y[,1] + sim_paths_month$zcp3m_yield[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$zcp10y[,i] + sim_paths_month$zcp3m[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(sim_paths_month$zcp10y[,1])
plot(forecast_date_month, plot_data_forecast, "l",  main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$zcp10y[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(sim_paths_month$home_index[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$home_index[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(sim_paths_month$rental_yield[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$rental_yield[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(sim_paths_month$GDP[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$GDP[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(sim_paths_month$CPI[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$CPI[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(sim_paths_month$ASX200[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$ASX200[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(sim_paths_month$AUD[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$AUD[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(sim_paths_month$mortgage_rate[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$mortgage_rate[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(sim_paths_month$unemployment_rate[,1])
plot(forecast_date_month, plot_data_forecast, "l", main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(sim_paths_month$unemployment_rate[,i])
    lines(forecast_date_month, plot_data_forecast, col = "grey")
}