# Tests get_discrete_simulations()

all_var_names = c("zcp3m", "zcp10y", "home_index", "rental_yield", "GDP", "CPI", "ASX200", "forex") # those to be kept 
all_data_original = as.data.frame(matrix(NA, nrow = 112, ncol = 1+length(all_var_names)))
colnames(all_data_original) = c("Date", all_var_names)
start_pc_date = as.Date("1993-04-01")
start_index_date = as.Date("1993-1-01") # indexes are converted to percentages, one period short
end_date = as.Date("2021-03-31")

################################################
# ZERO-COUPON INTEREST RATES - ANALYTICAL SERIES
# import - 3 months 
zcp_old = read_excel("ZCP_till2008.xls")[-c(1:10), c(1,3)]
zcp_new = read_excel("ZCP_from2008.xls")[-c(1:10), c(1,3)]
names(zcp_old) = c("Date", "ZCP3month")
names(zcp_new) = c("Date", "ZCP3month")
zcp3m = rbind(zcp_old, zcp_new)

# import - 10 years 
zcp_old = read_excel("ZCP_till2008.xls")[-c(1:10), c(1,42)]
zcp_new = read_excel("ZCP_from2008.xls")[-c(1:10), c(1,42)]
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
lending = read_excel("Mortgage-RBA.xls", sheet = "Data")[-c(1:10), c(1,4)]
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
properties = read.csv("Market_Trends_Data.csv")
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

plot_rent_syd = ggplot(rental_houses_syd) + 
    geom_boxplot(aes(Qtr, yield, group = Qtr)) + 
    theme_bw() + 
    labs(title = "Rental yield - Great Sydney Houses", ylab = "(%)") + 
    scale_x_date(breaks = "5 years", minor_breaks = "1 years", date_labels = "%Y") + 
    scale_y_continuous(limits = c(0,0.08))
plot_rent_nsw = ggplot(rental_houses_nsw) + 
    geom_boxplot(aes(Qtr, yield, group = Qtr)) + 
    theme_bw() + 
    labs(title = "Rental yield - NSW Houses", ylab = "(%)") + 
    scale_x_date(breaks = "5 years", minor_breaks = "1 years", date_labels = "%Y") + 
    scale_y_continuous(limits = c(0,0.08))
plot_house_syd = ggplot(home_value_houses_syd) + 
    geom_boxplot(aes(Qtr, index, group = Qtr)) + 
    theme_bw() + 
    labs(title = "Home value index - Great Sydney Houses", ylab = "(%)") + 
    scale_x_date(breaks = "5 years", minor_breaks = "1 years", date_labels = "%Y")
plot_house_nsw = ggplot(home_value_houses_nsw) + 
    geom_boxplot(aes(Qtr, index, group = Qtr)) +
    theme_bw() + 
    labs(title = "Home value index - NSW Houses", ylab = "(%)") + 
    scale_x_date(breaks = "5 years", minor_breaks = "1 years", date_labels = "%Y")


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
gdp = read_excel("GDP-ABS.xls", sheet = "Data1")[-c(1:9), c(1,54)]
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
cpi = read_excel("CPI-ABS.xls", sheet = "Data1")[-c(1:9), c(1,10)]
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
corpBBB_10y_spread = read_excel("corporate_bonds-RBA.xls", sheet = "Data")[-c(1:10), c(1,36)]
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
asx200_new = read_excel("ASX200_from2001.xlsx")[, 1:2]
asx200_new = clean_names(asx200_new)

asx200_oldest = read_excel("ASX200_till2001Q2.xlsx")[,c(1,6)]
names(asx200_oldest) = c("exchange_date", "close")
asx200_oldest = asx200_oldest %>% mutate_if(is.character, as.numeric)
asx200_oldest$exchange_date = as.Date(asx200_oldest$exchange_date, origin = "1899-12-30")
asx200_oldest = asx200_oldest[order(asx200_oldest$exchange_date),]

asx200_old = read_excel("ASX200_till2001Nov19.xlsx")[,c(1,6)]
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
unemploy_nsw = read_excel("Unemployment NSW-ABS.xls", sheet = "Data1")[-c(1:9), c(1, 48)]
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
forex1 = read_excel("Forex_till2010.xls")[-c(1:20), c(1,15)]
forex2 = read_excel("Forex_from2010.xls")[-c(1:10), c(1,3)]
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
num_pred = 400
num_paths = 10
forecast_date = c(all_data_original$Date[-1], as.yearqtr(seq(as.Date("2021-04-01"), by="quarter", length.out = num_pred)))
load("var_simulated_paths.RData")
sim_paths = var_simulated_paths
############
# zcp3 month
# simulations 
plot_data_forecast = c(all_data_original$zcp3m[-1], sim_paths$zcp3m[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(-5, 10), main = ("r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$zcp3m[-1], sim_paths$zcp3m[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
############
# zcp10 year 
# simulations 
plot_data_forecast = c(all_data_original$zcp10y[-1], sim_paths$zcp10y[,1] + sim_paths$zcp3m_yield[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(-2.5, 10), main = ("r^{(40)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$zcp10y[-1], sim_paths$zcp10y[,i] + sim_paths$zcp3m[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
###################
# zcp10 year spread
# simulations 
plot_data_forecast = c(all_data[-1,2], sim_paths$zcp10y[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(-2.5, 5), main = ("r^{(40)}- r^{(1)}"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data[-1,2], sim_paths$zcp10y[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
############
# home index 
# simulations 
plot_data_forecast = c(all_data_original$home_index[-1], sim_paths$home_index[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(25, 275), main = ("HVI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$home_index[-1], sim_paths$home_index[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
##############
# rental yield 
# simulations 
plot_data_forecast = c(all_data_original$rental_yield[-1], sim_paths$rental_yield[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(-0.025, 0.15), main = ("y_t"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$rental_yield[-1], sim_paths$rental_yield[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
################
# GDP  
# simulations 
plot_data_forecast = c(all_data_original$GDP[-1], sim_paths$GDP[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(200000, 625000), main = ("GDP"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$GDP[-1], sim_paths$GDP[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
#####
# CPI  
# simulations 
plot_data_forecast = c(all_data_original$CPI[-1], sim_paths$CPI[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(60, 150), main = ("CPI"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$CPI[-1], sim_paths$CPI[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
############
# S&P/ASX200
# simulations 
plot_data_forecast = c(all_data_original$ASX200[-1], sim_paths$ASX200[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(2000, 10000), main = ("ASX"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$ASX200[-1], sim_paths$ASX200[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(all_data_original$forex[-1], sim_paths$AUD[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(48, 100), main = ("AUD"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(all_data_original$forex[-1], sim_paths$AUD[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
###########
# mortgage 
# simulations 
plot_data_forecast = c(lending[-1], sim_paths$mortgage_rate[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(-1, 15), main = ("mortgage"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(lending[-1], sim_paths$mortgage_rate[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}
###########
# AUD index 
# simulations 
plot_data_forecast = c(unemploy_nsw[-1], sim_paths$unemployment_rate[,1])
plot(forecast_date, plot_data_forecast, "l", ylim = c(2, 20), main = ("UE"), ylab = "", xlab = "", col = "grey")
for (i in 2:(num_paths - 1)) {
    plot_data_forecast = c(unemploy_nsw[-1], sim_paths$unemployment_rate[,i])
    lines(forecast_date, plot_data_forecast, col = "grey")
}