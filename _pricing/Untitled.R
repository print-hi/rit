---
  title: "Retirement Income Toolkit - Economic Scenario - VAR"
author: "Queenie Huang"
output:
  bookdown::html_document2:
  toc: yes
toc_depth: '3'
number_sections: yes
fig_caption: yes
code_folding: hide
urlcolor: blue
header-includes: |    
  \usepackage{amsmath,amsthm,amssymb,amsfonts, mathtools, empheq, bbm}
\newcommand{\vect}[1]{\boldsymbol{#1}}
  ---
    
    <style type="text/css">
      
      body{ /* Normal  */
          font-size: 15px;
      }
    td {  /* Table  */
        font-size: 15px;
    }
    h1.title {
      font-size: 30px;
    }
    h1 { /* Header 1 */
        font-size: 26px;
      color: DarkBlue;
    }
    h2 { /* Header 2 */
        font-size: 22px;
      color: DarkBlue;
    }
    h3 { /* Header 3 */
        font-size: 18px;
      color: DarkBlue;
    }
    code.r{ /* Code block */
        font-size: 15px;
    }
    pre { /* Code block - determines code spacing between lines */
        font-size: 15px;
    }
    </style>
      
      ```{r setup, include=FALSE}
    knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
    options(scipen = 1, digits = 6) # use 2 d.p. in output
    ```
    
    ```{r}
    library(tidyverse)
    library(readxl)
    library(janitor)
    library(lubridate)
    library(vars)
    library(tseries)
    library(corrplot)
    library(kableExtra)
    library(aTSA)
    ```
    
    A VAR(p) process can be represented by \begin{align*}
    \vect Y_t = \vect \alpha + A_1 \vect Y_{t-1} + \ldots + A_p \vect Y_{t-p} + \vect \epsilon, 
    \end{align*} where $n$ is the number of economic/financial variables, $p$ is the lag, $\vect \epsilon$ is a white noise process, $A_1, \ldots, A_p$ are fixed coefficient matrices. 
    
    # Economic/financial variables 
    
    ```{r, message=FALSE, warning=FALSE}
    variables = read_excel("Variables.xlsx")
    
    all_var_names = c("3-month zero-coupon yield", "10-year zero-coupon spread", "Hedonic home value growth rate - Sydney houses", "Hedonic rental yields - Sydney houses", "GDP per capita growth rate", "S&P/ASX200 growth rate", "Unemployment rate - NSW", "AUD trade-weighted index growth rate")
    
    kbl(variables) %>% kable_styling(latex_options = c("striped", "scale_down"))
    ```
    
    # Assess data quality & cleaning 
    
    ## Data quality
    
    * **Variable selection/transformation.**  
      
      + `10-year zero-coupon spread` = `10-year zero-coupon yield` - `3-month zero-coupon yield`. 
    
    + `Hedonic home value index` and `Hedonic rental yields` are medians of Sydney (SA4 regions) houses (excluding units). 
    
    * **Time window.**  Q1 2002 - Q1 2021 (77 quarters). <span style="color:blue">PROBLEM: Building a VAR of all series simultaneously may have limited power due to the lack of data. IDEA: we can do a cascade structure, see Granger causality tests (ref: Melbourne SUPA methodology). </span>
      
      * **Time frequency.**  Converted to quarterly frequency by averaging. 
    
    * **Units.**  Quarterly indexes are converted to quarterly rates. 
    
    + `Hedonic home value growth rate`$_t$ = log(`Hedonic home value rate`$_t$) - log(`Hedonic home value rate`$_{t-1}$). 
    
    + `GDP per capita growth rate`$_t$ = log(`GDP`$_t$) - log(`GDP`$_{t-1}$). 
    
    + `S&P/ASX200 growth rate`$_t$ = log(`S&P/ASX200 price`$_t$) - log(`S&P/ASX200 price`$_{t-1}$). 
    
    + `AUD trade index growth rate`$_t$ = log(`AUD index`$_t$) - log(`AUD index`$_{t-1}$). 
    
    
    ```{r, message=FALSE, warning=FALSE}
    start_pc_date = as.Date("2002-01-01")
    start_index_date = as.Date("2001-10-01") # indexes are converted to percentages, one period short
    end_date = as.Date("2021-03-31")
    
    ################################################
    # ZERO-COUPON INTEREST RATES – ANALYTICAL SERIES
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
    zcp10y = zcp10y %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(ZCP10y_spread_qtr_rate = mean(ZCP10y_spread)) 
    
    # match the unit 
    
    # declare as a ts object 
    zcp3m = ts(zcp3m$ZCP3month_qtr_rate)
    zcp10y_spread = ts(zcp10y$ZCP10y_spread_qtr_rate)
    
    
    ######################################
    # House price dynamics & rental yields 
    # import & cleaning 
    properties = read.csv("Market_Trends_Data.csv")
    properties = clean_names(properties)
    properties = properties %>% mutate_if(is.character, as.factor)
    properties$value_at_date = as.Date(properties$value_at_date, format =  "%d/%m/%Y")
    
    # choose Sydney 
    # home_value_houses_syd1 = properties %>% filter(postcode == 2000, property_type == "Houses") %>% dplyr::select(value_at_date, hedonic_home_value_index)
    # rental_houses_syd1 = properties %>% filter(postcode == 2000, property_type == "Houses") %>% dplyr::select(value_at_date, hedonic_rental_yields)
    home_value_houses_syd = properties %>% filter(str_detect(sa4_name16, "Sydney"), property_type == "Houses") %>% group_by(value_at_date) %>% summarise(index = median(hedonic_home_value_index, na.rm = T))
    rental_houses_syd = properties %>% filter(str_detect(sa4_name16, "Sydney"), property_type == "Houses") %>% group_by(value_at_date) %>% summarise(yield = median(hedonic_rental_yields, na.rm = T))
    
    # match the time window 
    home_value_houses_syd = home_value_houses_syd %>% filter(value_at_date <= end_date & value_at_date >= start_index_date)
    rental_houses_syd = rental_houses_syd %>% filter(value_at_date <= end_date & value_at_date >= start_pc_date)
    
    # match the frequency 
    home_value_houses_syd = home_value_houses_syd %>% mutate(Qtr = as.yearqtr(value_at_date)) %>% group_by(Qtr) %>% summarise(home_val_qtr = mean(index)) 
    rental_houses_syd = rental_houses_syd %>% mutate(Qtr = as.yearqtr(value_at_date)) %>% group_by(Qtr) %>% summarise(rental_yields_qtr_rate = mean(yield)) 
    
    # match the unit 
    home_value_houses_syd = home_value_houses_syd %>% mutate(home_value_qtr_rate = log(home_val_qtr) - log(dplyr::lag(home_val_qtr, n = 1))) 
    home_value_houses_syd = home_value_houses_syd[-1,]
    
    # declares variables as time_series objects 
    home_value_houses_syd = ts(home_value_houses_syd$home_value_qtr_rate)
    rental_houses_syd = ts(rental_houses_syd$rental_yields_qtr_rate)
    
    ###############################################################################
    # Gross domestic product per capita: Chain volume measures; seasonally adjusted
    # import 
    gdp_capita = read_excel("GDP-ABS.xls", sheet = "Data1")[-c(1:9), c(1,55)]
    names(gdp_capita) = c("Date", "GDP_per_capita")
    gdp_capita = gdp_capita %>% mutate_if(is.character, as.numeric)
    gdp_capita$Date = as.Date(gdp_capita$Date, origin = "1899-12-30")
    
    # match the time window 
    gdp_capita = gdp_capita %>% filter(Date <= end_date & Date >= start_index_date)
    
    # match the frequency 
    gdp_capita = gdp_capita %>% mutate(Qtr = as.yearqtr(Date)) %>% dplyr::select(-Date)
    
    # match the unit 
    gdp_capita = gdp_capita %>% mutate(gdp_capita_rates = log(GDP_per_capita) - log(dplyr::lag(GDP_per_capita, n = 1))) 
    gdp_capita = gdp_capita[-1,]
    
    # declare as a ts object 
    gdp_capita = ts(gdp_capita$gdp_capita_rates)
    
    ##################################
    # CPI: sydney; seasonally adjusted
    # import 
    # cpi_syd = read_excel("CPI-ABS.xls", sheet = "Data1")[-c(1:9), 1:2]
    # names(cpi_syd) = c("Date", "CPI")
    # cpi_syd = cpi_syd %>% mutate_if(is.character, as.numeric)
    # cpi_syd$Date = as.Date(cpi_syd$Date, origin = "1899-12-30")
    # 
    # # match the time window 
    # cpi_syd = cpi_syd %>% filter(Date <= end_date & Date >= start_index_date)
    # 
    # # match the frequency 
    # cpi_syd = cpi_syd %>% mutate(Qtr = as.yearqtr(Date)) %>% dplyr::select(-Date)
    # 
    # # match the unit 
    # cpi_syd = cpi_syd %>% mutate(cpi_rate = log(CPI) - log(dplyr::lag(CPI, n = 1))) 
    # cpi_syd = cpi_syd[-1,]
    # 
    # # declare as a ts object 
    # cpi_syd = ts(cpi_syd$cpi_rate)
    
    ##################################################################
    # Non-financial corporate BBB bonds – yield – 10 year target tenor
    # import 
    # corpBBB_10y_spread = read_excel("corporate_bonds-RBA.xls", sheet = "Data")[-c(1:10), c(1,36)]
    # names(corpBBB_10y_spread) = c("Date", "yield")
    # corpBBB_10y_spread = corpBBB_10y_spread %>% mutate_if(is.character, as.numeric)
    # corpBBB_10y_spread$Date = as.Date(corpBBB_10y_spread$Date, origin = "1899-12-30")
    # 
    # # match the time window
    # corpBBB_10y_spread = corpBBB_10y_spread %>% filter(Date <= end_date & Date >= start_pc_date)
    # 
    # # match the frequency
    # corpBBB_10y_spread = corpBBB_10y_spread %>% mutate(Qtr = as.yearqtr(Date)) %>% group_by(Qtr) %>% summarise(yield_qtr = mean(yield))
    # 
    # # match the unit
    # corpBBB_10y_spread$spread_qtr = corpBBB_10y_spread$yield_qtr - zcp3m
    # 
    # # declare as a ts object
    # corpBBB_10y_spread = ts(corpBBB_10y_spread$spread_qtr)
    
    #################################
    # S&P/ASX200: daily closing price
    # import 
    asx200 = read_excel("S&P_ASX200.xlsx")[, 1:2]
    asx200 = clean_names(asx200)
    
    # match the time window 
    asx200 = asx200 %>% filter(exchange_date <= end_date & exchange_date >= start_index_date)
    
    # match the frequency 
    asx200 = asx200 %>% mutate(Qtr = as.yearqtr(exchange_date)) %>% group_by(Qtr) %>% summarise(close_qtr_price = mean(close)) 
    
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
    
    # match the unit 
    forex = forex %>% mutate(AUD_qtr_rate = log(AUD_qtr_index) - log(dplyr::lag(AUD_qtr_index, n = 1))) 
    forex = forex[-1,]
    
    # declare as a ts object 
    forex = ts(forex$AUD_qtr_rate)
    ```
    
    ## Analyse the time series characteristics 
    
    ### Raw data plots 
    
    ```{r, message=FALSE, warning=FALSE}
    ##################
    # individual plots 
    par(mfrow = c(3,3))
    plot(zcp10y$Qtr, zcp3m, "l", ylab = "yield", xlab = "", main = "3-month zero-coupon yields")
    plot(zcp10y$Qtr, zcp10y_spread, "l", ylab = "spread", xlab = "", main = "10-year zero-coupon spread")
    plot(zcp10y$Qtr, home_value_houses_syd, "l", ylab = "growth rate", xlab = "", main = "Hedonic home value growth rate \n Sydney houses")
    plot(zcp10y$Qtr, rental_houses_syd, "l", ylab = "yield", xlab = "", main = "Hedonic rental yields \n Sydney houses")
    plot(zcp10y$Qtr, gdp_capita, "l", ylab = "yield", xlab = "", main = "GDP per capita")
    # plot(zcp10y$Qtr, corpBBB_10y_spread, "l")
    plot(zcp10y$Qtr, asx200, "l", ylab = "growth rate", xlab = "", main = "S&P/ASX200 growth rates")
    plot(zcp10y$Qtr, unemploy_nsw, "l", ylab = "rate", xlab = "", main = "NSW unemployment rates")
    plot(zcp10y$Qtr, forex, "l", ylab = "growth rate", xlab = "", main = "AUD trade-weighted index growth")
    
    ################
    # combined plots 
    # 10-year bond spreads 
    # par(mfrow = c(1,2))
    # plot(zcp10y$Qtr, zcp10y_spread, "l", main = "10-year ZCP vs BBB corporate bond spreads", ylim = c(min(zcp10y_spread), max(corpBBB_10y_spread)), ylab = "")
    # lines(zcp10y$Qtr, corpBBB_10y_spread, col = 2)
    # legend("topright", legend = c("ZCP", "BBB bond"), lty = 1, col = 1:2)
    # 
    # # GDP per capita vs CPI 
    # plot(zcp10y$Qtr, gdp_capita, "l", main = "GDP per capita vs Sydney CPI", ylab = "")
    # lines(zcp10y$Qtr, cpi_syd, col = 2)
    # legend("bottomleft", legend = c("GDP per capita", "CPI"), lty = 1, col = 1:2)
    ```
    
    * Variables that are highly correlated: 
      
      + `GDP per capita` vs `Sydney CPI` ($\rho$ = 0.704). CPI was removed. 
    
    + `10-year zero-coupon spread` vs `10-year BBB-rated corporate bond spread` ($\rho$ = 0.621). Corporate bond was removed. 
    
    + `10-year zero-coupon spread` vs `3-month zero-coupon yield` ($\rho$ = -0.56). Both kept. 
    
    + `10-year zero-coupon spread` vs `NSW unemployment rate` ($\rho$ = 0.51). Both kept. 
    
    + `S&P/ASX200 growth` vs `AUD trade index growth` ($\rho$ = 0.51). Both kept.  
    
    * PCA (first and second principal components)
    
    + `GDP per capita`, `S&P/ASX200`, `AUD trade index growth` are similar. 
    
    + `NSW unemployment rate`, `Sydney home value index growth`, `10-year zero-coupon spread` are similar. 
    
    + 6 out of 8 variables explain 90% of variation. 
    
    ### Correlation 
    
    ```{r, message=FALSE, warning=FALSE,fig.width = 5, fig.height = 4, fig.align = "center"}
    ######
    # corr 
    par(mfrow = c(1,1))
    all_data = cbind(zcp3m, zcp10y_spread, home_value_houses_syd, rental_houses_syd, gdp_capita, asx200, unemploy_nsw, forex)
    corrplot(cor(all_data, method = "pearson"), tl.col = "black", diag = F, type = "upper", tl.srt = 90)
    ```
    
    ### Principal Component Analysis 
    
    ```{r, message=FALSE, warning=FALSE, fig.align = "center"}
    set.seed(1)
    all_pca = prcomp(all_data, scale = T)
    
    par(mfrow = c(1,2))
    biplot(all_pca)
    
    # proportion of variance explained 
    pve = (all_pca$sdev ^ 2 / sum(all_pca$sdev ^ 2))
    plot(cumsum(pve), xlab = "#Principal Components", main = "Proportion of Variance Explained", ylab = "Cumulative %", ylim = c(0,1), type = "b")
    ```
    
    # Stationarity for individual variables
    
    ## Statistical tests for stationarity 
    
    * Augmented Dickey–Fuller test ($H_0$: unit root present in the series). 
    
    * Phillips-Perron test ($H_0$: integrated of order 1). 
    
    * P-value threshold is chosen to be 0.05. pvalue = 0.01 means <= 0.01. No laps at this stage. 
    
    * Series to be transformed are: `3-month zero-coupon yield`, `10-year zero-coupon spread`, `Hedonic rental yields - Sydney houses`, `Unemployment rate - NSW`. 
    
    ```{r, message=FALSE, warning=FALSE, results='hide'}
    ##############################
    # Augmented Dickey–Fuller test
    adf_results = data.frame(ADF_statistics = rep(NA,length(all_var_names)), ADF_pvalues = rep(NA,length(all_var_names)), row.names = all_var_names)
    for (i in 1:length(all_var_names)) {
      results = adf.test(all_data[,i])$type1
      adf_results[i,1] = results[1,2] # statistic 
      adf_results[i,2] = results[1,3] # pvalue 
    } 
    adf_results = adf_results %>% mutate(ADF_stationary = ifelse(ADF_pvalues <= 0.05, "Yes", "No evidence"))
    
    ######################
    # Phillips-Perron test 
    pp_results = data.frame(PP_statistics = rep(NA, length(all_var_names)), PP_pvalues = rep(NA, length(all_var_names)), row.names = all_var_names)
    for (i in 1:ncol(all_data)) {
      pp_results[i,1] = tseries::pp.test(all_data[,i])$statistic
      pp_results[i,2] = tseries::pp.test(all_data[,i])$p.value
    } 
    pp_results = pp_results %>% mutate(PP_stationary = ifelse(PP_pvalues <= 0.05, "Yes", "No evidence"))
    ```
    
    ```{r, include=TRUE}
    non_stationary_var_names = c("3-month zero-coupon yield", "10-year zero-coupon spread", "Hedonic rental yields - Sydney houses", "Unemployment rate - NSW")
    pp_results_trans = data.frame(PP_statistics = rep(NA, length(non_stationary_var_names)), PP_pvalues = rep(NA, length(non_stationary_var_names)), row.names = non_stationary_var_names)
    adf_results_trans = data.frame(ADF_statistics = rep(NA, length(non_stationary_var_names)), ADF_pvalues = rep(NA, length(non_stationary_var_names)), row.names = non_stationary_var_names)
    
    # print results 
    stationary_results_lag0 = cbind(adf_results, pp_results)
    kbl(stationary_results_lag0) %>% kable_styling(latex_options = c("striped", "scale_down"))
    ```
    
    ## Transform if non-stationary  
    
    Non-stationary series were differenced once. Now, all series starts from Q2 2002.
    
    ```{r, results='hide'}
    ###########################
    # 3-month zero-coupon yield
    # differencing 
    zcp3m_stationary = zcp3m - dplyr::lag(as.vector(zcp3m), n = 1)
    zcp3m_stationary = ts(zcp3m_stationary[-1])
    
    # store new stationarity test results 
    pp_results_trans[1,1] = tseries::pp.test(zcp3m_stationary)$statistic
    pp_results_trans[1,2] = tseries::pp.test(zcp3m_stationary)$p.value
    results = adf.test(zcp3m_stationary)$type1
    adf_results_trans[1,1] = results[1,2] # stats
    adf_results_trans[1,2] = results[1,3] # pvalue 
    
    ############################
    # 10-year zero-coupon spread
    # differencing 
    zcp10y_spread_stationary = zcp10y_spread - dplyr::lag(as.vector(zcp10y_spread), n = 1)
    zcp10y_spread_stationary = ts(zcp10y_spread_stationary[-1])
    
    # store new stationarity test results 
    pp_results_trans[2,1] = tseries::pp.test(zcp10y_spread_stationary)$statistic
    pp_results_trans[2,2] = tseries::pp.test(zcp10y_spread_stationary)$p.value
    results = adf.test(zcp10y_spread_stationary)$type1
    adf_results_trans[2,1] = results[1,2] # stats
    adf_results_trans[2,2] = results[1,3] # pvalue 
    
    #######################################
    # Hedonic rental yields - Sydney houses
    # differencing 
    rental_houses_syd_stationary = rental_houses_syd - dplyr::lag(as.vector(rental_houses_syd), n = 1)
    rental_houses_syd_stationary = ts(rental_houses_syd_stationary[-1])
    
    # store new stationarity test results 
    results = tseries::pp.test(rental_houses_syd_stationary)
    pp_results_trans[3,1] = results$statistic
    pp_results_trans[3,2] = results$p.value
    results = adf.test(rental_houses_syd_stationary)$type1
    adf_results_trans[3,1] = results[1,2] # stats
    adf_results_trans[3,2] = results[1,3] # pvalue 
    
    
    #########################
    # Unemployment rate - NSW
    # differencing 
    unemploy_nsw_stationary = unemploy_nsw - dplyr::lag(as.vector(unemploy_nsw), n = 1)
    unemploy_nsw_stationary = ts(unemploy_nsw_stationary[-1])
    
    # store new stationarity test results 
    results = tseries::pp.test(unemploy_nsw_stationary)
    pp_results_trans[4,1] = results$statistic
    pp_results_trans[4,2] = results$p.value
    results = adf.test(unemploy_nsw_stationary)$type1
    adf_results_trans[4,1] = results[1,2] # stats
    adf_results_trans[4,2] = results[1,3] # pvalue 
    
    ####################
    # match time windows
    home_value_houses_syd = ts(home_value_houses_syd[-1])
    gdp_capita = ts(gdp_capita[-1])
    asx200 = ts(asx200[-1])
    forex = ts(forex[-1])
    ```
    
    
    ```{r}
    pp_results_trans = pp_results_trans %>% mutate(PP_stationary = ifelse(PP_pvalues <= 0.05, "Yes", "No evidence"))
    adf_results_trans = adf_results_trans %>% mutate(ADF_stationary = ifelse(ADF_pvalues <= 0.05, "Yes", "No evidence"))
    
    stationary_results_lag1 = cbind(adf_results_trans, pp_results_trans)
    kbl(stationary_results_lag1) %>% kable_styling(latex_options = c("striped", "scale_down"))
    ```
    
    
    # Find the optimal order p
    
    Choose p = 1. 
    
    ```{r}
    all_data_stationary = cbind(zcp3m_stationary, zcp10y_spread_stationary, home_value_houses_syd, rental_houses_syd_stationary, gdp_capita, asx200, unemploy_nsw_stationary, forex)
    
    # store statistics 
    max_lag = 6 # covariance matrix singular for lag >= 6
    choose_order = data.frame(lag = 1:max_lag, AIC = rep(NA, max_lag), BIC = rep(NA, max_lag), HQC = rep(NA, max_lag))
    
    # VARselect(all_data_stationary, lag.max = max_lag, type = "none") 
    # Note: `AIC()`, `BIC()` functions in `R` does not use the right residual covariance. 
    
    for (lag in 1:max_lag) { 
      results = VAR(all_data_stationary, type = "none", p = lag)
      summ_results = summary(results)
      choose_order[lag,2] = log(det(summ_results$covres)) + 2 * lag * results$K ^ 2 / results$obs # AIC 
      choose_order[lag,3] = log(det(summ_results$covres)) + results$K ^ 2 * lag * log(results$obs) / results$obs # BIC 
      choose_order[lag,4] = log(det(summ_results$covres)) + results$K ^ 2 * lag * 2 * log(log(results$obs)) / results$obs # HQC
    }
    
    ####################
    # find optimal order 
    choose_order$AIC = ifelse(choose_order$AIC == min(choose_order$AIC), paste(choose_order$AIC, "*"), choose_order$AIC) 
    choose_order$BIC = ifelse(choose_order$BIC == min(choose_order$BIC), paste(choose_order$BIC, "*"), choose_order$BIC) 
    choose_order$HQC = ifelse(choose_order$HQC == min(choose_order$HQC), paste(choose_order$HQC, "*"), choose_order$HQC) 
    
    optimal_order = 1
    
    kbl(choose_order) %>% kable_styling(latex_options = c("striped", "scale_down"))
    ```
    
    
    # Prepare training and test datasets
    
    Most recent 20% observations (15 quarters) as test set.
    
    ```{r}
    num_test = round(length(zcp3m) * 0.2)
    test = all_data_stationary[1:num_test, ]
    train = all_data_stationary[-c(1:num_test),]
    ```
    
    
    # Train the model
    
    Build VAR(1) on the training set. 
    
    ```{r}
    train_var = VAR(train, p = optimal_order, type = "none")
    full_var = VAR(all_data_stationary, p = optimal_order, type = "none")
    ```
    
    # Causality among variables 
    
    * [Why important](https://stats.stackexchange.com/questions/438822/how-does-granger-causality-relate-to-vector-autoregression). Granger causality test ($H_0$: no Granger causality). If X Granger-causes Y, X forecasts Y. 
    
    * Instantaneous causality test ($H_0$: no Granger causality). If X instantaneously Granger-causes Y, future values of X forecast Y. 
    
    * Group test conclusions: 
      
      + All series instantaneous Granger-cause others. 
    
    + `3-month zero-coupon yield`, `GDP per capita growth rate`, `S&P/ASX200 growth rate` also Granger-cause others. 
    
    * 1 vs 1 test conclusions: 
      
      + `3-month zero-coupon yield`, `GDP per capita`, `S&P/ASX200` Granger-cause 2-3 other variables.
    
    + `NSW unemployment rate` is Granger-caused by 5 other variables. 
    
    ![Granger causality test.](granger_causality.PNG)
    
    ```{r}
    ###########################
    # Granger causes ALL others 
    causality_group_results = data.frame(Granger_statistics = rep(NA, length(all_var_names)), Granger_pvalues = rep(NA, length(all_var_names)), Instant_statistics = rep(NA, length(all_var_names)),Instant_pvalues = rep(NA, length(all_var_names)), row.names = all_var_names)
    
    for (i in 1:length(all_var_names)) {
      results = causality(train_var, cause = colnames(train_var$y)[i])
      causality_group_results[i,1] = results$Granger$statistic
      causality_group_results[i,2] = results$Granger$p.value
      causality_group_results[i,3] = results$Instant$statistic
      causality_group_results[i,4] = results$Instant$p.value
    }
    
    causality_group_results = causality_group_results %>% mutate(Granger_cause = ifelse(Granger_pvalues < 0.05, "Yes", "No"), Instantaneous_cause = ifelse(Instant_pvalues < 0.05, "Yes", "No"))
    kbl(causality_group_results) %>% kable_styling(latex_options = c("striped", "scale_down"))
    
    ##########################
    # Granger causes ONE other
    # find p values 
    causality_presults = as.data.frame(matrix(NA, nrow = length(all_var_names), ncol = length(all_var_names)))
    for (row in 1:length(all_var_names)) {
      for (col in 1:length(all_var_names)) {
        if (row != col) {
          results = grangertest(all_data_stationary[,row], all_data_stationary[,col], order = 1)
          causality_presults[row,col] = results$`Pr(>F)`[2]
        }
      }
    }
    
    # state conclusions 
    causality_results = ifelse(causality_presults < 0.05, "Yes", "No")
    row.names(causality_results) = all_var_names
    colnames(causality_results) = all_var_names
    #kbl(causality_results) %>% kable_styling(latex_options = c("striped", "scale_down"))
    ```
    
    # Evaluate the model 
    
    ## Diagnostics on train set 
    
    * Portmanteau test ($H_0$: residual autocorrelation = 0). 
    
    * [Engle's ARCH test](https://au.mathworks.com/help/econ/engles-arch-test.html) ($H_0$: residual autocorrelation = 0).

* Jarque-Bera test ($H_0$: skewness and kurtosis match gaussian distribution). 

* The results are not very satisfactory. 

```{r}
resid_results = data.frame(statistics = rep(NA,4), pvalues = rep(NA,4), conclusion = rep(NA,4), row.names = c("Portmanteau test - white noise residuals", "Engle's ARCH test - white noise residuals", "Jarque-Bera Normality test - Skewness", "Jarque-Bera Normality test - Kurtosis"))

##################
# Portmanteau test
results = vars::serial.test(train_var, lags.pt = optimal_order, type = "PT.adjusted")
resid_results[1,1] = results$serial$statistic 
resid_results[1,2] = results$serial$p.value
resid_results[1,3] = ifelse(resid_results[1,2] <= 0.05, "Not white noise", "White noise")

###############
# Lagrange test
results = vars::arch.test(train_var, lags.multi = optimal_order, multivariate.only = T)
resid_results[2,1] = results$arch.mul$statistic
resid_results[2,2] = results$arch.mul$p.value
resid_results[2,3] = ifelse(resid_results[2,2] <= 0.05, "Not white noise", "White noise")

#######################
# normality - skewness
results = vars::normality.test(train_var, multivariate.only = T)
resid_results[3,1] = results$jb.mul$Skewness$statistic
resid_results[3,2] = results$jb.mul$Skewness$p.value
resid_results[3,3] = ifelse(resid_results[3,2] <= 0.05, "Not normal", "Normal")

######################
# normality - kurtosis
resid_results[4,1] = results$jb.mul$Kurtosis$statistic
resid_results[4,2] = results$jb.mul$Kurtosis$p.value
resid_results[4,3] = ifelse(resid_results[4,2] <= 0.05, "Not normal", "Normal")

kbl(resid_results) %>% kable_styling(latex_options = c("striped", "scale_down"))
```


## Validation on test set 

* Forecast to the test set. 

* 

```{r}
# forecast 
test_forecast = predict(train_var, n.ahead = 15, ci = 0.95)

# convert forecasted variables back to its original units 
# given the initial value 
zcp3m_back = function (x, init) {
  # it was differenced once 
  diffinv(x, xi = init)
}
zcp10y_back = function (x, init, zcp3) {
  # spread over zcp3m
  # then, it was differenced once
  new = diffinv(x, xi = init)
  new + zcp3
}
home_value_back = function (x, init) {
  # index to growth rate 
  
}
```


# Forecast 

Convert back to original units. 

```{r}

```

