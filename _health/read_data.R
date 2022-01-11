setwd("~/Documents/GitHub/retirement-toolkit/_health")
library("readxl")
library("expm")

# read data
my_data <- read_excel("Params_US_5_state.xlsx",col_names=TRUE)

params_5_no_frailty <- my_data[1:3,3:ncol(my_data)] # model 3

params_5_no_frailty_with_trend <- my_data[5:8,3:ncol(my_data)] # model 4

params_5_frailty <- my_data[10:14,3:ncol(my_data)] # model 5

# select the parameter and model
params=params_5_frailty
model=5

# input characteristics of the individual at time 0
age=65
gender=0
i=9 # wave index
latent=0 # initial value of latent factor