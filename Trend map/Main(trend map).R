##Author: Yanan Wu
## Date: August 18, 2018
setwd(".")

source("total_month.R")
source("Time series water level change trend.R")

## ## 5 means correlation, 6 means rsquared, 7 means p-value, 
## set up the threshold to choose the  appropriate lake level and prcepitation 

## select the lake with p-value smaller than 0.001
valid_lake <- valid_length(7, 0.001)

plot_trend_map(valid_lake, 15)
