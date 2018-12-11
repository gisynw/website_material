## Author:Yanan Wu
## Date: 2018/9/20

setwd(".")
source("valid_lake_level.R")
source("get_level.R")
source("get_precipitation.R")
source("Significant_level.R")
source("data_analysis.R")

list_ex <- Filte_Valid_data(2)
water_level <- get_water_leve(list_ex)
prcp <- get_precipitation_data(list_ex, 3)

data_analysis(water_level, prcp, list_ex)
