setwd(".")
source("valid_lake_level.R")
source("get_precipitation.R")

listfile <- Filte_Valid_data(4) #The result include the start date, end date, valid lake level ,and lake ID.
get_precipitation_data(listfile, 5)
