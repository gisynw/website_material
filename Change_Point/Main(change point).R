

setwd(".")

source("change_point.R")
source("valid_lake_level.R")
source("total_month.R")
source("get_month_period.R")
source("get_level.R")

validdata <- Filte_Valid_data(2)
level <- get_water_leve(validdata)

total <- create_month_year(1990, 2016, 5, 10) # run two times(first time download the package)
yearvalue <- get_the_time_period(validdata, total)

change_point(level, yearvalue)

