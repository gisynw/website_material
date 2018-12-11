setwd(".")

source("Significant_level.R")
source("valid_lake_level.R")
source("total_month.R")
source("get_month_period.R")
source("get_level.R")
source("get_precipitation.R")
source("data_analysis.R")
source("Summarize Data.R")

list_ex <- Filte_Valid_data(2)
total <- create_month_year(1990, 2016, 5, 10)
parttime <- get_the_time_period(list_ex, total)

level <- get_water_leve(list_ex)
prcp <- get_precipitation_data(list_ex,3)

analysisresult <- data_analysis(level, prcp, list_ex)


data_summarize(list_ex, parttime, analysisresult)
