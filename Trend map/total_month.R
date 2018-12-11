#####  Create month, the first day of each month,

create_month_year <- function(firstyear, lastyear, firstmonth, lastmonth)
{
  if (!require("zoo")) install.packages("zoo")
  library("zoo")
  start_year <- firstyear
  end_year <- lastyear
  start_month <- firstmonth
  end_month <- lastmonth
  # year_num <- end_year - start_year + 1
  num.year <- end_year - start_year + 1  ## the number of years in the time series
  num.month <- num.year*12-1  
  all_month <- as.yearmon(start_year + seq(0, num.month)/12) 
  all_month <- as.Date(all_month)
  all_month <- all_month[as.numeric(format(all_month,"%m")) >= start_month & as.numeric(format(all_month,"%m")) <= end_month]
  return(all_month)
}
