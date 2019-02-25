## calculate mean month level and mean year level  
setwd(".")
list.files()
library("zoo")
library("xts")
# id <- read.csv("ID.csv", stringsAsFactors = F)
# FILE_ID <- id$﻿ID
# 
# new_id_first <- gsub(" ","",paste(0,id$﻿ID[1:97]))
# 
# setwd("./ORIGINAL_TABLE")
# 
# for( i in 98:length(id$﻿ID))
# {
  df <- read.csv("01002300.csv", stringsAsFactors = F)  
  # ID <- FILE_ID[i]
  
  level <- df$ELEVATION    
  level_date <- df$READ_DATE    
  date_level <- as.Date(level_date, format = "%Y-%m-%d")
  
  ts <- zoo(level,date_level)
  
  nts <- aggregate(ts,time(ts),mean)   ### remove duplicate date and calculate mean water level
  # length(nts)
  
  month_mean <- apply.monthly(as.xts(ts),mean) 
  # plot(month_mean)
  year_mean <- apply.yearly(as.xts(ts), mean)
  # year_mean

  # month_date <- substring(index(month_mean),1,7)
  # core_level_month  <- coredata(month_mean)
  # df_month <- data.frame(month_date, core_level_month)
  
  year_date <- substring(index(year_mean),1,4)
  core_level <- coredata(year_mean)
  df_year <- data.frame(year_date, core_level)
  
  # write.csv(df_month, file = gsub(" ", "", paste("../Month_Table/", FILE_ID[i], ".csv")))
  write.csv(df_year, file = gsub(" ", "", paste("year_01002300", ".csv")))# }