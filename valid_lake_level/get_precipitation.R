## Author: Yanan Wu
## Date:   7/16/2018

setwd(".")
getwd()



get_precipitation_data <- function(level_list,n)
{
  datacsv <- list.files(pattern = ".csv")
  df <- read.csv(datacsv, stringsAsFactors = F)
  list_length <- level_list[[1]][1]
  k <- 1
  precipitation <- c()
  for (i in 2:list_length)
  {
    if(!(i %% 2))
    {
      precipitation[[k]] <- df[,n][level_list[[i]]: level_list[[i+1]]]
      k <- k + 1
    }
  }
  return(precipitation)
}


