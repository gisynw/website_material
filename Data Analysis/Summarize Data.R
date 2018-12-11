## Author:Yanan Wu
## Date: 9/23/2018

data_summarize <- function(list_ex,time_period,analysis_result)
{
  #empty <- matrix("", 1,6)
  start <- time_period[[1]][1]
  end <- time_period[[1]][length(time_period[[1]])]
  length_time <- length(time_period[[1]])
  Final <- data.frame(start,end,length_time)
  if(list_ex[[1]] > 2)
  {
    for(i in 2:(list_ex[[1]]/2))
    {
      start <- time_period[[i]][1]
      end <- time_period[[i]][length(time_period[[i]])]
      length_time <- length(time_period[[i]])
      Final[i,] <- data.frame(start,end,length_time)
    } 
  }
  ## get the pvalue, singilevel, and rsquared.
  # pvalue <- analysis_result[2]
  # singlevel <- analysis_result[3]
  # rsquared <- analysis_result[4]
  Yanan <- matrix(" ",(list_ex[[1]]/2),4)
  # Yanan[1,] <- c(pvalue,singlevel,rsquared)
  h <- 2
  for (i in 1:(list_ex[[1]]/2))
  {
    correlation <- analysis_result[h]
    rsquared <- analysis_result[h+1]
    p_value <- analysis_result[h+2]
    Sing_level <- analysis_result[h+3]
    Yanan[i,] <- c(correlation,rsquared,p_value,Sing_level)
    h <- h +4
  } 
  
  colnames(Yanan) <- c("correlation", "rsquared","p_value","Sing_level")
  summarizez_data <- cbind(Final,Yanan)
  summarizez_data <- as.matrix(summarizez_data)
  summ_rowname <- as.numeric(list_ex[length(list_ex)][1])
  row.names(summarizez_data) <- rep(summ_rowname,list_ex[[1]]/2)
  # colnames(empty) <- colnames(summarizez_data)
  # empty <- rbind(empty,summarizez_data)
  # write.csv(empty, file = gsub(" ", "",paste("summarize",".csv")))
  return(summarizez_data)
}