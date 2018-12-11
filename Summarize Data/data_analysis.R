## Author:Yanan Wu
## Date: 9/14/2018


data_analysis <- function(water_level, prcp, list_ex)
{
  setwd(".")
  first_matrix <- matrix('',1,1)
  
  for (k in 1:length(water_level)) 
  {
    level <- water_level[[k]]
    precipitation <- prcp[[k]]
    correl_coeffi <- cor(level, precipitation, method = "pearson") ## correlation coefficient 
    lm_result <- lm(level~precipitation)
    sum_corr <- summary(lm_result)
    # slope <- sum_corr$coefficients[[2]]
    p_value <- sum_corr$coefficients[[8]]
    Sing_level <- Significant(p_value)
    R_squared <- sum_corr$r.squared 
    
    result <- matrix("", 4,1)
    result[1,] <- round(correl_coeffi,4)
    result[2,] <- round(R_squared,4)
    result[3,] <- round(p_value,6)
    result[4,] <- Sing_level
    first_matrix <- rbind(first_matrix, result)
  }
  row.names(first_matrix)[2:length(row(first_matrix))] <- rep(c("Correlation","R_Squared", "P_Value","Sing_level"), length(water_level))   
  row.names(first_matrix)[1] <- "ID"
  first_matrix[1,] <- list_ex[[length(list_ex)]]
  # write.csv(first_matrix, file <- gsub(" ","", paste(list_ex[[length(list_ex)]],".csv")))
  return(first_matrix)
}

