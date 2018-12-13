## convert month ot year
setwd(".")
file <- list.files(pattern = ".csv")
level <- read.csv(file)


## calculate the year mean from month mean
level[is.na(level)] <- 0

multiple_lake <- matrix("", length(level$UID), 28)

for (i in 2:953) 
{
  small_matrix <- matrix("", 1, 28) 
  for(k in 0:27)
  {
    lake_value <- as.numeric(level[i,(k*6+2):(k*6+7)])
    na_value <- which(lake_value == 0)
    if(length(na_value) == 0)
    {
      single_lake <- mean(as.numeric(level[i,(k*6+2):(k*6+7)])) 
      small_matrix[1,k+1] <- round(single_lake,3)  
    }
    if(length(na_value) != 0)
    {
      single_lake <- sum(as.numeric(level[i,(k*6+2):(k*6+7)]))/ (6 - length(na_value)) 
      small_matrix[1,k+1] <- round(single_lake,3)  
    }
  }
  multiple_lake[i-1,] <- small_matrix 
}

colnames(multiple_lake) <- c(1990:2017)
rownames(multiple_lake) <- level$UID

write.csv(multiple_lake, file = "Year_1990-2017.csv")
