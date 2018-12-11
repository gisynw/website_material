

##### get the time period 

get_the_time_period <- function(list_ex,all_month)
{
  month_matrix <- c()
  e <- 1
  for (f in 2:(list_ex[[1]]+1)) 
    if(!(f %% 2))
    {
      month_matrix[[e]] <- all_month[(list_ex[[f]] - 1) : (list_ex[[f+1]] - 1)]
      e <- e + 1
    }
  return(month_matrix)
}