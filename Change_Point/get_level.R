get_water_leve <- function(list_ex)
{
  water_level <- c()
  v <- 1
  for(m in (list_ex[[1]] + 2): (length(list_ex) - 1))
  {
    if((m%%2))
    {
      water_level[[v]] <- list_ex[[m]] 
      v <- v+1
    }
  } 
  # waterlevel <- as.numeric(water_level)
  return(water_level)
}