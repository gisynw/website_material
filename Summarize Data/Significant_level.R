## Author:Yanan Wu
## Date: 9/17/2018

Significant = function(x) {
  
  if(x <0.001)
  {
    x7 <-'***' 
  }
  if((0.001 < x ) & (x < 0.01))
  {x7 <-'**'}
  if((0.01 < x ) & (x < 0.05))
  {x7 <-'*'}
  if((0.05 < x ) & (x < 0.1))
  {x7 <-'.'} 
  if(0.1 < x)
  {x7 <-' '}
  return(x7)
}