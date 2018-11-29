require('zoo')
require('bcp')
require('strucchange')

value <- as.numeric(df[2,5:28])
time <- c(1993:2016)

change_point <- function(value, time)
{
  
africa<- value
all_month <- as.numeric(time)
africa_ts <- zoo(africa,all_month)
    
bcp.ri <- bcp(as.vector(africa))
# plot(bcp.ri, main="Lake elevation bcp")
    
bp <- breakpoints(africa ~ 1, h = 2)$breakpoints    #it require at least 2 observations per segment
print(paste("The breakpoint is:", bp[1], "and", bp[2]))
    
rho <- rep(0, length(africa))
rho[bp] <- 1

b.num<-1 + c(0,cumsum(rho[1:(length(rho)-1)])) # Find the sum, the ith element is the sum

bp.mean <- unlist(lapply(split(africa,b.num),mean)) # calculate the mean at each segment

big_matrix <- matrix(NA,10,1)

for (i in 1:length(bp.ri)) 
  bp.ri[i] <- bp.mean[b.num[i]]

xax <- seq(1990, 2017)
op <- par(mfrow=c(2,1),col.lab="black",col.main="black")
op2 <- par(mar=c(0,4,4,2),xaxt="n", cex.axis=0.75)
plot(1:length(bcp.ri$data[,2]), bcp.ri$data[,2], col="grey", pch=20,
     xlab="", ylab="Posterior Mean", main="Lake Elevation")  # plot the data(Lake elevation)
lines(bcp.ri$posterior.mean, lwd=2)  #a vector containing the posterior means
lines(bp.ri, col="blue")  # plot the mean of the each segment 
par(op2)
op3 <- par(mar=c(5,4,0,2), xaxt="s", cex.axis=0.75)
plot(all_month, bcp.ri$posterior.prob, yaxt="n", type="l", ylim=c(0,1),
     xlab="Year", ylab="Posterior Probability", main="")
for (i in 1:length(bp.ri)) abline(v=xax[bp[i]], col="red")
axis(2, yaxp=c(0, 0.9, 3))
par(op3)
par(op)  
}


