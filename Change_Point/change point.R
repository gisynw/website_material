require('zoo')
require('bcp')
require('strucchange')

value <- level[[1]]
time <- yearvalue[[1]]

change_point <- function(value, time){
    all_month <- as.numeric(time)
    africa_ts <- zoo(value,all_month)
        
    bcp.ri <- bcp(as.vector(value))
    # plot(bcp.ri, main="Lake elevation bcp")
        
    bp <- breakpoints(value ~ 1, h = 2)$breakpoints    #it require at least 2 observations per segment
    print(c("The breakpoint is:", bp))
        
    rho <- rep(0, length(value))
    rho[bp] <- 1
    
    b.num<-1 + c(0,cumsum(rho[1:(length(rho)-1)])) # Find the sum, the ith element is the sum
    
    bp.mean <- unlist(lapply(split(value,b.num),mean)) # calculate the mean at each segment
    
    # big_matrix <- matrix(NA,10,1)
    bp.ri <- b.num
    # bp.ri = c(rep(NA, length(b.num)))
    for (i in 1:length(bp.ri)) 
      bp.ri[which(bp.ri ==i)] <- bp.mean[i]
    
    xax <- seq(1990, 2017)
    op <- par(mfrow=c(2,1),col.lab="black",col.main="black")
    op2 <- par(mar=c(0,4,4,2),xaxt="n", cex.axis=0.75)
    par(op2)
    plot(1:length(bcp.ri$data[,2]), bcp.ri$data[,2], col="grey", pch=20,
         xlab="", ylab="Posterior Mean", main="Lake Elevation")  # plot the data(Lake elevation)
    lines(bcp.ri$posterior.mean, lwd=2)  #a vector containing the posterior means
    lines(bp.ri, col="blue",lwd = 2, lty = 4)  # plot the mean of the each segment 
    
    op3 <- par(mar=c(5,4,0,2), xaxt="s", cex.axis=0.75)
    
    final_rst = data.frame(
      # times = as.character(year(time)),
      times = time,
      posterior_prob = bcp.ri$posterior.prob)
    if(any(is.na(final_rst))){
      final_rst = na.omit(final_rst)
    }
    plot(final_rst$times, final_rst$posterior_prob, yaxt="n", type="l", ylim=c(0,1),
         xlab="Year", ylab="Posterior Probability", main="")
    # axis(side=1, at=seq(1, nrow(final_rst), by=5))
    for (i in 1:length(bp.ri)) abline(v=xax[bp[i]], col="red")
    axis(2, yaxp=c(0, 0.9, 3))
    par(op3)
    par(op)  
}

