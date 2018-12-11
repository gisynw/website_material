## Author: Yanan Wu
## Date: July 1 2018

##   Time series water-level variation and precipitation  
source("total_month.R")

## 5 means correlation, 6 means rsquared, 7 means p-value, 
## set up the threshold to choose the  appropriate lake level and prcepitation 
valid_length <- function(value, number)
{
value <- as.numeric(value)
number <- as.numeric(number)
if (!require("zoo")) install.packages("zoo")
library("zoo")
setwd('.')

df <-  read.csv("Data_Analysis_Prcp.csv",stringsAsFactors = F)
df_search <- read.csv("1990-2017.csv", stringsAsFactors = F)
prcp <- read.csv("PRCP_Mean.csv",stringsAsFactors = F)

month <- create_month_year(1990, 2017, 5, 10)

valid_lake <- which(df[,value] < number)

small_matrix <- matrix("",length(valid_lake),2)
valid_data_length <- length(valid_lake)
return(valid_lake)
}

plot_trend_map <- function(valid_lake, i)
{
  df <-  read.csv("Data_Analysis_Prcp.csv",stringsAsFactors = F)
  df_search <- read.csv("1990-2017.csv", stringsAsFactors = F)
  prcp <- read.csv("PRCP_Mean.csv",stringsAsFactors = F)
  
  month <- create_month_year(1990, 2017, 5, 10)
    
  startmonth <- which(df$start[valid_lake[i]] == month)
    endmonth <- which(df$end[valid_lake[i]] == month)
    ID <- df$X[valid_lake[i]]
    total_lake <- df_search[df_search[,1] == ID,]
    level <- as.numeric(total_lake[(startmonth+1):(endmonth+1)])
    precipitation <- as.numeric(prcp[prcp[,3] == ID,][(startmonth+3):(endmonth+3)])
    timesegment <- month[startmonth : endmonth]
    
    par(mar=c(5, 6, 2, 5) + 0.1)
    #Plot first set of data and draw its axis
    plot(timesegment, level, pch=16, axes=FALSE, xlab="", ylab="",
         type="b",col="black", main= paste(ID, "lake") )
    axis(2, ylim=c(0,1),col="blue",las=1)  ## las=1 makes horizontal labels
    mtext("Water_Level",side=2,line=5)
    box()

    # # ## Allow a second plot on the same graph
    par(new=TRUE)
    # ## Plot the second plot and put axis scale on right
    plot(timesegment, precipitation, pch=15,  xlab="", ylab="",
         axes=FALSE, type="b", col="red")
    # ## a little farther out (line=4) to make room for labels
    mtext("Precipitation",side=4,col="red",line=4)
    axis(4, col="black",col.axis="red",las=1)
    # ## Draw the time axis
    mtext("Time(Month)",side=1,col="black",line=2.5)
    axis(1, timesegment, format(timesegment, "%Y-%m"), cex.axis = .9)
    ## Add Legend
    legend("topleft",legend=c("Water_level","Precipitation"),
           text.col=c("black","red"),pch=c(16,15),col=c("black","red"))
}


