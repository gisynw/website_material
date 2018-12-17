wssplot <- function(data, nc = 15, seed =1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for( i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)
  }
  plot(1:nc,wss,type = "b", xlab = "Number of clusters",
       ylab = "Within groups sum of squares")
}
data <- df
list.files()
setwd(".")
total <- list.files(pattern = ".csv")
total

dfarea <- read.csv(total[1], stringsAsFactors = F)
# write.csv(fileyanan, file = "2002.csv")
# warnings()

# dfarea <- readxl::read_xls(total[k], sheet = NULL, range = NULL, col_names = TRUE,
#                            col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf)
# 
# totallen <- length(rownames(dfarea))
colength <- length(colnames(dfarea))
df <- dfarea[,2:(colength-1)]

# wssplot(df)
base::t(df)

if (!require("NbClust")) install.packages("NbClust")
library("NbClust")
devAskNewPage(ask=TRUE)

nc <- NbClust::NbClust(base::t(df), min.nc = 2, max.nc = 55, method = "kmeans")

table(nc$Best.nc[1,])

# barplot(table(nc$Best.nc[1,]),
#         xlab = "Numbers of Clusters",ylab = "Numbers of Criteria",
#         main = "Numbers of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df,2, nstart =25)
fit.km
fit.km$size
# fit.km$centers
fit.km$cluster
# aggregate(dfarea[-1], by=list(cluster=fit.km$cluster), mean)
# dfarea$group <- fit.km$cluster
# ck.km <- table(dfarea$ID, fit.km$cluster)
# 
# lin <- matrix(" ", 1,1)
# for (t in 1:length(dfarea$X)) 
# {
#   m <- gregexpr('[0-9]+',dfarea$X[t])
#   result <- regmatches(dfarea$X[t],m)
#   dfarea$X[t] <- result
# }
dfarea$group <- c(fit.km$cluster)
m <- gregexpr('[0-9]+',total[k])
result <- regmatches(total[k],m)
# result
write.csv(dfarea, file = gsub(" ","",paste(result[[1]][1], "_cluster.csv")))
# library(flexclust)
# randIndex(ck.km)
