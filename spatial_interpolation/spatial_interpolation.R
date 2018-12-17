if (!require("raster")) install.packages("raster")
library("raster")
if (!require("sp")) install.packages("sp")
library("sp")
if (!require("rgdal")) install.packages("rgdal")
library("rgdal")
if (!require("raster")) install.packages("raster")
library("raster")
if (!require("gstat")) install.packages("gstat")
library("gstat")
if (!require("maptools")) install.packages("maptools")
library("maptools")

setwd(".")
getwd()
totalprcp <- list.files(pattern = ".csv")
Data <- read.csv(totalprcp[1], stringsAsFactors = F, header = T)
head(Data)
Data$PRCP
## plot the precipitation data 
plot(sort(Data$PRCP), ylab = "1990_05 monthly precipitation(mm)", las=1, xlab = "station")

## input the Minnesota shapefile 
boundary <- readOGR("MN.shp")
plot(boundary, col = "blue")

Data$new_longtitude <- Data[,6]
Data$new_latitude <- Data[,5]
Data[,16:17]
## set the projection as 
dsp <- SpatialPoints(Data[,16:17], proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
dsp <- SpatialPointsDataFrame(dsp,Data)

cuts<-c(0,2,4,6,20,40)# set the interval
##set up the solor gradiants ("red","blue")
blues <- colorRampPalette(c("red","yellow","blue"))(5)
plot(10:1, bg = blues[rank(5:1)], cex = 2, pch = 22) ## example

# Spatial Polyton Objects
pols <- list("sp.polygons",boundary, fill = "lightgray")
spplot(dsp,"PRCP",cuts=cuts, col.regions=blues, sp.layout=pols, pch=20, cex=2)

NAD83 <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

#using WGS, transfer the lon lat to Geographic Coordinate System:
dsp1<-spTransform(dsp,NAD83)
bound1<-spTransform(boundary,NAD83)

## neighborhood polygon interpolation
if (!require("dismo")) install.packages("dismo")
library("dismo")
if (!require("deldir")) install.packages("deldir")
library("deldir")

v <- voronoi(dsp1)
plot(v)

if (!require("rgeos")) install.packages("rgeos")
library("rgeos")
# aggregation, decreased the resolution
bound2<-aggregate(bound1) 
# intersect the two layer 
v1<-intersect(v,bound2) 
#plot polygon
spplot(v1, "PRCP", col.regions=rev(get_col_regions()), main = "Neighoborhood Polygon Interpolation",
       ylab = "Prcp(inch)")

## the result is polygon, next step convert the polygon to raster,
## before I convert the shape for raster, create a new blank raster, and set up the raster resolution, use the 
## extent to set up the spatial extent

blank_raster<-raster(nrow=100,ncol=100,extent(boundary))

## next step, assign the raster value

values(blank_raster)<-1

plot(blank_raster)

## cause the all the value are1, so the colour is same, next, we assign more raster value
values(blank_raster)<-1:(100*100)

plot(blank_raster,col=rainbow(100))

## now we convert the polygon to raster, but the cell size is not sure, so we use loop to decide the best cell size.
layout(matrix(1:4, ncol=2, byrow=TRUE))

res<-c(20,100,500,1000)

for(r in res){
  
  blank_raster<-raster(nrow=r,ncol=r,extent(boundary))
  
  values(blank_raster)<-1
  
  bound_raster<-rasterize(boundary,blank_raster)
  
  bound_raster[!(is.na(bound_raster))] <- 1
  
  plot(bound_raster,main=paste("Res: ",r,"*",r))
  
  plot(boundary,add=T)
  rf <- writeRaster(bound_raster, filename= paste(r,".tif"), format="GTiff", overwrite=TRUE)
}

## 1000*1000 is the good result
## neighboring polygon interpolation
vr <- rasterize(v1, bound_raster,"PRCP")
plot(vr)
polygon_inter <- writeRaster(vr, filename= paste("Polygon_RESULT",".tif"), format="GTiff", overwrite=TRUE)

## Nearest Neighboring interpolation
gs<-gstat(formula=PRCP~1,location=dsp1,nmax=5,set=list(idp=0))

nn<-interpolate(bound_raster,gs)

##mask
nnmask<-mask(nn,vr)

plot(nnmask, main = "Nearest Neighoboring Interpolation")
rf <- writeRaster(nnmask, filename= paste("Nearest_RESULT",".tif"), format="GTiff", overwrite=TRUE)

## IDW interpolation
gs <- gstat(formula=PRCP~1, locations=dsp1)
idw <- interpolate(bound_raster, gs)
idwmask<-mask(idw,vr)
plot(idwmask, main = "IDW Interpolation")
IDW <- writeRaster(idwmask, filename= paste("IDW_RESULT",".tif"), format="GTiff", overwrite=TRUE)

## Ordinary Krining Interpolation
if (!require("raster")) install.packages("raster")
library("raster")

vtry <- variogram(log(PRCP) ~ LONGITUDE+LATITUDE, dsp)
plot(vtry, plot.number=T)

v<- variogram(log(PRCP) ~ 1, data =dsp)
plot(v,plot.number=T)
v.fit<-fit.variogram(v,model=vgm(1,"Gau",250,0.0))
plot(v,v.fit, plot.number = T, main = "Exp_Function")

#transfer the bound raster to spatial grid raster
Grid<-as(bound_raster,"SpatialGridDataFrame")

kri<-krige(formula=PRCP~1,model=v.fit,locations=dsp,newdata=Grid,nmax=70, nmin=10)
spplot(kri["var1.pred"], main = "Oridinary Kriging")
# raster(a)
# raster_a <-raster(a)
# IDW <- writeRaster(a, filename= paste("Krig_RESULT",".tif"), format="GTiff", overwrite=TRUE)
# show.vgms()








