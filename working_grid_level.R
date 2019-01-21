library(raster)
library(rgeos)
library(sp)
library(rgdal)

library(adehabitatMA)

#♣setwd("C:/Users/bgobbi/Nextcloud/SFTP/R/INPUTS/2018")
##setwd("F:/results_agisoft/proj_results")

list.files()
r<-raster("180912_10.tif")
crs(r)
#crs(r)<-CRS.new
#CRS.new <- CRS("+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_def")

test<-bbox(r)
class(test)
x <- seq(from = test[[1]], test[[3]], by =3)
y <- seq(test[[2]], test[[4]], by = 35)

xy <- expand.grid(x = x, y = y)
class(xy)
str(xy)
grid.pts<-SpatialPointsDataFrame(coords= xy, data=xy)
crs(grid.pts)<-"+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(grid.pts)
gridded(grid.pts)
class(grid.pts)



gridded(grid.pts) <- TRUE
gridded(grid.pts)
str(grid.pts)
plot(grid.pts)

#Make the grid of points into a Spatial Polygon then convert the spatial polygons to a SpatialPolygonsDataFrame.

grid <- as(grid.pts, "SpatialPolygons")
plot(grid)
str(grid)
class(grid)
summary(grid)
gridspdf <- SpatialPolygonsDataFrame(grid, data=data.frame(id=row.names(grid), 
                                                           row.names=row.names(grid)))
names.grd<-sapply(gridspdf@polygons, function(x) slot(x,"ID"))
text(coordinates(gridspdf), labels=sapply(slot(gridspdf, "polygons"), function(i) slot(i, 
                                                                                       "ID")), cex=0.3)
points(deer.albers, col="red")
str(gridspdf@polygons)

gridspdf
sub_gridspdf<-gridspdf[which(gridspdf@data$id == "g300" | gridspdf@data$id == "g301" | gridspdf@data$id ==  "g302") ,]
sub_gridspdf
plot(sub_gridspdf)


# Crop grid selon sub_grispdf
re<-crop(r, sub_gridspdf)

plot(re)
############################CORRECTING dem#############################

#USING FIRST PERCENTILE
plot(re)
plot(sub_gridspdf, add=TRUE)

#creates lists of values raster from spdf
#q<-extract(r,sub_gridspdf,na.rm=TRUE)
#qdat<-as.data.frame(q)


#quantile de chaque case

quantile<-lapply(q, function(x) if (!is.null(x)) quantile(x, c(.02), na.rm=TRUE) else NA )
quantile
DT <-as.data.frame(quantile)
colnames(DT)<-sub_gridspdf@data$id
col<-colnames(DT)
dim(DT)
tDT<-t(DT)
sub_gridspdf<-cbind(sub_gridspdf, tDT)
merge<-merge(sub_gridspdf,DT, by.x=sub_gridspdf@data$id, by.y=DT)
DT
head(sub_gridspdf)


sub_gridspdf@data$X2.<- as.numeric(as.character(sub_gridspdf@data$X2.))

#rasterize


rp <- rasterize(sub_gridspdf, r, "X2.")

?rp
head(rp)
summary(rp)
res(rp)

test<-r-rp

CHM<-overlay(r, rp, fun=function(x,y){return(x-y)})




soustraction <- q[] - quantile []
CHM<-q-list(rep(quantile[[1]],273529),rep(quantile[[1]],273006),rep(quantile[[1]],273006))
CHM
quantile

#rasterize
make.grid(x, y, z, byx , byy , xlim, ylim,  function(x) sum(x, na.rm = T))



############################END CORRECTING DEM ########################

r <- raster(ncol=100, nrow=100)
r[] <- rnorm(ncell(r), 0, 50)
quantile(r)
quantile(r, probs = c(0.25, 0.75), type=7,names = FALSE)

#################classes#################
m2 <- c(-20, 0.5, 1, 0.5, 1,2,1,2,3,2,4,4,  4, 6,5,6,20,6)
rclmat <- matrix(m2, ncol=3, byrow=TRUE)
rc <- reclassify(CHM, rclmat)

#extract for canopy cover
#canopy cover####################
canopy<-CHM>5
canopy_cover<-(cellStats(canopy,stat="sum")/length(CHM))*100

canop<-extract(canopy,sub_gridspdf,fun=sum)

cel<-extract(CHM,sub_gridspdf,fun=sum)

canop<-extract(canopy,sub_gridspdf,fun=cellStats(canopy,stat="sum"))

??cellStats

# get list of individual polys
p <- lapply(sub_gridspdf@polygons , slot , "Polygons")

# areas of individual polygons
lapply(p[[2]], function(x) slot(x, "area"))


canop<-extract(canopy,sub_gridspdf,function(canopy){sum()/`length<-.factor`()})
canop<-extract(canopy,sub_gridspdf,function(canopy){
  sum()
  return()})


################high canopy class#############################
fun1=function(x){x>5}
poly<-rasterToPolygons(rc, fun1,dissolve=TRUE)

out<-ms_explode(poly)



# density of patches 
density_high_canopy<-(nrow(out@data))
#mean height 
zonal<-zonal.stats(out,CHM,max )
mean_max_height<-mean(zonal)

