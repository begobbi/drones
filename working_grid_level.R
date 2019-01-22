library(raster)
library(rgeos)
library(sp)
library(rgdal)
library(rmapshaper)
library(adehabitatMA)
library(maptools) 
library(plyr)
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

rp <- rasterize(sub_gridspdf, re, "X2.")

?rp
head(rp)
summary(rp)
res(rp)

test<-r-rp

CHM<-overlay(re, rp, fun=function(x,y){return(x-y)})

plot(CHM)




############################END CORRECTING DEM ########################



#################classes#################
m2 <- c(-20, 0.5, 1, 0.5, 1,2,1,2,3,2,4,4,  4, 6,5,6,20,6)
rclmat <- matrix(m2, ncol=3, byrow=TRUE)
rc <- reclassify(CHM, rclmat)

#extract for canopy cover
#canopy cover####################
canopy<-rc>5
#canopy_cover<-(cellStats(canopy,stat="sum")/length(CHM))*100
v <- extract(canopy, sub_gridspdf)
canopy_cover<-sapply(v, function(x) if (!is.null(x)) { sum(x)/length(x)} else NA)

##density
fun1=function(x){x>5}
poly<-rasterToPolygons(rc, fun1,dissolve=TRUE)
plot(poly)
p<-disaggregate(poly)

# where each patch is located 

patches_location <- over(x =p , y = sub_gridspdf)

patches_with_location <- spCbind(p, patches_location)
patches_with_location@data$area<-gArea(patches_with_location,byid = TRUE )

cdata <- ddply(patches_with_location@data,"id", summarise, N    = length(layer),A=sum(area))
sub_gridspdf@data$area<-gArea(sub_gridspdf,byid = TRUE)
density<-cdata[3]/sub_gridspdf@data$area

#mean height  - value  The order of the returned values corresponds to the order of object y. If df=TRUE, this is also indicated in the ﬁrst variable (’ID’).

CHM_in_patches <- extract(CHM, p)
max_perunit <-sapply(CHM_in_patches, function(x) if (!is.null(x)) { max(x)} else NA)
max_spdf <- spCbind(patches_with_location, max_perunit)
mean_max <- ddply(max_spdf@data,"id", summarise,max = max(max_perunit))

# soil cover
soil<-rc<2
soil_list <- extract(soil, sub_gridspdf)
soil_cover<-sapply(soil_list, function(x) if (!is.null(x)) { sum(x)/length(x)} else NA)

#raster to polygon
fun2=function(x){x<2}
poly2<-rasterToPolygons(rc, fun2, dissolve=TRUE)
#explode
SOIL<-ms_explode(poly2)
SOIL$Area_sqm <- area(SOIL)
#head(SOIL)
newdata <- SOIL[which(SOIL@data$Area_sqm >10),]

#soil cover
soil_cover<-sum(newdata$Area_sqm)

ground_density<-nrow(newdata@data)



####using point cloud  ####




####canopy_cover<-(cellStats(canopy,stat="sum")/length(CHM))*100
CHM_in_grid <- extract(CHM, sub_gridspdf)
canopy_cover<-sapply(v, function(x) if (!is.null(x)) { sum(x)/length(x)} else NA)
pioneers_density<-length(pt_CHM@data[pt_CHM@data$V3>4 & pt_CHM@data$V3 < 6,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100



#encroachment
shrub_density<-length(pt_CHM@data[pt_CHM@data$V3>0.5 & pt_CHM@data$V3 < 4,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100




########Shannon diversity 
#classes
suite1<-c(0:14)
suite2<-c(1:15)
suite3<-c(1:15)
mat<-cbind(suite1,suite2,suite3)
mat2<-cbind(mat,NA)
mat2<-cbind(mat2,NA)
bins <- reclassify(CHM, mat)
v<-1
for(v in suite3){
  bin<-bins==v
  p<-(cellStats(bin,stat="sum")/length(CHM))
  p2<-p*log(p)
  mat2[v,4]<-p
  mat2[v,5]<-p2
}


cinco<-mat2[,5]
vertical_complexity<--sum(cinco,na.rm = TRUE)

quantiles<-quantile(pt_CHM@data$V3, c(.01,.05,0.1,.30,0.5, .60,.90, .99))

last_percentile<-quantiles[[8]]

difference_height<-quantiles[[7]]-quantiles[[3]]

standard_deviation<-sd(pt_CHM@data$V3, na.rm = FALSE)



vertical_distribution<-(last_percentile-median(pt_CHM@data$V3))/last_percentile

