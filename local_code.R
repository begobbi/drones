rm(list=ls())

library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(knitr)
library(geojsonio)
library(spatialEco)
library(rmapshaper)
getwd()

setwd("C:/Users/bgobbi/Nextcloud/RStudio/R/Data_processing")

CRS.new <- CRS("+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
coordenadas.name<-"coords_all"
coordenadas.df<-read.table(paste(coordenadas.name,".csv",sep=""),sep=";",header = TRUE,stringsAsFactors = FALSE)
coordenadas.sp<-SpatialPoints(coordenadas.df[,c(4,3)])
coordenadas.spdf<-SpatialPointsDataFrame(coordenadas.sp,coordenadas.df)
coordenadas.spdf@proj4string<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
coordspro<-spTransform(coordenadas.spdf, CRS.new)

zones<-c("zone1","zone2","zone3","zone4","zone5","zone6","zone7","zone8","zone10", "zone11","zone12","zone13")
list.buffer<-c("A","B","C","D")

results.list<-list()
i<-0

for (zone in zones){

  # A b C d  pour boucle
  list.buffer<-coordspro@data[which(coordspro@data$zone_numb == zone),"letra"]
  # buffer<-list.buffer[1]
  ##
  
  for (buffer in list.buffer){
    i<-i+1
  CHM<-raster(paste0(zone,buffer,".tif"))
  pt_CHM<-readOGR(paste0(zone,buffer,".shp"))
  
  #classes
  m2 <- c(-20, 0.5, 1, 0.5, 1,2,1,2,3,2,4,4,  4, 6,5,6,20,6)
  rclmat <- matrix(m2, ncol=3, byrow=TRUE)
  rc <- reclassify(CHM, rclmat)
  
  #high canopy class
  fun1=function(x){x>5}
  poly<-rasterToPolygons(rc, fun1,dissolve=TRUE)
  
  out<-ms_explode(poly)
  
  
  #############canopy cover####################
  
  r<-CHM>5
  canopy_cover<-(cellStats(r,stat="sum")/length(CHM))*100
  # density of patches 
  density_high_canopy<-(nrow(out@data))
  #mean height 
  zonal<-zonal.stats(out,CHM,max )
  mean_max_height<-mean(zonal)
  #proliferation of pioneers
  pioneers_density<-length(pt_CHM@data[pt_CHM@data$V3>4 & pt_CHM@data$V3 < 6,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100
  #encroachment
  shrub_density<-length(pt_CHM@data[pt_CHM@data$V3>0.5 & pt_CHM@data$V3 < 4,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100
  #process for soil percentage
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
 
colum_res<-c(zone,buffer, canopy_cover, density_high_canopy,mean_max_height,pioneers_density,shrub_density, soil_cover,  ground_density, last_percentile,difference_height,standard_deviation ,vertical_complexity,vertical_distribution)


 
 results.list[[i]]<-colum_res
    

  }
 
 
}

res<-results.list
ver<-as.data.frame(res)
ver
write.table(res,file="resultats4.csv")
