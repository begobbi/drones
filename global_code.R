# rm(list=ls())

library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(TRAMPR)
library(rmapshaper)
setwd

CRS.new <- CRS("+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
coordenadas.name<-"coords_all"
coordenadas.df<-read.table(paste(coordenadas.name,".csv",sep=""),sep=";",header = TRUE,stringsAsFactors = FALSE)
coordenadas.sp<-SpatialPoints(coordenadas.df[,c(4,3)])
coordenadas.spdf<-SpatialPointsDataFrame(coordenadas.sp,coordenadas.df)
coordenadas.spdf@proj4string<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
coordspro<-spTransform(coordenadas.spdf, CRS.new)

#zone<-"zone7"
#zones

#zones<-c("zone11","zone12","zone13","zone14")
zones<-c("zone3","zone4","zone5","zone7","zone8")
list.buffer<-c("A","B","C","D")
results<-data.frame()

  results.list=list(0)
#zone<-zones[1]
for (zone in zones){
#  zone<-"zone7"
  current_raster<-raster(paste0(zone,".tif"))
  rasterpro<-projectRaster(current_raster,crs=CRS.new)
  
  ptcloud.df<-read.table(paste0(zone, ".txt"))
  ptcloud.sp<-SpatialPoints(ptcloud.df[,c(1,2)])
  ptcloud.spdf<-SpatialPointsDataFrame(ptcloud.sp,ptcloud.df)
  proj4string(ptcloud.spdf) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # assign WGS 84
  ptcloudpro <- spTransform(ptcloud.spdf, CRS.new)
  
  
# A b C d  pour boucle
#list.buffer<-coordspro@data[which(coordspro@data$zone_numb == zone&coordspro@data$letra != "A"),"letra"]

# buffer<-list.buffer[1]
##

for (buffer in list.buffer){
  #crop point cloud

  pt.buffer<-coordspro[which(coordspro@data$zone_numb == zone & coordspro@data$letra == buffer),]
  poly.buffer<-gBuffer(pt.buffer,width = 17.84)
  #crop raster
  raster.crop<-crop(rasterpro,poly.buffer)
  raster.mask<-mask(raster.crop,poly.buffer)

  
  ############################CORRECTING dem#############################
  
  #USING FIRST PERCENTILE
 
   first<-quantile(raster.mask, c(.015)) 
  CHM<-raster.mask-first
  
  ############################END CORRECTING DEM ########################
  
  
  #crop nuage de pts
  ptcloud.crop<-ptcloudpro[poly.buffer,]
  pt_CHM<-ptcloud.crop
  pt_CHM@data$V3<-ptcloud.crop@data$V3-first
  

  
  # #classes
  # m2 <- c(-20, 0.5, 1, 0.5, 1,2,1,2,3,2,4,4,  4, 6,5,6,20,6)
  # rclmat <- matrix(m2, ncol=3, byrow=TRUE)
  # rc <- reclassify(CHM, rclmat)
  # 
  # #high canopy class
  # fun1=function(x){x>5}
  # poly<-rasterToPolygons(rc, fun1,dissolve=TRUE)
  # 
  # out<-ms_explode(poly)
  # out$Area_sqm <- area(out)
  # 
  # 
  # #canopy cover
  # #canopy<-sum(out@data$Area_sqm)
  # #canopy_cover<-(canopy/area(poly.buffer))*100
  # r<-CHM>6
  # canopy_cover<-(cellStats(r,stat="sum")/length(CHM))*100
  # # density of patches 
  # density_high_canopy<-(nrow(out@data)/area(poly.buffer))/10
  # #mean height 
  # zonal<-zonal.stats(out,CHM,max )
  # mean_max_height<-mean(zonal)
  # #proliferation of pioneers
  # pt_CHM@data$V3<-pt_CHM@data$V3-1.75
  # pioneers_density<-length(pt_CHM@data[pt_CHM@data$V3>4 & pt_CHM@data$V3 < 6,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100
  # #encroachment
  # shrub_density<-length(pt_CHM@data[pt_CHM@data$V3>0.5 & pt_CHM@data$V3 < 4,"V3"])/length(pt_CHM@data[pt_CHM@data$V3 > 0,"V3"])*100
  # #process for soil percentage
  # #raster to polygon
  # fun2=function(x){x<2}
  # poly2<-rasterToPolygons(rc, fun2, dissolve=TRUE)
  # 
  # 
  # 
  # #explode
  # SOIL<-ms_explode(poly2)
  # SOIL$Area_sqm <- area(SOIL)
  # #head(SOIL)
  # #newdata <- SOIL[which(SOIL@data$Area_sqm >1),]
  # 
  # 
  # #soil cover
  # #soil<-sum(newdata$Area_sqm)
  # #soil_cover<-(sum(area(SOIL))/area(poly.buffer))*100
  # s<-CHM<2
  # so<-(cellStats(s,stat="sum")/length(CHM))*100
  # soil_cover<-(cellStats(s,stat="sum")/length(CHM))*100
  # # density of ground 
  # 
  # ground_density<-(nrow(SOIL@data)/area(poly.buffer))/10
  # 
  
  ########Shannon diversity 
  #classes
  # suite1<-c(0:14)
  # suite2<-c(1:15)
  # suite3<-c(1:15)
  # mat<-cbind(suite1,suite2,suite3)
  # mat<-cbind(mat,NA)
  # mat<-cbind(mat,NA)
  # bins<-classify(pt_CHM$data@V3)
  # bins <- reclassify(CHM, mat)
  # v<-1
  # for(v in suite3){
  #   bin<-bins==v
  #   p<-(cellStats(bin,stat="sum")/length(CHM))
  #   p2<-p*log(p)
  #   mat[v,4]<-p
  #   mat[v,5]<-p2
  # }
  # 
  # 
  # cinco<-mat[,5]
  # HI<--sum(cinco,na.rm = TRUE)
  # 
  # quantiles<-quantile(pt_cloud, c(.01,.05,0.1,.30,0.5, .60,.90, .99))
  # 
  
  ### zone
  
  
  #co<-data.frame(zone=zone,buffer=buffer, canopy_cover=canopy_cover, density_high_canopy=density_high_canopy,mean_max_height=mean_max_height,pioneers_density=pioneers_density,shrub_density=shrub_density, soil_cover=soil_cover,  ground_density=ground_density,stringsAsFactors=F)
  # colum_res<-c(zone,buffer, canopy_cover, density_high_canopy,mean_max_height,pioneers_density,shrub_density, soil_cover,  ground_density, quantiles)
  #colum_res[3:9]<-as.numeric(colum_res[3:9])
  #colnames(results)<-c("zone","buffer","canopy_cover","density_high_canopy","mean_max_height","pioneers_density","shrub_density","soil_cover","ground_density")
  #op <- options(stringsAsFactors=F) 
  #results<-rbind(results,co)
  
  # results.list[[i]]<-colum_res
  
  # res<-data.frame()
  #res<-rbind(res,colum_res)
  #res
  # colnames(res)<-c("zone","buffer","canopy_cover","density_high_canopy","mean_max_height","pioneers_density","shrub_density","soil_cover","ground_density")
  # 
   writeRaster(CHM,paste0(zone, buffer)     ,format="GTiff",overwrite=TRUE)
  writeOGR(pt_CHM,work.dir,paste0(zone, buffer),driver="ESRI Shapefile",overwrite=TRUE)
  #results<-data.frame()
   }
  
 }
  colum_res
  


res<-results.list
#res[[1]]<-NULL
test<-t(as.data.frame(res))
test
rownames(test)<-c("zone","buffer","canopy_cover","density_high_canopy","mean_max_height","pioneers_density","shrub_density","soil_cover","ground_density")
colnames(test)<-c("1","2","3")
write.table(res,file="file.csv")
    
  # }
    
 
  