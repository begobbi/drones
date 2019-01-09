### remame folders ###

liste<-list.files(path = "/home/users/b/g/bgobbi/R/INPUTS/2018")
liste


library(raster)


# example data
x <- raster(system.file("external/test.grd", package="raster"))
To get the rectangular extent

e <- extent(x)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
To get a polygon that surrounds cells that are not NA

# make all values the same. Either do
r <- x > -Inf
# or alternatively
# r <- reclassify(x, cbind(-Inf, Inf, 1))

# convert to polygons (you need to have package 'rgeos' installed for this to work)
pp <- rasterToPolygons(r, dissolve=TRUE)

# look at the results
plot(x)
plot(p, lwd=5, border='red', add=TRUE)
plot(pp, lwd=3, border='blue', add=TRUE)
