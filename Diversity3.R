setwd("/Users/TingleyLab/Dropbox/Work/Diversity_accum")
library(sp)
library(raster)


load("xy_prop_meta_2_new.Rdata")  # Warning: this is a large object!
load("cs2_meta_new.Rdata")
load("/Users/TingleyLab/Dropbox/Work/Diversity_accum/points.Rdata") 

points2 <- points[2:length(points)]

load("world_conts2.Rdata")
world.conts3 <- spTransform(world.conts2, crs("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))
save(world.conts3, file="world_conts3.Rdata")

##### Load in masking polygons for large lakes #####
#Unzip it
unzip("worldglwd1.zip", exdir="Caspian")
#Load it
Caspian <- maptools::readShapeSpatial("Caspian/worldglwd1.shp", proj4string=crs)
Caspian <- sp::spTransform(Caspian, CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))
#Plot it



#Unzip it
unzip("ne_10m_lakes.zip", exdir="lakes")
#Load it
lakes <- maptools::readShapeSpatial("lakes/ne_10m_lakes.shp", proj4string=crs)
lakes <- sp::spTransform(lakes, CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))
#Plot it

#plot(lakes[2,])

areas <- vector()
for(i in 1:length(lakes)){
  areas[i] <- area(lakes[i,])
}

top <- vector()
for(i in 1:30){
  top[i] <- order(areas)[length(areas)+1-i]
  print(lakes$name[top[i]])
}

all.lakes <- Caspian
for(i in 1:16){
  all.lakes <- rgeos::gUnion(all.lakes, lakes[top[i],])
}

save(all.lakes, file="all_lakes.Rdata")
load("all_lakes.Rdata")

load("GElle.Rdata")
#######################

for(tree_i in 1:30){
  print(tree_i)
  L5ma <- vector()
  for(i in 1:length(xy.prop.meta.2[[tree_i]][[1]])){
    if(! is.na(xy.prop.meta.2[[tree_i]][[1]][[i]])){
      L5ma[i] <- xy.prop.meta.2[[tree_i]][[1]][[i]][min(which(xy.prop.meta.2[[tree_i]][[1]][[i]][,1] > -5)),][2]
    }
    else{L5ma[i] <- NA
    }
  }
  
  allriches <- cs2.meta[[tree_i]]  # renaming the total richness across points
  ddd <- data.frame(L5ma=L5ma)
  breeding_points_df <- SpatialPointsDataFrame(points2, data=ddd)
  bpdf_reproj <- spTransform(breeding_points_df, crs("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))
  bpdf_reproj$allrich <- allriches[[1]]

#save(bpdf_reproj, file="bpdf_reproj.Rdata")
  bpdf_reproj[which(!is.na(over(bpdf_reproj, all.lakes))),] <- NA
  bpdf_reproj[which(!is.na(over(bpdf_reproj, GElle))),] <- NA
  save(bpdf_reproj, file=paste("bpdf_reproj_", tree_i, ".Rdata", sep=""))
}

breeding_rasterP <- raster::rasterize(bpdf_reproj, raster(nrows=5000, ncols=10000, ext=extent(world.conts3)), fun=mean, na.rm=T)
save(breeding_rasterP, file="breeding_rasterP.Rdata")

smooth.rasters <- as.list(rep(NA, 2))
names(smooth.rasters) <- names(breeding_rasterP)[2:3]
masked.rasters <- smooth.rasters
raster.points <- smooth.rasters
points.df <- smooth.rasters

for(i in 1:2){#10){
  ras <- subset(breeding_rasterP, i+1)
  smooth.rasters[[i]] <- focal(ras, w=matrix(1, 51, 51), mean, na.rm=T)
  msr <- mask(smooth.rasters[[i]], world.conts3)
  msr2 <- mask(msr, all.lakes, inverse=TRUE)
  writeRaster(msr2, file=paste("msr2NEW_",i,".grd", sep=""), overwrite=T)
  masked.rasters[[i]] <- raster(paste("msr2NEW_",i,".grd", sep=""))
  print(i)
}

for(i in 1:2){#10){
  raster.points[[i]] <- rasterToPoints(masked.rasters[[i]])
  points.df[[i]] <- data.frame(raster.points[[i]])
  colnames(points.df[[i]]) <- c("Longitude", "Latitude", "Values")
  print(i)
}

for(i in 1:2){#10){
  points.df[[i]]$RichnessCont <- 1-points.df[[i]]$Values
}

save(points.df, file=paste("points_df", tree_i, ".Rdata", sep=""))
