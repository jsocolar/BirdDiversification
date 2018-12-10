## Diversification contribution analysis part 1: extract species lists

library(maptools)
library(rgdal)
library(sp)
library(rgeos)
library(ape)
library(phytools)
library(coda)
library(mgcv)
library(geosphere)
library(ncf)
library(raster)
library(ggplot2)
"%ni%" <- Negate("%in%")
setwd("/Users/JacobSocolar/Dropbox/Work/Diversity_accum")
crs <- CRS("+proj=longlat +datum=WGS84")
crs2 <- CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")



########### Part 1: Get lists of breeding birds for equally spaced points across global continental land masses ##############

bird_ranges <- readOGR("/Users/JacobSocolar/Desktop/useful_datasets/BOTW_2017/BOTW.gdb")
save(bird_ranges, file = "bird_ranges.Rdata")
#Download the continents shapefile
download.file("http://faculty.baruch.cuny.edu/geoportal/data/esri/world/continent.zip",
              "continents/cont.zip")
#Unzip it
unzip("continents/cont.zip", exdir="continents")
#Load it
cont <- readOGR("continents/continent.shp")
cont <- spTransform(cont, crs)
C <- 1
# Extract areas of individual polygons belonging to continent 1
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]   # Index of polygon w/ largest area
areas[top]



p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
ps <-sp:: Polygons(list(p), 1)
Africa <- sp::SpatialPolygons(list(ps), proj4string=crs)
plot(Africa, col="red")




C <- 2
# Extract areas of individual polygons belonging to continent 2
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]   # Index of polygon w/ largest area
areas[top]
top2 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-1]   # 2nd largest area
areas[top2]
top3 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-2]   # 3nd largest area
areas[top3]
top4 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-3]   # 4th largest area
areas[top4]
top5 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-4]   # 5th largest area
areas[top5]
top6 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-5]   # 6th largest area
areas[top6]
top7 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-6]   # 7th largest area
areas[top7]
top8 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-7]   # 8th largest area
areas[top8]
top9 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-8]   # 9th largest area
areas[top9]
top10 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-9]   # 10th largest area
areas[top10]


# Double-checking which islands these are based on area:
rgeos::gArea(SpatialPolygons(list(Polygons(list(cont[C,]@polygons[[1]]@Polygons[[top6]]),1))))


p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
ps <-sp:: Polygons(list(p), 1)
Asia <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Asia, col='red')

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top2]]@coords)
ps <-sp:: Polygons(list(p), 1)
NG <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(NG, col='red')

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top3]]@coords)
ps <-sp:: Polygons(list(p), 1)
Borneo <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Borneo, col='red')

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top4]]@coords)
ps <-sp:: Polygons(list(p), 1)
Sumatra <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Sumatra, col='red')

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top5]]@coords)
ps <-sp:: Polygons(list(p), 1)
Honshu <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Honshu, col='red')

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top6]]@coords)
ps <-sp:: Polygons(list(p), 1)
Russia <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Russia, col='red')

# top7 is Sulawesi

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top8]]@coords)
ps <-sp:: Polygons(list(p), 1)
Java <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Java, col='red', add=F)


C <- 3
# Extract areas of individual polygons belonging to continent 4
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]   # Index of polygon w/ largest area
areas[top]


p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
ps <-sp:: Polygons(list(p), 1)
Australia <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Australia, add=F)



C <- 4
# Extract areas of individual polygons belonging to continent 4
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]   # Index of polygon w/ largest area
areas[top]
top2 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-1]   # 2nd largest area
areas[top2]
top3 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-2]   # 3nd largest area
areas[top3]
top4 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-3]   # 4th largest area
areas[top4]
top5 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-4]   # 5th largest area
areas[top5]
top6 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-5]   # 5th largest area
areas[top6]

# Double-checking which islands these are based on area:
rgeos::gArea(SpatialPolygons(list(Polygons(list(cont[C,]@polygons[[1]]@Polygons[[top6]]),1))))


p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
ps <-sp:: Polygons(list(p), 1)
N.Am <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(N.Am, add=F)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top2]]@coords)
ps <-sp:: Polygons(list(p), 1)
Greenland <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Greenland, add=T)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top3]]@coords)
ps <-sp:: Polygons(list(p), 1)
Baffin <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Baffin, add=T)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top4]]@coords)
ps <-sp:: Polygons(list(p), 1)
Victoria <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Victoria, add=T)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top5]]@coords)
ps <-sp:: Polygons(list(p), 1)
Ellesmere <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Ellesmere, add=T)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top6]]@coords)
ps <-sp:: Polygons(list(p), 1)
Newfoundland <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Newfoundland, add=T)

# C <- 5 is oceania


C <- 6
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
S.ps <- sp::Polygons(list(p), 1)
S.Am <- sp::SpatialPolygons(list(S.ps), proj4string=crs)
#plot(S.Am, add=F)


# C <- 7 is Antarctica

C <- 8
# Extract areas of individual polygons belonging to continent 8
areas <- vector()
for(i in 1:length(cont[C,]@polygons[[1]]@Polygons)){
  areas[i] <- cont[C,]@polygons[[1]]@Polygons[[i]]@area
}
top <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)]   # Index of polygon w/ largest area
areas[top]
top2 <- order(areas)[length(cont[C,]@polygons[[1]]@Polygons)-1]   # 2nd largest area
areas[top2]

# Double-checking which islands these are based on area:
rgeos::gArea(SpatialPolygons(list(Polygons(list(cont[C,]@polygons[[1]]@Polygons[[top2]]),1))))


p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top]]@coords)
ps <-sp:: Polygons(list(p), 1)
Europe <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Europe, add=F)

p <- sp::Polygon(cont[C,]@polygons[[1]]@Polygons[[top2]]@coords)
ps <-sp:: Polygons(list(p), 1)
Britain <- sp::SpatialPolygons(list(ps), proj4string=crs)
#plot(Britain, add=T)



N.Am <- rgeos::gUnion(N.Am, Greenland)
N.Am <- rgeos::gUnion(N.Am, Baffin)
N.Am <- rgeos::gUnion(N.Am, Victoria)
N.Am <- rgeos::gUnion(N.Am, Ellesmere)
N.Am <- rgeos::gUnion(N.Am, Newfoundland)

S.Am <- S.Am

Asia <- rgeos::gUnion(Asia, Borneo)
Asia <- rgeos::gUnion(Asia, Sumatra)
Asia <- rgeos::gUnion(Asia, Honshu)
Asia <- rgeos::gUnion(Asia, Java)
Asia <- rgeos::gUnion(Asia, Russia)

Asia2 <- raster::crop(Asia, raster::extent(-179.99, 179.99, -55, 85))

Australia <- rgeos::gUnion(Australia, NG)

Europe <- rgeos::gUnion(Europe, Britain)

Africa <- Africa

Americas <- rgeos::gUnion(S.Am, N.Am)
Eurasia <- rgeos::gUnion(Europe, Asia)
Eurasia2 <- rgeos::gUnion(Europe, Asia2)

world.conts <- rgeos::gUnion(Americas,Africa)
world.conts <- rgeos::gUnion(world.conts,Eurasia)
world.conts <- rgeos::gUnion(world.conts,Australia)

world.conts2 <- raster::crop(world.conts, raster::extent(-179.99, 179.99, -55, 85))
save(world.conts2, file="world_conts2.Rdata")



Americas <- spTransform(Americas, crs2)
Africa <- spTransform(Africa, crs2)
Eurasia2 <- spTransform(Eurasia2, crs2)
Australia <- spTransform(Australia, crs2)
N.America <- spTransform(N.Am, crs2)
S.America <- spTransform(S.Am, crs2)

continents <- list(Americas=Americas, Eurasia2=Eurasia2, Africa=Africa, Australia=Australia, N.America=N.America, S.America=S.America)

save(continents, file="continents.Rdata")

load("continents.Rdata")
load("world_conts2.Rdata")


points <- sp::spsample(world.conts2, 40000, type="Fibonacci")
#plot(points)
length(points)

breeding_ranges <- bird_ranges[which(bird_ranges$SEASONAL %in% c(1,2)), ]
breeding_ranges <- spTransform(breeding_ranges, crs2)
#breeding_ranges <- raster::aggregate(breeding_ranges, by = 'SCINAME')
# above line returns error: 
# Error in spChFIDs(z, as.character(y)) : lengths differ
scinames <- unique(breeding_ranges$SCINAME)
mem.shapes <- list()
for(i in 1:length(scinames)){
  print(i)
  print(scinames[i])
  species_i <- breeding_ranges[which(breeding_ranges$SCINAME == scinames[i]), ]
  if(i %in% c(1106,3785,5471,6385,6478,6518,6529,6545,6548,6928,8980,9051,9977)){ # This is a workaround for dealing with invalid geometries (i.e. self-intersecting or containing a line segment of zero length)
    species_i <- spTransform(species_i, crs2)
    species_i <- rgeos::gBuffer(species_i, width = 0)
    species_i2 <- raster::aggregate(species_i)
    species_i2 <- spTransform(species_i2, CRS(proj4string(bird_ranges)))
  }else{
    species_i2 <- raster::aggregate(species_i)
  }
  mem.shapes[[i]] <- species_i2
}

save(mem.shapes, file="/Users/Tingleylab/Dropbox/Work/Diversity_accum/mem_shapes_new.Rdata")

breeding_point_data <- as.data.frame(matrix(data=NA, nrow=length(scinames), 
                                            ncol=(length(points)+1)))
breeding_point_data[ ,1] <- gsub(as.character(scinames[1]), " ", "_")


points <- spTransform(points, CRS(proj4string(mem.shapes[[1]])))
for(ii in 0:110){
  for(iii in 1:100){
    i <- 100*ii + iii
    print(i)
    breeding_point_data[i, 1] <- gsub(" ", "_", as.character(scinames[i]))
    breeding_point_data[i, 2:(length(points)+1)] <- tryCatch(!is.na(sp::over(points, spTransform(mem.shapes[[i]], CRS(proj4string(points))))),
                                                             error=function(e){rep(FALSE, length(points))})
  }
}
save(breeding_point_data, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_data_new.Rdata")


for(i in 11101:11116){
  print(i)
  breeding_point_data[i, 1] <- gsub(" ", "_", as.character(scinames[i]))
  breeding_point_data[i, 2:(length(points)+1)] <- tryCatch(!is.na(sp::over(points, spTransform(mem.shapes[[i]], CRS(proj4string(points))))),
                                                           error=function(e){rep(FALSE, length(points))})
}


save(breeding_point_data, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_data_new.Rdata")


for(i in 2:dim(breeding_point_data)[2]){
  breeding_point_data[ ,i] <- as.integer(as.logical(breeding_point_data[,i]))
}

save(breeding_point_data, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_data_new.Rdata")
save(points, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/points.Rdata")

load("breeding_point_data_new.Rdata")
load("points.Rdata")

length(which(rowSums(breeding_point_data[,2:dim(breeding_point_data)[2]])>0))

breeding_point <- breeding_point_data[which(rowSums(breeding_point_data[,2:dim(breeding_point_data)[2]])>0), ]

####### Part 2: Cleaning data to match birdtree species names #########
#PTrees <- list()
#for(i in 1:30){
#  PTrees[[i]] <- ape::read.tree(file=
#                                  paste("/Users/Tingleylab/Desktop/useful_datasets/Pulido_phylogeny/JETZ TREES/Jetz_with_Furnariidae_March2013__tree",
#                                        i,".txt", sep = ""))
#}

#save(PTrees, file="/Users/Tingleylab/Dropbox/Work/Diversity_accum/PTrees.Rdata")

load("PTrees.Rdata")
PTrees1 <- PTrees[[1]]
load("breeding_point_data_new.Rdata")

breeding_point <- breeding_point_data[which(rowSums(breeding_point_data[,2:dim(breeding_point_data)[2]])>0), ]

"%ni%" <- Negate("%in%")

length(which(breeding_point[,1] %ni% PTrees[[1]]$tip.label))

breeding_point[,1] <- gsub("Acanthidops_bairdi", "Acanthidops_bairdii", breeding_point[,1])
breeding_point[,1] <- gsub("Acanthis_flammea", "Carduelis_flammea", breeding_point[,1])
breeding_point[,1] <- gsub("Acanthiza_cinerea", "Gerygone_cinerea", breeding_point[,1])
breeding_point[,1] <- gsub("Acanthoptila_nipalensis", "Turdoides_nipalensis", breeding_point[,1])

S1 <- "Accipiter_novaehollandiae"     # Species in breeding_point & HTrees1
S2 <- "Accipiter_hiogaster"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Acridotheres_burmannicus", "Sturnus_burmannicus", breeding_point[,1])
breeding_point[,1] <- gsub("Acridotheres_javanicus", "Acridotheres_cinereus", breeding_point[,1])

S1 <- "Sturnus_burmannicus"     # Species in breeding_point & HTrees1
S2 <- "Acridotheres_leucocephalus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Acridotheres_melanopterus", "Sturnus_melanopterus", breeding_point[,1])

S1 <- "Sturnus_melanopterus"     # Species in breeding_point & HTrees1
S2 <- "Acridotheres_tricolor"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Acritillas_indica", "Iole_indica", breeding_point[,1])

S1 <- "Acrocephalus_arundinaceus"     # Species in breeding_point & HTrees1
S2 <- "Acrocephalus_orientalis"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Actinodura_ramsayi"     # Species in breeding_point & HTrees1
S2 <- "Actinodura_radcliffei"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aegithalos_concinnus"     # Species in breeding_point & HTrees1
S2 <- "Aegithalos_annamensis"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aegithalos_iouschistos"     # Species in breeding_point & HTrees1
S2 <- "Aegithalos_bonvaloti"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aegithalos_caudatus"     # Species in breeding_point & HTrees1
S2 <- "Aegithalos_glaucogularis"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aegithalos_concinnus"     # Species in breeding_point & HTrees1
S2 <- "Aegithalos_iredalei"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Aerodramus_", "Collocalia_", breeding_point[,1])
breeding_point[,1] <- gsub("Aethopyga_latouchii", "Aethopyga_christinae", breeding_point[,1])

S1 <- "Aethopyga_mystacalis"     # Species in breeding_point & HTrees1
S2 <- "Aethopyga_temminckii"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aethopyga_siparaja"     # Species in breeding_point & HTrees1
S2 <- "Aethopyga_vigorsii"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Afrotis_afra", "Eupodotis_afra", breeding_point[,1])
breeding_point[,1] <- gsub("Afrotis_afraoides", "Eupodotis_afraoides", breeding_point[,1])

S1 <- "Agelaioides_badius"     # Species in breeding_point & HTrees1
S2 <- "Agelaioides_fringillarius"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Aglaiocercus_kingii", "Aglaiocercus_kingi", breeding_point[,1])
breeding_point[,1] <- gsub("Agraphospiza_rubescens", "Carpodacus_rubescens", breeding_point[,1])
breeding_point[,1] <- gsub("Agricola_infuscatus", "Bradornis_infuscatus", breeding_point[,1])
breeding_point[,1] <- gsub("Agricola_pallidus", "Bradornis_pallidus", breeding_point[,1])
breeding_point[,1] <- gsub("Agropsar_philippensis", "Sturnus_philippensis", breeding_point[,1])
breeding_point[,1] <- gsub("Agropsar_sturninus", "Sturnus_sturninus", breeding_point[,1])
breeding_point[,1] <- gsub("Akletos_", "Myrmeciza_", breeding_point[,1])
breeding_point[,1] <- gsub("Alauda_leucoptera", "Melanocorypha_leucoptera", breeding_point[,1])
breeding_point[,1] <- gsub("Alaudala_", "Calandrella_", breeding_point[,1])

S1 <- "Alcedo_euryzona"     # Species in breeding_point & HTrees1
S2 <- "Alcedo_peninsulae"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Alethe_diademata"     # Species in breeding_point & HTrees1
S2 <- "Alethe_castanea"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Alopecoenas_beccarii", "Gallicolumba_beccarii", breeding_point[,1])
breeding_point[,1] <- gsub("Alopecoenas_jobiensis", "Gallicolumba_jobiensis", breeding_point[,1])

S1 <- "Alophoixus_pallidus"     # Species in breeding_point & HTrees1
S2 <- "Alophoixus_griseiceps"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Alophoixus_ochraceus"     # Species in breeding_point & HTrees1
S2 <- "Alophoixus_ruficrissus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Alophoixus_bres"     # Species in breeding_point & HTrees1
S2 <- "Alophoixus_tephrogenys"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Amaurornis_cinerea", "Porzana_cinerea", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_marginalis", "Aenigmatolimnas_marginalis", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurospiza_relicta", "Amaurospiza_concolor", breeding_point[,1])

S1 <- "Amazilia_lactea"
S2 <- "Amazilia_bartletti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Amazilia_viridigaster"
S2 <- "Amazilia_cupreicauda"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Amazilia_grayi", "Hylocharis_grayi", breeding_point[,1])
breeding_point[,1] <- gsub("Amazilia_humboldtii", "Hylocharis_humboldtii", breeding_point[,1])
breeding_point[,1] <- gsub("Amazilia_sapphirina", "Hylocharis_sapphirina", breeding_point[,1])
breeding_point[,1] <- gsub("Amazilia_saucerottei", "Amazilia_saucerrottei", breeding_point[,1])

S1 <- "Amazilia_viridifrons"     # Species in breeding_point & HTrees1
S2 <- "Amazilia_wagneri"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Amazona_festiva"
S2 <- "Amazona_bodini"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Amazona_autumnalis"
S2 <- "Amazona_diadema"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Amazona_farinosa"
S2 <- "Amazona_guatemalae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Amazona_autumnalis"
S2 <- "Amazona_lilacina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Amazona_mercenarius", "Amazona_mercenaria", breeding_point[,1])
breeding_point[,1] <- gsub("Ammomanopsis_grayi", "Ammomanes_grayi", breeding_point[,1])
breeding_point[,1] <- gsub("Ammonastes_pelzelni", "Myrmeciza_pelzelni", breeding_point[,1])
breeding_point[,1] <- gsub("Ammospiza_caudacuta", "Ammodramus_caudacutus", breeding_point[,1])
breeding_point[,1] <- gsub("Ammospiza_leconteii", "Ammodramus_leconteii", breeding_point[,1])
breeding_point[,1] <- gsub("Ammospiza_maritima", "Ammodramus_maritimus", breeding_point[,1])
breeding_point[,1] <- gsub("Ammospiza_nelsoni", "Ammodramus_nelsoni", breeding_point[,1])
breeding_point[,1] <- gsub("Ampelornis_griseiceps", "Myrmeciza_griseiceps", breeding_point[,1])
breeding_point[,1] <- gsub("Amphispiza_quinque", "Aimophila_quinque", breeding_point[,1])

S1 <- "Amytornis_textilis"     # Species in breeding_point & HTrees1
S2 <- "Amytornis_modestus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Anabacerthia_lichtensteini", "Philydor_lichtensteini", breeding_point[,1])
breeding_point[,1] <- gsub("Anabacerthia_ruficaudata", "Philydor_ruficaudatum", breeding_point[,1])
breeding_point[,1] <- gsub("Anabathmis_reichenbachii", "Nectarinia_reichenbachii", breeding_point[,1])

S1 <- "Anaplectes_rubriceps"     # Species in breeding_point & HTrees1
S2 <- "Anaplectes_jubaensis"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Anaplectes_rubriceps"     # Species in breeding_point & HTrees1
S2 <- "Anaplectes_leuconotos"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Anas_poecilorhyncha"     # Species in breeding_point & HTrees1
S2 <- "Anas_zonorhyncha"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Anisognathus_somptuosus"     # Species in breeding_point & HTrees1
S2 <- "Anisognathus_flavinucha"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Anisognathus_igniventris"     # Species in breeding_point & HTrees1
S2 <- "Anisognathus_lunulatus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Anser_caerulescens", "Chen_caerulescens", breeding_point[,1])
breeding_point[,1] <- gsub("Anser_canagicus", "Chen_canagica", breeding_point[,1])
breeding_point[,1] <- gsub("Anser_cygnoid", "Anser_cygnoides", breeding_point[,1])
breeding_point[,1] <- gsub("Anser_rossii", "Chen_rossii", breeding_point[,1])
breeding_point[,1] <- gsub("Anthipes_monileger", "Ficedula_monileger", breeding_point[,1])
breeding_point[,1] <- gsub("Anthipes_solitaris", "Ficedula_solitaris", breeding_point[,1])
breeding_point[,1] <- gsub("Anthobaphes_violacea", "Nectarinia_violacea", breeding_point[,1])
breeding_point[,1] <- gsub("Anthochaera_phrygia", "Xanthomyza_phrygia", breeding_point[,1])

S1 <- "Anthoscopus_caroli"     # Species in breeding_point & HTrees1
S2 <- "Anthoscopus_sylviella"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Anthreptes_aurantius", "Anthreptes_aurantium", breeding_point[,1])
breeding_point[,1] <- gsub("Anthreptes_seimundi", "Nectarinia_seimundi", breeding_point[,1])

S1 <- "Anthreptes_rectirostris"     # Species in breeding_point & HTrees1
S2 <- "Anthreptes_tephrolaemus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Anthropoides_paradiseus", "Grus_paradisea", breeding_point[,1])
breeding_point[,1] <- gsub("Anthropoides_virgo", "Grus_virgo", breeding_point[,1])

S1 <- "Anthus_hoeschi"     # Species in breeding_point & HTrees1
S2 <- "Anthus_cinnamomeus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Anthus_similis"     # Species in breeding_point & HTrees1
S2 <- "Anthus_nyassae"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Anthus_lutescens"     # Species in breeding_point & HTrees1
S2 <- "Anthus_peruvianus"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Antigone_antigone", "Grus_antigone", breeding_point[,1])
breeding_point[,1] <- gsub("Antigone_canadensis", "Grus_canadensis", breeding_point[,1])
breeding_point[,1] <- gsub("Antigone_rubicunda", "Grus_rubicunda", breeding_point[,1])
breeding_point[,1] <- gsub("Antigone_vipio", "Grus_vipio", breeding_point[,1])
breeding_point[,1] <- gsub("Antrostomus", "Caprimulgus", breeding_point[,1])
breeding_point[,1] <- gsub("Anumara_forbesi", "Curaeus_forbesi", breeding_point[,1])

S1 <- "Apalis_flavida"
S2 <- "Apalis_flavocincta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Apalis_porphyrolaema"
S2 <- "Apalis_kaboboensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aphelocoma_ultramarina"
S2 <- "Aphelocoma_wollweberi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Aphrodroma_brevirostris", "Lugensa_brevirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Aprositornis_disjuncta", "Myrmeciza_disjuncta", breeding_point[,1])
breeding_point[,1] <- gsub("Aquila_africana", "Aquila_africanus", breeding_point[,1])
breeding_point[,1] <- gsub("Aquila_fasciata", "Aquila_fasciatus", breeding_point[,1])
breeding_point[,1] <- gsub("Aquila_spilogaster", "Hieraaetus_spilogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Arachnothera_hypogrammica", "Hypogramma_hypogrammicum", breeding_point[,1])

S1 <- "Arachnothera_affinis"
S2 <- "Arachnothera_modesta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Aramides_cajaneus", "Aramides_cajanea", breeding_point[,1])

S1 <- "Aratinga_solstitialis"
S2 <- "Aratinga_maculata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Aratinga_nenday", "Nandayus_nenday", breeding_point[,1])

S1 <- "Arborophila_charltonii"     # Species in breeding_point & HTrees1
S2 <- "Arborophila_graydoni"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arborophila_charltonii"     # Species in breeding_point & HTrees1
S2 <- "Arborophila_tonkinensis"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Arcanator_orostruthus", "Modulatrix_orostruthus", breeding_point[,1])

breeding_point[,1] <- gsub("Ardea_alba", "Casmerodius_albus", breeding_point[,1])
breeding_point[,1] <- gsub("Ardea_intermedia", "Mesophoyx_intermedia", breeding_point[,1])

S1 <- "Mesophoyx_intermedia"     # Species in breeding_point & HTrees1
S2 <- "Ardea_brachyrhyncha"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Mesophoyx_intermedia"     # Species in breeding_point & HTrees1
S2 <- "Ardea_plumifera"  # Species just in SPD, to roll into S1 and then delete
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ardenna_bulleri", "Puffinus_bulleri", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_carneipes", "Puffinus_carneipes", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_creatopus", "Puffinus_creatopus", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_gravis", "Puffinus_gravis", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_grisea", "Puffinus_griseus", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_pacifica", "Puffinus_pacificus", breeding_point[,1])
breeding_point[,1] <- gsub("Ardenna_tenuirostris", "Puffinus_tenuirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_caudata", "Turdoides_caudata", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_altirostris", "Turdoides_altirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_aylmeri", "Turdoides_aylmeri", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_earlei", "Turdoides_earlei", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_fulva", "Turdoides_fulva", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_malcolmi", "Turdoides_malcolmi", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_rubiginosa", "Turdoides_rubiginosa", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_squamiceps", "Turdoides_squamiceps", breeding_point[,1])
breeding_point[,1] <- gsub("Argya_subrufa", "Turdoides_subrufa", breeding_point[,1])
breeding_point[,1] <- gsub("Arizelocichla_nigriceps", "Andropadus_nigriceps", breeding_point[,1])

S1 <- "Andropadus_nigriceps"
S2 <- "Arizelocichla_fusciceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Andropadus_nigriceps"
S2 <- "Arizelocichla_chlorigula"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Arizelocichla_masukuensis", "Andropadus_masukuensis", breeding_point[,1])
breeding_point[,1] <- gsub("Arizelocichla_milanjensis", "Andropadus_milanjensis", breeding_point[,1])
breeding_point[,1] <- gsub("Arizelocichla_montana", "Andropadus_montanus", breeding_point[,1])

S1 <- "Andropadus_milanjensis"
S2 <- "Arizelocichla_striifacies"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Andropadus_nigriceps"
S2 <- "Arizelocichla_tephrolaema"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_assimilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Arremon_torquatus"
S2 <- "Arremon_atricapillus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Arremon_taciturnus"
S2 <- "Arremon_axillaris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_basilicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_costaricensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_abeillei"
S2 <- "Arremon_nigriceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_perijanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_phaeopleurus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Arremon_torquatus"
S2 <- "Arremon_phygas"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Arses_telescopthalmus", "Arses_telescophthalmus", breeding_point[,1])

S1 <- "Arses_telescophthalmus"
S2 <- "Arses_lorealis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Artamus_leucoryn", "Artamus_leucorynchus", breeding_point[,1])
breeding_point[,1] <- gsub("Artemisiospiza_belli", "Amphispiza_belli", breeding_point[,1])

S1 <- "Amphispiza_belli"
S2 <- "Artemisiospiza_nevadensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Arundinax_aedon", "Acrocephalus_aedon", breeding_point[,1])
breeding_point[,1] <- gsub("Asarcornis_scutulata", "Cairina_scutulata", breeding_point[,1])
breeding_point[,1] <- gsub("Asemospiza_fuliginosa", "Tiaris_fuliginosus", breeding_point[,1])
breeding_point[,1] <- gsub("Asemospiza_obscura", "Tiaris_obscurus", breeding_point[,1])
breeding_point[,1] <- gsub("Asio_clamator", "Pseudoscops_clamator", breeding_point[,1])

S1 <- "Asthenes_dorbignyi"
S2 <- "Asthenes_arequipae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Asthenes_vilcabambae", "Schizoeaca_vilcabambae", breeding_point[,1])

S1 <- "Schizoeaca_vilcabambae"
S2 <- "Asthenes_ayacuchensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Asthenes_berlepschi"), ]   #Missing from Derryberry tree?
breeding_point <- breeding_point[-which(breeding_point[,1]=="Asthenes_heterura"), ]   #Missing from Derryberry tree?



breeding_point[,1] <- gsub("Asthenes_coryi", "Schizoeaca_coryi", breeding_point[,1])
breeding_point[,1] <- gsub("Asthenes_fuliginosa", "Schizoeaca_fuliginosa", breeding_point[,1])
breeding_point[,1] <- gsub("Asthenes_griseomurina", "Schizoeaca_griseomurina", breeding_point[,1])
breeding_point[,1] <- gsub("Asthenes_harterti", "Schizoeaca_harterti", breeding_point[,1])
breeding_point[,1] <- gsub("Asthenes_helleri", "Schizoeaca_helleri", breeding_point[,1])

S1 <- "Asthenes_dorbignyi"
S2 <- "Asthenes_huancavelicae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Asthenes_moreirae", "Oreophylax_moreirae", breeding_point[,1])
breeding_point[,1] <- gsub("Asthenes_palpebralis", "Schizoeaca_palpebralis", breeding_point[,1])

S1 <- "Asthenes_dorbignyi"
S2 <- "Asthenes_usheri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Atimastillas_flavicollis", "Chlorocichla_flavicollis", breeding_point[,1])

S1 <- "Chlorocichla_flavicollis"
S2 <- "Atimastillas_flavigula"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Atlapetes_blancae"), ] # Newly describe; unclear what closest relative is

S1 <- "Atlapetes_tricolor"
S2 <- "Atlapetes_crassus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Atlapetes_albofrenatus"
S2 <- "Atlapetes_meridae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Atlapetes_latinuchus"
S2 <- "Atlapetes_nigrifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Atlapetes_leucopterus"
S2 <- "Atlapetes_paynteri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Atlapetes_schistaceus"
S2 <- "Atlapetes_taczanowskii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Atticora_pileata", "Notiochelidon_pileata", breeding_point[,1])
breeding_point[,1] <- gsub("Atticora_tibialis", "Neochelidon_tibialis", breeding_point[,1])

S1 <- "Aulacorhynchus_prasinus"
S2 <- "Aulacorhynchus_albivitta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_prasinus"
S2 <- "Aulacorhynchus_atrogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_prasinus"
S2 <- "Aulacorhynchus_caeruleogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_sulcatus"
S2 <- "Aulacorhynchus_calorhynchus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_prasinus"
S2 <- "Aulacorhynchus_cyanolaemus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_prasinus"
S2 <- "Aulacorhynchus_wagleri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Aulacorhynchus_derbianus"
S2 <- "Aulacorhynchus_whitelianus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Automolus_subulatus", "Hyloctistes_subulatus", breeding_point[,1])

S1 <- "Hyloctistes_subulatus"
S2 <- "Automolus_virgatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Bambusicola_thoracicus"
S2 <- "Bambusicola_sonorivox"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Basileuterus_culicivorus"
S2 <- "Basileuterus_auricapilla"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Basileuterus_culicivorus"
S2 <- "Basileuterus_cabanisi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Basileuterus_lachrymosus", "Euthlypis_lachrymosa", breeding_point[,1])
breeding_point[,1] <- gsub("Basilinna_leucotis", "Hylocharis_leucotis", breeding_point[,1])
breeding_point[,1] <- gsub("Basilinna_xantusii", "Hylocharis_xantusii", breeding_point[,1])

S1 <- "Batis_capensis"
S2 <- "Batis_dimorpha"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Batis_minor"
S2 <- "Batis_erlangeri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Batis_mixta"
S2 <- "Batis_reichenowi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Berenicornis_comatus", "Aceros_comatus", breeding_point[,1])

S1 <- "Bleda_notatus"
S2 <- "Bleda_ugandae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Bocagia_minuta", "Tchagra_minutus", breeding_point[,1])
breeding_point[,1] <- gsub("Bolemoreus_frenatus", "Lichenostomus_frenatus", breeding_point[,1])
breeding_point[,1] <- gsub("Bolemoreus_hindwoodi", "Lichenostomus_hindwoodi", breeding_point[,1])
breeding_point[,1] <- gsub("Brachypodius_atriceps", "Pycnonotus_atriceps", breeding_point[,1])
breeding_point[,1] <- gsub("Brachypodius_priocephalus", "Pycnonotus_priocephalus", breeding_point[,1])

S1 <- "Brachypteryx_montana"
S2 <- "Brachypteryx_cruralis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Brachypteryx_montana"
S2 <- "Brachypteryx_erythrogyna"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Brachypteryx_leucophris", "Brachypteryx_leucophrys", breeding_point[,1])

S1 <- "Brachypteryx_montana"
S2 <- "Brachypteryx_saturata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Brachypteryx_montana"
S2 <- "Brachypteryx_sinensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Bradornis_boehmi", "Muscicapa_boehmi", breeding_point[,1])
breeding_point[,1] <- gsub("Bradornis_comitatus", "Muscicapa_comitata", breeding_point[,1])
breeding_point[,1] <- gsub("Bradornis_fuliginosus", "Muscicapa_infuscata", breeding_point[,1])
breeding_point[,1] <- gsub("Bradornis_ussheri", "Muscicapa_ussheri", breeding_point[,1])

S1 <- "Bradypterus_baboecala"
S2 <- "Bradypterus_centralis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Bubo_blakistoni", "Ketupa_blakistoni", breeding_point[,1])
breeding_point[,1] <- gsub("Bubo_scandiacus", "Bubo_scandiaca", breeding_point[,1])
breeding_point[,1] <- gsub("Bucanetes_mongolicus", "Eremopsaltria_mongolicus", breeding_point[,1])
breeding_point[,1] <- gsub("Bucorvus_leadbeateri", "Bucorvus_cafer", breeding_point[,1])
breeding_point[,1] <- gsub("Bugeranus_carunculatus", "Grus_carunculatus", breeding_point[,1])
breeding_point[,1] <- gsub("Buphagus_erythrorynchus", "Buphagus_erythrorhynchus", breeding_point[,1])

S1 <- "Burhinus_oedicnemus"
S2 <- "Burhinus_indicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Buteo_buteo"
S2 <- "Buteo_japonicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Buteo_nitidus"
S2 <- "Buteo_plagiatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Buteo_buteo"
S2 <- "Buteo_refectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Buteo_oreophilus"
S2 <- "Buteo_trizonatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Buteogallus_coronatus", "Harpyhaliaetus_coronatus", breeding_point[,1])
breeding_point[,1] <- gsub("Buteogallus_lacernulatus", "Leucopternis_lacernulatus", breeding_point[,1])
breeding_point[,1] <- gsub("Buteogallus_schistaceus", "Leucopternis_schistaceus", breeding_point[,1])
breeding_point[,1] <- gsub("Buteogallus_solitarius", "Harpyhaliaetus_solitarius", breeding_point[,1])

S1 <- "Bycanistes_fistulator"
S2 <- "Bycanistes_sharpii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cacicus_latirostris", "Ocyalus_latirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Cacicus_oseryi", "Clypicterus_oseryi", breeding_point[,1])

S1 <- "Cacicus_microrhynchus"
S2 <- "Cacicus_pacificus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cacicus_cela"
S2 <- "Cacicus_vitellinus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Calamanthus_cautus", "Hylacola_cauta", breeding_point[,1])

S1 <- "Calamanthus_campestris"
S2 <- "Calamanthus_montanellus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Calamanthus_pyrrhopygius", "Hylacola_pyrrhopygia", breeding_point[,1])
breeding_point[,1] <- gsub("Calamonastes_fasciolatus", "Camaroptera_fasciolata", breeding_point[,1])
breeding_point[,1] <- gsub("Calamonastes_simplex", "Camaroptera_simplex", breeding_point[,1])

S1 <- "Camaroptera_simplex"
S2 <- "Calamonastes_stierlingi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Camaroptera_simplex"
S2 <- "Calamonastes_undosus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Calamonastides_gracilirostris", "Chloropeta_gracilirostris", breeding_point[,1])

S1 <- "Chloropeta_gracilirostris"
S2 <- "Calamonastides_bensoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Calandrella_brachydactyla"
S2 <- "Calandrella_blanfordi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Calandrella_brachydactyla"
S2 <- "Calandrella_dukhunensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Calandrella_brachydactyla"
S2 <- "Calandrella_eremica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Calendulauda_africanoides", "Mirafra_africanoides", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_albescens", "Certhilauda_albescens", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_alopex", "Mirafra_alopex", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_burra", "Certhilauda_burra", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_erythrochlamys", "Certhilauda_erythrochlamys", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_poecilosterna", "Mirafra_poecilosterna", breeding_point[,1])
breeding_point[,1] <- gsub("Calendulauda_sabota", "Mirafra_sabota", breeding_point[,1])
breeding_point[,1] <- gsub("Calherodius_leuconotus", "Gorsachius_leuconotus", breeding_point[,1])
breeding_point[,1] <- gsub("Calidris_falcinellus", "Limicola_falcinellus", breeding_point[,1])
breeding_point[,1] <- gsub("Calidris_pugnax", "Philomachus_pugnax", breeding_point[,1])
breeding_point[,1] <- gsub("Calidris_pygmaea", "Eurynorhynchus_pygmeus", breeding_point[,1])
breeding_point[,1] <- gsub("Calidris_subruficollis", "Tryngites_subruficollis", breeding_point[,1])
breeding_point[,1] <- gsub("Calidris_virgata", "Aphriza_virgata", breeding_point[,1])
breeding_point[,1] <- gsub("Caligavis_chrysops", "Lichenostomus_chrysops", breeding_point[,1])
breeding_point[,1] <- gsub("Caligavis_subfrenata", "Lichenostomus_subfrenatus", breeding_point[,1])
breeding_point[,1] <- gsub("Caligavis_obscura", "Lichenostomus_obscurus", breeding_point[,1])
breeding_point[,1] <- gsub("Calliope_calliope", "Stellula_calliope", breeding_point[,1])
breeding_point[,1] <- gsub("Calliope_obscura", "Luscinia_obscura", breeding_point[,1])
breeding_point[,1] <- gsub("Calliope_pectardens", "Luscinia_pectardens", breeding_point[,1])
breeding_point[,1] <- gsub("Calliope_pectoralis", "Luscinia_pectoralis", breeding_point[,1])

S1 <- "Luscinia_pectoralis"
S2 <- "Calliope_tschebaiewi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Calonectris_diomedea"
S2 <- "Calonectris_borealis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Caloramphus_", "Calorhamphus_", breeding_point[,1])

S1 <- "Calorhamphus_fuliginosus"
S2 <- "Calorhamphus_hayii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Calyptocichla_serinus", "Calyptocichla_serina", breeding_point[,1])

S1 <- "Camaroptera_brachyura"
S2 <- "Camaroptera_harterti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Camaroptera_chloronota"
S2 <- "Camaroptera_toroensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Campephilus_haematogaster"
S2 <- "Campephilus_splendens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Campethera_tullbergi"
S2 <- "Campethera_taeniolaema"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Campicoloides_bifasciatus", "Oenanthe_bifasciata", breeding_point[,1])
breeding_point <- breeding_point[-which(breeding_point[,1]=="Camptorhynchus_labradorius"), ]
breeding_point[,1] <- gsub("Campylopterus_cuvierii", "Phaeochroa_cuvierii", breeding_point[,1])

S1 <- "Campylorhamphus_procurvoides"
S2 <- "Campylorhamphus_multostriatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Campylorhamphus_procurvoides"
S2 <- "Campylorhamphus_probatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Campylorhynchus_rufinucha"
S2 <- "Campylorhynchus_capistratus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Campylorhynchus_rufinucha"
S2 <- "Campylorhynchus_humilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cantorchilus_", "Thryothorus_", breeding_point[,1])

S1 <- "Caprimulgus_vociferus"
S2 <- "Caprimulgus_arizonae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Caprimulgus_indicus"
S2 <- "Caprimulgus_jotaka"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Caprimulgus_longipennis", "Macrodipteryx_longipennis", breeding_point[,1])
breeding_point[,1] <- gsub("Caprimulgus_vexillarius", "Macrodipteryx_vexillarius", breeding_point[,1])
breeding_point[,1] <- gsub("Cardellina_canadensis", "Wilsonia_canadensis", breeding_point[,1])
breeding_point[,1] <- gsub("Cardellina_pusilla", "Wilsonia_pusilla", breeding_point[,1])
breeding_point[,1] <- gsub("Cardellina_rubra", "Ergaticus_ruber", breeding_point[,1])
breeding_point[,1] <- gsub("Cardellina_versicolor", "Ergaticus_versicolor", breeding_point[,1])

S1 <- "Carduelis_carduelis"
S2 <- "Carduelis_caniceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Carpococcyx_radiceus", "Carpococcyx_radiatus", breeding_point[,1])

S1 <- "Carpodacus_thura"
S2 <- "Carpodacus_dubius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Carpodacus_sibiricus", "Uragus_sibiricus", breeding_point[,1])
breeding_point[,1] <- gsub("Carpodacus_sillemi", "Leucosticte_sillemi", breeding_point[,1])
breeding_point[,1] <- gsub("Carpodacus_sipahi", "Haematospiza_sipahi", breeding_point[,1])

S1 <- "Carpodacus_synoicus"
S2 <- "Carpodacus_stoliczkae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Carpodacus_subhimachalus", "Pinicola_subhimachala", breeding_point[,1])

S1 <- "Carpodacus_rodopeplus"
S2 <- "Carpodacus_verreauxii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Carpodacus_pulcherrimus"
S2 <- "Carpodacus_waltoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Carpospiza_brachydactyla", "Petronia_brachydactyla", breeding_point[,1])
breeding_point[,1] <- gsub("Carterornis_chrysomela", "Monarcha_chrysomela", breeding_point[,1])
breeding_point[,1] <- gsub("Carterornis_leucotis", "Monarcha_leucotis", breeding_point[,1])
breeding_point[,1] <- gsub("Caryothraustes_celaeno", "Rhodothraupis_celaeno", breeding_point[,1])
breeding_point[,1] <- gsub("Caryothraustes_erythromelas", "Periporphyrus_erythromelas", breeding_point[,1])
breeding_point[,1] <- gsub("Cassiculus_melanicterus", "Cacicus_melanicterus", breeding_point[,1])
breeding_point[,1] <- gsub("Castanozoster_thoracicus", "Poospiza_thoracica", breeding_point[,1])

S1 <- "Catharus_ustulatus"
S2 <- "Catharus_swainsoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Catreus_wallichii", "Catreus_wallichi", breeding_point[,1])
breeding_point[,1] <- gsub("Ceblepyris_caesius", "Coracina_caesia", breeding_point[,1])
breeding_point[,1] <- gsub("Ceblepyris_graueri", "Coracina_graueri", breeding_point[,1])
breeding_point[,1] <- gsub("Ceblepyris_pectoralis", "Coracina_pectoralis", breeding_point[,1])
breeding_point[,1] <- gsub("Cecropis_", "Hirundo_", breeding_point[,1])

S1 <- "Celeus_torquatus"
S2 <- "Celeus_occidentalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Celeus_flavescens"
S2 <- "Celeus_ochraceus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Celeus_torquatus"
S2 <- "Celeus_tinnunculus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cerasophila_thompsoni", "Hypsipetes_thompsoni", breeding_point[,1])
breeding_point[,1] <- gsub("Ceratopipra_", "Pipra_", breeding_point[,1])
breeding_point[,1] <- gsub("Cercomacroides_", "Cercomacra_", breeding_point[,1])

S1 <- "Cercomacra_nigrescens"
S2 <- "Cercomacra_fuscicauda"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cercotrichas_galactotes", "Erythropygia_galactotes", breeding_point[,1])
breeding_point[,1] <- gsub("Cercotrichas_hartlaubi", "Erythropygia_hartlaubi", breeding_point[,1])
breeding_point[,1] <- gsub("Cercotrichas_leucophrys", "Erythropygia_leucophrys", breeding_point[,1])
breeding_point[,1] <- gsub("Cercotrichas_paena", "Erythropygia_paena", breeding_point[,1])
breeding_point[,1] <- gsub("Cettia_castaneocoronata", "Tesia_castaneocoronata", breeding_point[,1])

S1 <- "Ceuthmochares_aereus"
S2 <- "Ceuthmochares_australis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ceyx_azureus", "Alcedo_azurea", breeding_point[,1])
breeding_point[,1] <- gsub("Ceyx_pusillus", "Alcedo_pusilla", breeding_point[,1])
breeding_point[,1] <- gsub("Ceyx_solitarius", "Ceyx_lepidus", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_basalis", "Chrysococcyx_basalis", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_lucidus", "Chrysococcyx_lucidus", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_megarhynchus", "Rhamphomantis_megarhynchus", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_meyerii", "Chrysococcyx_meyerii", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysococcyx_meyerii", "Chrysococcyx_meyeri", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_minutillus", "Chrysococcyx_minutillus", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_osculans", "Chrysococcyx_osculans", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcites_ruficollis", "Chrysococcyx_ruficollis", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcomitra_", "Nectarinia_", breeding_point[,1])
breeding_point[,1] <- gsub("Chalcoparia_singalensis", "Anthreptes_singalensis", breeding_point[,1])

S1 <- "Chalcophaps_indica"
S2 <- "Chalcophaps_longirostris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chalcopsitta_scintillata", "Chalcopsitta_sintillata", breeding_point[,1])
breeding_point[,1] <- gsub("Chamaetylas_", "Alethe_", breeding_point[,1])

S1 <- "Charadrius_alexandrinus"
S2 <- "Charadrius_dealbatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Charadrius_alexandrinus"
S2 <- "Charadrius_nivosus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Charmosyna_papou"
S2 <- "Charmosyna_stellae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chatarrhaea_gularis", "Turdoides_gularis", breeding_point[,1])
breeding_point[,1] <- gsub("Chatarrhaea_longirostris", "Turdoides_longirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Chelidorhynx_hypoxanthus", "Chelidorhynx_hypoxantha", breeding_point[,1])
breeding_point[,1] <- gsub("Chenorhamphus_grayi", "Malurus_grayi", breeding_point[,1])
breeding_point[,1] <- gsub("Chionodacryon_speculiferum", "Diuca_speculifera", breeding_point[,1])

S1 <- "Chlamydotis_undulata"
S2 <- "Chlamydotis_macqueenii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chleuasicus_atrosuperciliaris", "Paradoxornis_atrosuperciliaris", breeding_point[,1])
breeding_point[,1] <- gsub("Chloebia_gouldiae", "Erythrura_gouldiae", breeding_point[,1])
breeding_point[,1] <- gsub("Chloris_", "Carduelis_", breeding_point[,1])

S1 <- "Chlorochrysa_calliparaea"
S2 <- "Chlorochrysa_fulgentissima"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chlorophoneus_", "Telophorus_", breeding_point[,1])
breeding_point[,1] <- gsub("Chloropipo_flavicapilla", "Xenopipo_flavicapilla", breeding_point[,1])
breeding_point[,1] <- gsub("Chloropipo_unicolor", "Xenopipo_unicolor", breeding_point[,1])

S1 <- "Chloropsis_hardwickii"
S2 <- "Chloropsis_lazulina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Chloropsis_cochinchinensis"
S2 <- "Chloropsis_moluccensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chlorospingus_flavopectus", "Chlorospingus_ophthalmicus", breeding_point[,1])
breeding_point[,1] <- gsub("Chlorostilbon_notatus", "Chlorestes_notata", breeding_point[,1])
breeding_point[,1] <- gsub("Cholornis_paradoxus", "Paradoxornis_paradoxus", breeding_point[,1])
breeding_point[,1] <- gsub("Cholornis_unicolor", "Paradoxornis_unicolor", breeding_point[,1])
breeding_point[,1] <- gsub("Chordeiles_nacunda", "Podager_nacunda", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysocolaptes_guttacristatus", "Chrysocolaptes_lucidus", breeding_point[,1])
# nominate lucidus is a phillippine endemic

S1 <- "Chrysocolaptes_lucidus"
S2 <- "Chrysocolaptes_strictus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chrysocolaptes_validus", "Reinwardtipicus_validus", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysocorypha_delatrii", "Tachyphonus_delatrii", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysocorythus_estherae", "Serinus_estherae", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysominla_strigula", "Minla_strigula", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysophlegma_flavinucha", "Picus_flavinucha", breeding_point[,1])

S1 <- "Chrysophlegma_mentale"
S2 <- "Chrysophlegma_humii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Chrysophlegma_mentale", "Picus_mentalis", breeding_point[,1])
breeding_point[,1] <- gsub("Chrysophlegma_miniaceum", "Picus_mineaceus", breeding_point[,1])
breeding_point[,1] <- gsub("Ciccaba_", "Strix_", breeding_point[,1])

S1 <- "Cichlocolaptes_leucophrus"
S2 <- "Cichlocolaptes_holti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cichlopsis_leucogenys"
S2 <- "Cichlopsis_chubbi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cichlopsis_leucogenys"
S2 <- "Cichlopsis_gularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cichlopsis_leucogenys"
S2 <- "Cichlopsis_peruviana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ciconia_episcopus"
S2 <- "Ciconia_microscelis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cincloramphus_timoriensis", "Megalurus_timoriensis", breeding_point[,1])

S1 <- "Megalurus_timoriensis"
S2 <- "Cincloramphus_macrurus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cinclosoma_cinnamomeum"
S2 <- "Cinclosoma_alisteri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cinclosoma_castaneothorax"
S2 <- "Cinclosoma_marginatum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cinclus_schulzii", "Cinclus_schulzi", breeding_point[,1])
breeding_point[,1] <- gsub("Cinnyris_", "Nectarinia_", breeding_point[,1])

S1 <- "Circus_cyaneus"
S2 <- "Circus_hudsonius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Circus_spilonotus"
S2 <- "Circus_spilothorax"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cissa_thalassina"
S2 <- "Cissa_jefferyi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cissomela_pectoralis", "Certhionyx_pectoralis", breeding_point[,1])

S1 <- "Cisticola_aberrans"
S2 <- "Cisticola_bailunduensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cisticola_brunnescens"
S2 <- "Cisticola_cinnamomeus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Cisticola_galactotes"
S2 <- "Cisticola_haematocephalus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Cisticola_galactotes"
S2 <- "Cisticola_luapula"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Cisticola_galactotes"
S2 <- "Cisticola_lugubris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Cisticola_galactotes"
S2 <- "Cisticola_marginatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cisticola_nana", "Cisticola_nanus", breeding_point[,1])

S1 <- "Cistothorus_platensis"
S2 <- "Cistothorus_stellaris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Clanga_clanga", "Aquila_clanga", breeding_point[,1])
breeding_point[,1] <- gsub("Clanga_hastata", "Aquila_hastata", breeding_point[,1])
breeding_point[,1] <- gsub("Clanga_pomarina", "Aquila_pomarina", breeding_point[,1])
breeding_point[,1] <- gsub("Claravis_geoffroyi", "Claravis_godefrida", breeding_point[,1])
breeding_point[,1] <- gsub("Clibanornis_", "Automolus_", breeding_point[,1])
breeding_point[,1] <- gsub("Automolus_erythrocephalus", "Hylocryptus_erythrocephalus", breeding_point[,1])
breeding_point[,1] <- gsub("Automolus_dendrocolaptoides", "Clibanornis_dendrocolaptoides", breeding_point[,1])
breeding_point[,1] <- gsub("Automolus_rectirostris", "Hylocryptus_rectirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Automolus_rubiginosus", "Automolus_rubiginosus_nigricauda", breeding_point[,1])
breeding_point[,1] <- gsub("Cnemathraupis_aureodorsalis", "Buthraupis_aureodorsalis", breeding_point[,1])
breeding_point[,1] <- gsub("Cnemathraupis_eximia", "Buthraupis_eximia", breeding_point[,1])

S1 <- "Cnemophilus_macgregorii"
S2 <- "Cnemophilus_sanguineus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cnemoscopus_rubrirostris"
S2 <- "Cnemoscopus_chrysogaster"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Coccopygia_melanotis", "Estrilda_melanotis", breeding_point[,1])

S1 <- "Estrilda_melanotis"
S2 <- "Coccopygia_bocagei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Coccopygia_quartinia", "Estrilda_quartinia", breeding_point[,1])

S1 <- "Coeligena_violifer"
S2 <- "Coeligena_albicaudata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Coeligena_torquata"
S2 <- "Coeligena_conradii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Coeligena_consita", "Coeligena_bonapartei", breeding_point[,1]) # bonapartei didn't make into the dataset

S1 <- "Coeligena_violifer"
S2 <- "Coeligena_dichroura"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Coeligena_torquata"
S2 <- "Coeligena_eisenmanni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Coeligena_bonapartei"
S2 <- "Coeligena_eos"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Coeligena_torquata"
S2 <- "Coeligena_inca"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Coeligena_violifer"
S2 <- "Coeligena_osculans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_rubiginosus"
S2 <- "Colaptes_aeruginosus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_rivolii"
S2 <- "Colaptes_atriceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_auratus"
S2 <- "Colaptes_cafer"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_campestris"
S2 <- "Colaptes_campestroides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_rupicola"
S2 <- "Colaptes_cinereicapillus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_melanochloros"
S2 <- "Colaptes_melanolaimus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Colaptes_auratus"
S2 <- "Colaptes_mexicanoides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Collocalia_fuciphagus", "Collocalia_fuciphaga", breeding_point[,1])
breeding_point[,1] <- gsub("Collocalia_hirundinaceus", "Collocalia_hirundinacea", breeding_point[,1])
breeding_point[,1] <- gsub("Collocalia_maximus", "Collocalia_maxima", breeding_point[,1])
breeding_point[,1] <- gsub("Conirostrum_binghami", "Oreomanes_fraseri", breeding_point[,1])

S1 <- "Conirostrum_cinereum"
S2 <- "Conirostrum_fraseri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Conopophaga_aurita"
S2 <- "Conopophaga_snethlageae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Conostoma_aemodium", "Conostoma_oemodium", breeding_point[,1])

S1 <- "Contopus_cinereus"
S2 <- "Contopus_bogotensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Conuropsis_carolinensis"), ]

S1 <- "Coracias_benghalensis"
S2 <- "Coracias_affinis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Coracias_naevius", "Coracias_naevia", breeding_point[,1])
breeding_point[,1] <- gsub("Corcorax_melanoramphos", "Corcorax_melanorhamphos", breeding_point[,1])
breeding_point[,1] <- gsub("Corvus_pectoralis", "Corvus_torquatus", breeding_point[,1])
breeding_point[,1] <- gsub("Corydospiza_alaudina", "Phrygilus_alaudinus", breeding_point[,1])
breeding_point[,1] <- gsub("Corydospiza_carbonaria", "Phrygilus_carbonarius", breeding_point[,1])

S1 <- "Corythaixoides_personatus"
S2 <- "Corythaixoides_leopoldi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Corythornis_cristatus", "Alcedo_cristata", breeding_point[,1])
breeding_point[,1] <- gsub("Corythornis_leucogaster", "Alcedo_leucogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Cossypha_albicapillus", "Cossypha_albicapilla", breeding_point[,1])

S1 <- "Cracticus_torquatus"
S2 <- "Cracticus_argenteus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cranioleuca_berlepschi", "Thripophaga_berlepschi", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Cranioleuca_henricae"), ]  # missing from derryberry

S1 <- "Cranioleuca_marcapatae"
S2 <- "Cranioleuca_weskei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Crax_fasciolata"
S2 <- "Crax_pinima"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Crex_egregia", "Crecopsis_egregia", breeding_point[,1])
breeding_point[,1] <- gsub("Criniferoides_leucogaster", "Corythaixoides_leucogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Crithagra_", "Serinus_", breeding_point[,1])
breeding_point[,1] <- gsub("Cryptillas_victorini", "Bradypterus_victorini", breeding_point[,1])
breeding_point[,1] <- gsub("Cryptoleucopteryx_plumbea", "Leucopternis_plumbeus", breeding_point[,1])
breeding_point[,1] <- gsub("Cryptolybia_olivacea", "Stactolaema_olivacea", breeding_point[,1])
breeding_point[,1] <- gsub("Cryptopipo_holochlora", "Xenopipo_holochlora", breeding_point[,1])

S1 <- "Xenopipo_holochlora"
S2 <- "Cryptopipo_litae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Crypturellus_cinnamomeus"
S2 <- "Crypturellus_occidentalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyanecula_svecica", "Luscinia_svecica", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanistes_caeruleus", "Parus_caeruleus", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanistes_cyanus", "Parus_cyanus", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanistes_teneriffae", "Parus_teneriffae", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanocorax_coeruleus", "Cyanocorax_caeruleus", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanocorax_colliei", "Calocitta_colliei", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanocorax_formosus", "Calocitta_formosa", breeding_point[,1])

S1 <- "Cyanocorax_heilprini"
S2 <- "Cyanocorax_hafferi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyanoderma_", "Stachyris_", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanograucalus_azureus", "Coracina_azurea", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanoloxia_brissonii", "Cyanocompsa_brissonii", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanoloxia_cyanoides", "Cyanocompsa_cyanoides", breeding_point[,1])
breeding_point[,1] <- gsub("Cyanoloxia_rothschildii", "Cyanocompsa_rothschildii", breeding_point[,1])

S1 <- "Cyanocompsa_cyanoides"
S2 <- "Cyanocompsa_rothschildii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyanolyca_nanus", "Cyanolyca_nana", breeding_point[,1])

S1 <- "Cyanolyca_armillata"
S2 <- "Cyanolyca_quindiuna"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyanomitra_", "Nectarinia_", breeding_point[,1])

S1 <- "Cyanopica_cyanus"
S2 <- "Cyanopica_cooki"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cyanoptila_cyanomelana"
S2 <- "Cyanoptila_cumatilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cyclopsitta_gulielmitertii"
S2 <- "Cyclopsitta_amabilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cyclopsitta_diophthalma"
S2 <- "Cyclopsitta_coxeni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cyclopsitta_gulielmitertii"
S2 <- "Cyclopsitta_melanogenia"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cyclopsitta_gulielmitertii"
S2 <- "Cyclopsitta_nigrifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cymbirhynchus_macrorhynchos"
S2 <- "Cymbirhynchus_affinis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cynanthus_latirostris"
S2 <- "Cynanthus_doubledayi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyornis_brunneatus", "Rhinomyias_brunneatus", breeding_point[,1])

S1 <- "Cyornis_rubeculoides"
S2 <- "Cyornis_glaucicomans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyornis_olivaceus", "Rhinomyias_olivaceus", breeding_point[,1])
breeding_point[,1] <- gsub("Cyornis_pallidipes", "Cyornis_pallipes", breeding_point[,1])
breeding_point[,1] <- gsub("Cyornis_ruficauda", "Rhinomyias_ruficauda", breeding_point[,1])

S1 <- "Cyornis_tickelliae"
S2 <- "Cyornis_sumatrensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyornis_umbratilis", "Rhinomyias_umbratilis", breeding_point[,1])

S1 <- "Cyphorhinus_thoracicus"
S2 <- "Cyphorhinus_dichrous"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Cyphos_", "Bucco_", breeding_point[,1])

S1 <- "Cyrtonyx_montezumae"
S2 <- "Cyrtonyx_sallei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Daphoenositta_chrysoptera"
S2 <- "Daphoenositta_papuensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Deconychura_longicauda"
S2 <- "Deconychura_pallida"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Deconychura_longicauda"
S2 <- "Deconychura_typica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Deleornis_axillaris", "Anthreptes_fraseri", breeding_point[,1])

S1 <- "Anthreptes_fraseri"
S2 <- "Deleornis_fraseri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Delichon_urbicum"
S2 <- "Delichon_lagopodum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Dendrocincla_fuliginosa"
S2 <- "Dendrocincla_turdina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Dendrocitta_occipitalis"
S2 <- "Dendrocitta_cinerascens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dendrocolaptes_certhia", "Dendrocolaptes_certhia_certhia", breeding_point[,1])

S1 <- "Dendrocolaptes_sanctithomae"
S2 <- "Dendrocolaptes_punctipectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Dendrocopos_macei"
S2 <- "Dendrocopos_analis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dendroperdix_sephaena", "Francolinus_sephaena", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_dorae", "Dendrocopos_dorae", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_elliotii", "Dendrocopos_elliotii", breeding_point[,1])
breeding_point[,1] <- gsub("Dendrocopos_elliotii", "Mesopicos_elliotii", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_goertae", "Mesopicos_goertae", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_griseocephalus", "Mesopicos_griseocephalus", breeding_point[,1])

S1 <- "Mesopicos_elliotii"
S2 <- "Dendropicos_johnstoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dendropicos_namaquus", "Thripias_namaquus", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_obsoletus", "Dendrocopos_obsoletus", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_pyrrhogaster", "Thripias_pyrrhogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Dendropicos_xantholophus", "Thripias_xantholophus", breeding_point[,1])
breeding_point[,1] <- gsub("Dessonornis_anomalus", "Cossypha_anomala", breeding_point[,1])
breeding_point[,1] <- gsub("Dessonornis_archeri", "Cossypha_archeri", breeding_point[,1])
breeding_point[,1] <- gsub("Dessonornis_caffer", "Cossypha_caffra", breeding_point[,1])
breeding_point[,1] <- gsub("Dessonornis_humeralis", "Cossypha_humeralis", breeding_point[,1])

S1 <- "Cossypha_anomala"
S2 <- "Dessonornis_macclounii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cossypha_anomala"
S2 <- "Dessonornis_mbuluensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Devioeca_papuana", "Microeca_papuana", breeding_point[,1])

S1 <- "Dicaeum_ignipectus"
S2 <- "Dicaeum_beccarii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Dicaeum_ignipectus"
S2 <- "Dicaeum_cambodianum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dicaeum_melanozanthum", "Dicaeum_melanoxanthum", breeding_point[,1])

S1 <- "Dicaeum_concolor"
S2 <- "Dicaeum_minullum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dicrurus_annectens", "Dicrurus_annectans", breeding_point[,1])

S1 <- "Diopsittaca_nobilis"
S2 <- "Diopsittaca_cumanensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dives_warczewiczi", "Dives_warszewiczi", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Nectarinia_reichenowi"), ]
breeding_point[,1] <- gsub("Drepanorhynchus_reichenowi", "Nectarinia_reichenowi", breeding_point[,1])
breeding_point[,1] <- gsub("Drepanornis_albertisi", "Epimachus_albertisi", breeding_point[,1])
breeding_point[,1] <- gsub("Drepanornis_bruijnii", "Epimachus_bruijnii", breeding_point[,1])

S1 <- "Drymodes_superciliaris"
S2 <- "Drymodes_beccarii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Drymophila_caudata"
S2 <- "Drymophila_hellmayri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Drymophila_caudata"
S2 <- "Drymophila_klagesi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Drymophila_caudata"
S2 <- "Drymophila_striaticeps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Drymotoxeres_pucherani", "Campylorhamphus_pucherani", breeding_point[,1])
breeding_point[,1] <- gsub("Campylorhamphus_pucheranii", "Drymotoxeres_pucherani", breeding_point[,1])
breeding_point[,1] <- gsub("Dryobates_cathpharius", "Dendrocopos_cathpharius", breeding_point[,1])
breeding_point[,1] <- gsub("Dryobates_minor", "Dendrocopos_minor", breeding_point[,1])
breeding_point[,1] <- gsub("Dryobates_nuttallii", "Picoides_nuttallii", breeding_point[,1])

S1 <- "Dendrocopos_cathpharius"
S2 <- "Dryobates_pernyii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dryobates_pubescens", "Picoides_pubescens", breeding_point[,1])
breeding_point[,1] <- gsub("Dryobates_scalaris", "Picoides_scalaris", breeding_point[,1])

S1 <- "Dubusia_taeniata"
S2 <- "Dubusia_carrikeri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dubusia_castaneoventris", "Delothraupis_castaneoventris", breeding_point[,1])

S1 <- "Dubusia_taeniata"
S2 <- "Dubusia_stictocephala"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ducula_badia"
S2 <- "Ducula_cuprea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dyaphorophyia_", "Platysteira_", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Ectopistes_migratorius"), ]

breeding_point[,1] <- gsub("Edolisoma_incertum", "Coracina_incerta", breeding_point[,1])
breeding_point[,1] <- gsub("Edolisoma_melas", "Coracina_melas", breeding_point[,1])
breeding_point[,1] <- gsub("Edolisoma_montanum", "Coracina_montana", breeding_point[,1])
breeding_point[,1] <- gsub("Edolisoma_schisticeps", "Coracina_schisticeps", breeding_point[,1])
breeding_point[,1] <- gsub("Edolisoma_tenuirostre", "Coracina_tenuirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Egretta_picata", "Ardea_picata", breeding_point[,1])
breeding_point[,1] <- gsub("Elachura_formosa", "Spelaeornis_formosus", breeding_point[,1])

S1 <- "Elaenia_chiriquensis"
S2 <- "Elaenia_brachyptera"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Elaenia_pallatangae"
S2 <- "Elaenia_olivina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Elaenia_obscura"
S2 <- "Elaenia_sordida"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Emarginata_schlegelii", "Cercomela_schlegelii", breeding_point[,1])
breeding_point[,1] <- gsub("Emarginata_sinuata", "Cercomela_sinuata", breeding_point[,1])
breeding_point[,1] <- gsub("Emarginata_tractrac", "Cercomela_tractrac", breeding_point[,1])
breeding_point[,1] <- gsub("Emberiza_calandra", "Miliaria_calandra", breeding_point[,1])

S1 <- "Emberiza_tahapisi"
S2 <- "Emberiza_goslingi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Emberiza_lathami", "Melophus_lathami", breeding_point[,1])

S1 <- "Emberiza_spodocephala"
S2 <- "Emberiza_personata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Emberiza_striolata"
S2 <- "Emberiza_sahari"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Emberiza_siemsseni", "Latoucheornis_siemsseni", breeding_point[,1])

S1 <- "Entomyzon_cyanotis"
S2 <- "Entomyzon_albipennis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Eolophus_roseicapilla", "Cacatua_roseicapilla", breeding_point[,1])
breeding_point[,1] <- gsub("Ephippiospingus_dorsalis", "Phrygilus_dorsalis", breeding_point[,1])
breeding_point[,1] <- gsub("Ephippiospingus_erythronotus", "Phrygilus_erythronotus", breeding_point[,1])
breeding_point[,1] <- gsub("Epimachus_fastosus", "Epimachus_fastuosus", breeding_point[,1])

S1 <- "Epinecrophylla_haematonota"
S2 <- "Epinecrophylla_amazonica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Epinecrophylla_gutturalis", "Myrmotherula_gutturalis", breeding_point[,1])

S1 <- "Epinecrophylla_ornata"
S2 <- "Epinecrophylla_hoffmannsi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Eremalauda_dunni"
S2 <- "Eremalauda_eremodites"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Eriocnemis_aline", "Eriocnemis_alinae", breeding_point[,1])

S1 <- "Eriocnemis_luciani"
S2 <- "Eriocnemis_sapphiropygia"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Erythrogenys_", "Pomatorhinus_", breeding_point[,1])
breeding_point[,1] <- gsub("Erythropitta_", "Pitta_", breeding_point[,1])
breeding_point[,1] <- gsub("Esacus_magnirostris", "Esacus_giganteus", breeding_point[,1])
breeding_point[,1] <- gsub("Estrilda_coerulescens", "Estrilda_caerulescens", breeding_point[,1])

S1 <- "Eubucco_richardsoni"
S2 <- "Eubucco_aurantiicollis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Eubucco_versicolor"
S2 <- "Eubucco_glaucogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Eubucco_versicolor"
S2 <- "Eubucco_steerii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Euchrepomis_", "Terenura_", breeding_point[,1])

S1 <- "Eumyias_indigo"
S2 <- "Eumyias_ruficrissa"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Euodice_cantans", "Lonchura_cantans", breeding_point[,1])
breeding_point[,1] <- gsub("Euodice_malabarica", "Lonchura_malabarica", breeding_point[,1])

S1 <- "Euplectes_ardens"
S2 <- "Euplectes_laticauda"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Euplectes_psammacromius", "Euplectes_psammocromius", breeding_point[,1])
breeding_point[,1] <- gsub("Eupsittula_", "Aratinga_", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_astec", "Aratinga_nana", breeding_point[,1])  # nana is only on Jamaica
breeding_point[,1] <- gsub("Euptilotus_eutilotus", "Pycnonotus_eutilotus", breeding_point[,1])
breeding_point[,1] <- gsub("Eurillas_", "Andropadus_", breeding_point[,1])
breeding_point[,1] <- gsub("Eurocephalus_ruppelli", "Eurocephalus_rueppelli", breeding_point[,1])

S1 <- "Eurylaimus_javanicus"
S2 <- "Eurylaimus_harterti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Euscarthmus_meloryphus"
S2 <- "Euscarthmus_fulviceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Falcipennis", "Dendragapus", breeding_point[,1])

S1 <- "Dendragapus_canadensis"
S2 <- "Dendragapus_franklinii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Falco_chicquera"
S2 <- "Falco_ruficollis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Falcunculus_frontatus"
S2 <- "Falcunculus_leucogaster"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Falcunculus_frontatus"
S2 <- "Falcunculus_whitei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ficedula_narcissina"
S2 <- "Ficedula_elisae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ficedula_hodgsoni", "Muscicapella_hodgsoni", breeding_point[,1])
breeding_point[,1] <- gsub("Ficedula_erithacus", "Ficedula_hodgsonii", breeding_point[,1])
breeding_point[,1] <- gsub("Ficedula_ruficauda", "Muscicapa_ruficauda", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Formicivora_grantsaui"), ] #Newly discovered; not in Derryberry tree
breeding_point <- breeding_point[-which(breeding_point[,1]=="Formicivora_paludicola"), ] #Newly discovered; not in Derryberry tree

S1 <- "Forpus_xanthopterygius"
S2 <- "Forpus_spengeli"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Fraseria_caerulescens", "Muscicapa_caerulescens", breeding_point[,1])
breeding_point[,1] <- gsub("Fraseria_griseigularis", "Myioparus_griseigularis", breeding_point[,1])
breeding_point[,1] <- gsub("Fraseria_lendu", "Muscicapa_lendu", breeding_point[,1])
breeding_point[,1] <- gsub("Fraseria_olivascens", "Muscicapa_olivascens", breeding_point[,1])
breeding_point[,1] <- gsub("Fraseria_plumbea", "Myioparus_plumbeus", breeding_point[,1])
breeding_point[,1] <- gsub("Fraseria_tessmanni", "Muscicapa_tessmanni", breeding_point[,1])
breeding_point[,1] <- gsub("Frederickena_unduliger", "Frederickena_unduligera", breeding_point[,1])
breeding_point[,1] <- gsub("Fulvetta_", "Alcippe_", breeding_point[,1])

S1 <- "Furnarius_leucopus"
S2 <- "Furnarius_cinnamomeus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Furnarius_leucopus"
S2 <- "Furnarius_longirostris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Galbula_albirostris"
S2 <- "Galbula_chalcocephala"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gallinago_gallinago"
S2 <- "Gallinago_delicata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gallinula_chloropus"
S2 <- "Gallinula_galeata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gallirex_johnstoni", "Ruwenzorornis_johnstoni", breeding_point[,1])
breeding_point[,1] <- gsub("Gallirex_porphyreolophus", "Tauraco_porphyreolophus", breeding_point[,1])
breeding_point[,1] <- gsub("Garritornis_isidorei", "Pomatostomus_isidorei", breeding_point[,1])

S1 <- "Garrulax_cineraceus"
S2 <- "Garrulax_cinereiceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Garrulax_koslowi", "Babax_koslowi", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_lanceolatus", "Babax_lanceolatus", breeding_point[,1])

S1 <- "Garrulax_mitratus"
S2 <- "Garrulax_treacheri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Garrulax_waddelli", "Babax_waddelli", breeding_point[,1])

S1 <- "Babax_lanceolatus"
S2 <- "Garrulax_woodi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Garrulus_glandarius"
S2 <- "Garrulus_bispecularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Garrulus_glandarius"
S2 <- "Garrulus_leucotis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gavicalis_fasciogularis", "Lichenostomus_fasciogularis", breeding_point[,1])
breeding_point[,1] <- gsub("Gavicalis_versicolor", "Lichenostomus_versicolor", breeding_point[,1])
breeding_point[,1] <- gsub("Gavicalis_virescens", "Lichenostomus_virescens", breeding_point[,1])
breeding_point[,1] <- gsub("Gelochelidon_nilotica", "Sterna_nilotica", breeding_point[,1])

S1 <- "Sterna_nilotica"
S2 <- "Gelochelidon_macrotarsa"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gennaeodryas_placens", "Poecilodryas_placens", breeding_point[,1])
breeding_point[,1] <- gsub("Geokichla_", "Zoothera_", breeding_point[,1])

S1 <- "Geophaps_plumifera"
S2 <- "Geophaps_ferruginea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Geospizopsis_plebejus", "Phrygilus_plebejus", breeding_point[,1])
breeding_point[,1] <- gsub("Geospizopsis_unicolor", "Phrygilus_unicolor", breeding_point[,1])

S1 <- "Geothlypis_semiflava"
S2 <- "Geothlypis_bairdi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Geothlypis_formosa", "Oporornis_formosus", breeding_point[,1])
breeding_point[,1] <- gsub("Geothlypis_philadelphia", "Oporornis_philadelphia", breeding_point[,1])
breeding_point[,1] <- gsub("Geothlypis_tolmiei", "Oporornis_tolmiei", breeding_point[,1])

S1 <- "Geotrygon_saphirina"
S2 <- "Geotrygon_purpurata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Geranoaetus_albicaudatus", "Buteo_albicaudatus", breeding_point[,1])
breeding_point[,1] <- gsub("Geranoaetus_polyosoma", "Buteo_polyosoma", breeding_point[,1])

S1 <- "Glaucidium_gnoma"
S2 <- "Glaucidium_cobanense"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Glaucidium_gnoma"
S2 <- "Glaucidium_hoskinsii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Glaucidium_nana", "Glaucidium_nanum", breeding_point[,1])

S1 <- "Glaucidium_brasilianum"
S2 <- "Glaucidium_tucumanum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gliciphila_melanops", "Phylidonyris_melanops", breeding_point[,1])

S1 <- "Goura_scheepmakeri"
S2 <- "Goura_sclaterii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gracula_religiosa"
S2 <- "Gracula_indica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gracupica_contra", "Sturnus_contra", breeding_point[,1])

S1 <- "Sturnus_contra"
S2 <- "Gracupica_jalla"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gracupica_nigricollis", "Sturnus_nigricollis", breeding_point[,1])

S1 <- "Grallaria_quitensis"
S2 <- "Grallaria_alticola"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Grallaria_quitensis"
S2 <- "Grallaria_atuensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Grallaria_rufula"
S2 <- "Grallaria_saltuensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Grallaricula_nana"
S2 <- "Grallaricula_cumanensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Grallaricula_ferrugineipectus"
S2 <- "Grallaricula_leymebambae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Grallina_bruijnii", "Grallina_bruijni", breeding_point[,1])

S1 <- "Graminicola_bengalensis"
S2 <- "Graminicola_striatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Grammatoptila_striata", "Garrulax_striatus", breeding_point[,1])

S1 <- "Granatellus_pelzelni"
S2 <- "Granatellus_paraensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Granatina_granatina", "Uraeginthus_granatinus", breeding_point[,1])
breeding_point[,1] <- gsub("Granatina_ianthinogaster", "Uraeginthus_ianthinogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Griseotyrannus_aurantioatrocristatus", "Empidonomus_aurantioatrocristatus", breeding_point[,1])

S1 <- "Guttera_pucherani"
S2 <- "Guttera_edouardi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Guttera_pucherani"
S2 <- "Guttera_verreauxi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gymnobucco_bonapartei"
S2 <- "Gymnobucco_cinereiceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gymnobucco_calvus"
S2 <- "Gymnobucco_vernayi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Gymnopithys_leucaspis"
S2 <- "Gymnopithys_bicolor"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Gymnoris_", "Petronia_", breeding_point[,1])
breeding_point[,1] <- gsub("Gyps_rueppelli", "Gyps_rueppellii", breeding_point[,1])
breeding_point[,1] <- gsub("Habia_carmioli", "Chlorothraupis_carmioli", breeding_point[,1])
breeding_point[,1] <- gsub("Habia_frenata", "Chlorothraupis_frenata", breeding_point[,1])
breeding_point[,1] <- gsub("Habia_olivacea", "Chlorothraupis_olivacea", breeding_point[,1])
breeding_point[,1] <- gsub("Habia_stolzmanni", "Chlorothraupis_stolzmanni", breeding_point[,1])
breeding_point[,1] <- gsub("Haemorhous_", "Carpodacus_", breeding_point[,1])
breeding_point[,1] <- gsub("Hafferia_", "Myrmeciza_", breeding_point[,1])
breeding_point[,1] <- gsub("Hapalocrex_flaviventer", "Porzana_flaviventer", breeding_point[,1])
breeding_point[,1] <- gsub("Hedydipna_collaris", "Anthreptes_collaris", breeding_point[,1])
breeding_point[,1] <- gsub("Hedydipna_metallica", "Anthreptes_metallicus", breeding_point[,1])
breeding_point[,1] <- gsub("Hedydipna_platura", "Anthreptes_platurus", breeding_point[,1])
breeding_point[,1] <- gsub("Heleia_javanica", "Lophozosterops_javanicus", breeding_point[,1])
breeding_point[,1] <- gsub("Heleia_squamifrons", "Oculocincta_squamifrons", breeding_point[,1])

S1 <- "Heliangelus_amethysticollis"
S2 <- "Heliangelus_clarisse"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Heliangelus_amethysticollis"
S2 <- "Heliangelus_spencei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Heliangelus_zusii"), ]

S1 <- "Heliodoxa_schreibersii"
S2 <- "Heliodoxa_whitelyana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Hemicircus_concretus"
S2 <- "Hemicircus_sordidus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Hemimacronyx_chloris", "Anthus_chloris", breeding_point[,1])
breeding_point[,1] <- gsub("Hemitesia_pallidipes", "Cettia_pallidipes", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Hemitriccus_cohnhafti"), ]

S1 <- "Hemixos_flavala"
S2 <- "Hemixos_cinereus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Hemixos_flavala"
S2 <- "Hemixos_connectens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Henicorhina_leucophrys"
S2 <- "Henicorhina_anachoreta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Herpsilochmus_praedictus"), ]

S1 <- "Herpsilochmus_rufimarginatus"
S2 <- "Herpsilochmus_scapularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Herpsilochmus_stotzi"), ]

breeding_point[,1] <- gsub("Hesperiphona_abeillei", "Coccothraustes_abeillei", breeding_point[,1])
breeding_point[,1] <- gsub("Hesperiphona_vespertina", "Coccothraustes_vespertinus", breeding_point[,1])

S1 <- "Heteromyias_albispecularis"
S2 <- "Heteromyias_armiti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Heteromyias_albispecularis"
S2 <- "Heteromyias_cinereifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Heteroscenes_pallidus", "Cuculus_pallidus", breeding_point[,1])
breeding_point[,1] <- gsub("Heterotetrax_humilis", "Eupodotis_humilis", breeding_point[,1])
breeding_point[,1] <- gsub("Heterotetrax_rueppelii", "Eupodotis_rueppellii", breeding_point[,1])
breeding_point[,1] <- gsub("Heterotetrax_vigorsii", "Eupodotis_vigorsii", breeding_point[,1])
breeding_point[,1] <- gsub("Heteroxenicus_stellatus", "Brachypteryx_stellata", breeding_point[,1])
breeding_point[,1] <- gsub("Hieraaetus_wahlbergi", "Aquila_wahlbergi", breeding_point[,1])
breeding_point[,1] <- gsub("Hierococcyx_sparverioides", "Cuculus_sparverioides", breeding_point[,1])

S1 <- "Cuculus_sparverioides"
S2 <- "Hierococcyx_bocki"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Hierococcyx_fugax", "Cuculus_fugax", breeding_point[,1])

S1 <- "Cuculus_fugax"
S2 <- "Hierococcyx_hyperythrus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Cuculus_fugax"
S2 <- "Hierococcyx_nisicolor"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Hierococcyx_vagans", "Cuculus_vagans", breeding_point[,1])
breeding_point[,1] <- gsub("Hierococcyx_varius", "Cuculus_varius", breeding_point[,1])

S1 <- "Hirundinea_ferruginea"
S2 <- "Hirundinea_bellicosa"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Hirundo_javanica", "Hirundo_tahitica", breeding_point[,1])
breeding_point[,1] <- gsub("Histurgops_ruficauda", "Histurgops_ruficaudus", breeding_point[,1])
breeding_point[,1] <- gsub("Hoploxypterus_cayanus", "Vanellus_cayanus", breeding_point[,1])
breeding_point[,1] <- gsub("Horizocerus_albocristatus", "Tropicranus_albocristatus", breeding_point[,1])

S1 <- "Tropicranus_albocristatus"
S2 <- "Horizocerus_cassini"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Horizocerus_hartlaubi", "Tockus_hartlaubi", breeding_point[,1])

S1 <- "Tockus_hartlaubi"
S2 <- "Horizocerus_granti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Horornis_", "Cettia_", breeding_point[,1])
breeding_point[,1] <- gsub("Cettia_flavolivaceus", "Cettia_flavolivacea", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrobates_furcatus", "Oceanodroma_furcata", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrobates_leucorhous", "Oceanodroma_leucorhoa", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrobates_markhami", "Oceanodroma_markhami", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrobates_monorhis", "Oceanodroma_monorhis", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrocoloeus_minutus", "Larus_minutus", breeding_point[,1])
breeding_point[,1] <- gsub("Hydroprogne_caspia", "Sterna_caspia", breeding_point[,1])
breeding_point[,1] <- gsub("Hydropsalis_cayennensis", "Caprimulgus_cayennensis", breeding_point[,1])
breeding_point[,1] <- gsub("Hydropsalis_maculicaudus", "Caprimulgus_maculicaudus", breeding_point[,1])
breeding_point[,1] <- gsub("Hydrornis_", "Pitta_", breeding_point[,1])
breeding_point[,1] <- gsub("Hylatomus_", "Dryocopus_", breeding_point[,1])

S1 <- "Dryocopus_lineatus"
S2 <- "Dryocopus_fuscipennis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dryocopus_schulzii", "Dryocopus_schulzi", breeding_point[,1])

S1 <- "Hylophilus_thoracicus"
S2 <- "Hylophilus_griseiventris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Hylophilus_flavipes"
S2 <- "Hylophilus_viridiflavus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Hylopsar_cupreocauda", "Lamprotornis_cupreocauda", breeding_point[,1])
breeding_point[,1] <- gsub("Hylopsar_purpureiceps", "Lamprotornis_purpureiceps", breeding_point[,1])

S1 <- "Hypnelus_ruficollis"
S2 <- "Hypnelus_bicinctus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Hypocnemis_rondoni"), ]
breeding_point[,1] <- gsub("Hypotaenidia_philippensis", "Gallirallus_philippensis", breeding_point[,1])
breeding_point[,1] <- gsub("Hypotaenidia_torquata", "Gallirallus_torquatus", breeding_point[,1])
breeding_point[,1] <- gsub("Hypsipetes_amaurotis", "Ixos_amaurotis", breeding_point[,1])

S1 <- "Hypsipetes_leucocephalus"
S2 <- "Hypsipetes_ganeesa"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Icterus_cayanensis"
S2 <- "Icterus_chrysocephalus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Icterus_spurius"
S2 <- "Icterus_fuertesi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Icterus_cayanensis"
S2 <- "Icterus_pyrrhopterus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Icthyophaga_humilis", "Ichthyophaga_humilis", breeding_point[,1])
breeding_point[,1] <- gsub("Icthyophaga_ichthyaetus", "Ichthyophaga_ichthyaetus", breeding_point[,1])
breeding_point[,1] <- gsub("Ictinaetus_malaiensis", "Ictinaetus_malayensis", breeding_point[,1])
breeding_point[,1] <- gsub("Iduna_", "Hippolais_", breeding_point[,1])
breeding_point[,1] <- gsub("Hippolais_similis", "Chloropeta_similis", breeding_point[,1])
breeding_point[,1] <- gsub("Hippolais_natalensis", "Chloropeta_natalensis", breeding_point[,1])

S1 <- "Illadopsis_rufipennis"
S2 <- "Illadopsis_distans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Illadopsis_turdina", "Ptyrticus_turdinus", breeding_point[,1])
breeding_point[,1] <- gsub("Iole_charlottae", "Iole_olivacea", breeding_point[,1])
breeding_point[,1] <- gsub("Iole_viridescens", "Iole_virescens", breeding_point[,1])
breeding_point[,1] <- gsub("Isleria_guttata", "Myrmotherula_guttata", breeding_point[,1])
breeding_point[,1] <- gsub("Isleria_hauxwelli", "Myrmotherula_hauxwelli", breeding_point[,1])
breeding_point[,1] <- gsub("Islerothraupis_", "Tachyphonus_", breeding_point[,1])
breeding_point[,1] <- gsub("Ispidina_lecontei", "Ceyx_lecontei", breeding_point[,1])
breeding_point[,1] <- gsub("Ispidina_picta", "Ceyx_pictus", breeding_point[,1])

S1 <- "Ixobrychus_minutus"
S2 <- "Ixobrychus_dubius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ixoreus_naevius", "Zoothera_naevia", breeding_point[,1])
breeding_point[,1] <- gsub("Ixos_mcclellandii", "Hypsipetes_mcclellandii", breeding_point[,1])
breeding_point[,1] <- gsub("Ixos_virescens", "Hypsipetes_virescens", breeding_point[,1])

S1 <- "Hypsipetes_virescens"
S2 <- "Ixos_sumatranus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Juliamyia_", "Damophila_", breeding_point[,1])

S1 <- "Junco_phaeonotus"
S2 <- "Junco_bairdi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Kempiella_flavovirescens", "Microeca_flavovirescens", breeding_point[,1])
breeding_point[,1] <- gsub("Kempiella_griseoceps", "Microeca_griseoceps", breeding_point[,1])
breeding_point[,1] <- gsub("Kittacincla_malabarica", "Copsychus_malabaricus", breeding_point[,1])
breeding_point[,1] <- gsub("Kleinothraupis_atropileus", "Hemispingus_atropileus", breeding_point[,1])
breeding_point[,1] <- gsub("Kleinothraupis_auricularis", "Hemispingus_auricularis", breeding_point[,1])
breeding_point[,1] <- gsub("Kleinothraupis_", "Hemispingus_", breeding_point[,1])

S1 <- "Knipolegus_signatus"
S2 <- "Knipolegus_cabanisi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Knipolegus_aterrimus"
S2 <- "Knipolegus_heterogyna"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Knipolegus_orenocensis"
S2 <- "Knipolegus_sclateri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lacedo_pulchella"
S2 <- "Lacedo_melanops"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lagonosticta_larvata"
S2 <- "Lagonosticta_nigricollis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lagonosticta_larvata"
S2 <- "Lagonosticta_vinacea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lalage_fimbriata", "Coracina_fimbriata", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lalage_melanoptera", "Coracina_melanoptera", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lalage_melaschistos", "Coracina_melaschistos", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lalage_polioptera", "Coracina_polioptera", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lampornis_cinereicauda", "Lampornis_castaneoventris", breeding_point[,1]) # castaneoventris doesn't show up in our points
breeding_point[,1] <- gsub("Lamprotornis_albicapillus", "Spreo_albicapillus", breeding_point[,1]) 

S1 <- "Lamprotornis_mevesii"
S2 <- "Lamprotornis_benguelensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lamprotornis_bicolor", "Spreo_bicolor", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lamprotornis_fischeri", "Spreo_fischeri", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lamprotornis_iris", "Coccycolius_iris", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lamprotornis_regius", "Cosmopsarus_regius", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lamprotornis_unicolor", "Cosmopsarus_unicolor", breeding_point[,1]) 

S1 <- "Lamprotornis_mevesii"
S2 <- "Lamprotornis_violacior"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Laniarius_poensis"
S2 <- "Laniarius_holomelas"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Laniarius_aethiopicus"
S2 <- "Laniarius_nigerrimus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Laniarius_aethiopicus"
S2 <- "Laniarius_sublacteus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Laniellus_albonotatus", "Crocias_albonotatus", breeding_point[,1])

S1 <- "Lanius_excubitor"
S2 <- "Lanius_borealis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lanius_sphenocercus"
S2 <- "Lanius_giganteus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lanius_isabellinus"
S2 <- "Lanius_phoenicuroides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Larus_argentatus"
S2 <- "Larus_smithsonianus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Larus_scoresbii", "Leucophaeus_scoresbii", breeding_point[,1])
breeding_point[,1] <- gsub("Larvivora_akahige", "Erithacus_akahige", breeding_point[,1])
breeding_point[,1] <- gsub("Larvivora_brunnea", "Luscinia_brunnea", breeding_point[,1])
breeding_point[,1] <- gsub("Larvivora_cyane", "Luscinia_cyane", breeding_point[,1])
breeding_point[,1] <- gsub("Larvivora_ruficeps", "Luscinia_ruficeps", breeding_point[,1])
breeding_point[,1] <- gsub("Larvivora_sibilans", "Luscinia_sibilans", breeding_point[,1])
breeding_point[,1] <- gsub("Laticilla_burnesii", "Prinia_burnesii", breeding_point[,1])

S1 <- "Prinia_burnesii"
S2 <- "Laticilla_cinerascens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Leiopicus_auriceps", "Dendrocopos_auriceps", breeding_point[,1])
breeding_point[,1] <- gsub("Leiopicus_mahrattensis", "Dendrocopos_mahrattensis", breeding_point[,1])
breeding_point[,1] <- gsub("Leiopicus_medius", "Dendrocopos_medius", breeding_point[,1])
breeding_point[,1] <- gsub("Leioptila_annectens", "Heterophasia_annectens", breeding_point[,1])
breeding_point[,1] <- gsub("Leiothlypis_", "Vermivora_", breeding_point[,1])

S1 <- "Leiothrix_argentauris"
S2 <- "Leiothrix_laurinae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Leistes_bellicosus", "Sturnella_bellicosa", breeding_point[,1])
breeding_point[,1] <- gsub("Leistes_defilippii", "Sturnella_defilippii", breeding_point[,1])
breeding_point[,1] <- gsub("Leistes_loyca", "Sturnella_loyca", breeding_point[,1])
breeding_point[,1] <- gsub("Leistes_militaris", "Sturnella_militaris", breeding_point[,1])
breeding_point[,1] <- gsub("Leistes_superciliaris", "Sturnella_superciliaris", breeding_point[,1])

S1 <- "Lepidocolaptes_albolineatus"
S2 <- "Lepidocolaptes_duidae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lepidocolaptes_albolineatus"
S2 <- "Lepidocolaptes_fatimalimae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lepidocolaptes_albolineatus"
S2 <- "Lepidocolaptes_fuscicapillus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lepidocolaptes_affinis"
S2 <- "Lepidocolaptes_neglectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lepidocolaptes_falcinellus"
S2 <- "Lepidocolaptes_squamatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Leptasthenura_aegithaloides"
S2 <- "Leptasthenura_berlepschi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Leptasthenura_aegithaloides"
S2 <- "Leptasthenura_pallida"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Leptocoma_", "Nectarinia_", breeding_point[,1])

S1 <- "Leptopogon_superciliaris"
S2 <- "Leptopogon_albidiventer"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Leptoptilos_crumenifer", "Leptoptilos_crumeniferus", breeding_point[,1])
breeding_point[,1] <- gsub("Leptotila_cassinii", "Leptotila_cassini", breeding_point[,1])
breeding_point[,1] <- gsub("Leptotrygon_", "Geotrygon_", breeding_point[,1])
breeding_point[,1] <- gsub("Leucogeranus_leucogeranus", "Grus_leucogeranus", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_albolarvatus", "Picoides_albolarvatus", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_arizonae", "Picoides_arizonae", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_borealis", "Picoides_borealis", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_fumigatus", "Picoides_fumigatus", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_stricklandi", "Picoides_stricklandi", breeding_point[,1])
breeding_point[,1] <- gsub("Leuconotopicus_villosus", "Picoides_villosus", breeding_point[,1])
breeding_point[,1] <- gsub("Lewinia_striata", "Gallirallus_striatus", breeding_point[,1])
breeding_point[,1] <- gsub("Linaria_cannabina", "Carduelis_cannabina", breeding_point[,1])
breeding_point[,1] <- gsub("Linaria_flavirostris", "Carduelis_flavirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Linaria_johannis", "Carduelis_johannis", breeding_point[,1])
breeding_point[,1] <- gsub("Linaria_yemenensis", "Carduelis_yemenensis", breeding_point[,1])

S1 <- "Liocichla_phoenicea"
S2 <- "Liocichla_ripponi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lioparus_chrysotis", "Alcippe_chrysotis", breeding_point[,1])
breeding_point[,1] <- gsub("Lipaugus_ater", "Tijuca_atra", breeding_point[,1])
breeding_point[,1] <- gsub("Lissotis_hartlaubii", "Eupodotis_hartlaubii", breeding_point[,1])
breeding_point[,1] <- gsub("Lissotis_melanogaster", "Eupodotis_melanogaster", breeding_point[,1])
breeding_point[,1] <- gsub("Lobotos_lobatus", "Campephaga_lobata", breeding_point[,1])
breeding_point[,1] <- gsub("Lobotos_oriolinus", "Campephaga_oriolina", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Locustella_chengi"), ]

breeding_point[,1] <- gsub("Locustella_davidi", "Bradypterus_davidi", breeding_point[,1])
breeding_point[,1] <- gsub("Locustella_thoracica", "Bradypterus_thoracicus", breeding_point[,1])

S1 <- "Bradypterus_thoracicus"
S2 <- "Locustella_kashmirensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Locustella_luteoventris", "Bradypterus_luteoventris", breeding_point[,1])
breeding_point[,1] <- gsub("Locustella_major", "Bradypterus_major", breeding_point[,1])
breeding_point[,1] <- gsub("Locustella_mandelli", "Bradypterus_mandelli", breeding_point[,1])
breeding_point[,1] <- gsub("Locustella_montis", "Bradypterus_montis", breeding_point[,1])
breeding_point[,1] <- gsub("Locustella_tacsanowskia", "Bradypterus_tacsanowskius", breeding_point[,1])
breeding_point[,1] <- gsub("Lonchura_oryzivora", "Padda_oryzivora", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_alboterminatus", "Tockus_alboterminatus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_bradfieldi", "Tockus_bradfieldi", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_camurus", "Tockus_camurus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_fasciatus", "Tockus_fasciatus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_hemprichii", "Tockus_hemprichii", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_nasutus", "Tockus_nasutus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophoceros_pallidirostris", "Tockus_pallidirostris", breeding_point[,1])

S1 <- "Tockus_fasciatus"
S2 <- "Lophoceros_semifasciatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lophophanes_cristatus", "Parus_cristatus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophophanes_dichrous", "Parus_dichrous", breeding_point[,1])
breeding_point[,1] <- gsub("Lophorina_intercedens", "Ptiloris_intercedens", breeding_point[,1])
breeding_point[,1] <- gsub("Lophorina_magnifica", "Ptiloris_magnificus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophorina_paradisea", "Ptiloris_paradiseus", breeding_point[,1])
breeding_point[,1] <- gsub("Lophorina_victoriae", "Ptiloris_victoriae", breeding_point[,1])

S1 <- "Lophornis_chalybeus"
S2 <- "Lophornis_verreauxii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lophotis_", "Eupodotis_", breeding_point[,1])

S1 <- "Lophura_erythrophthalma"
S2 <- "Lophura_pyronota"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lophura_ignita"
S2 <- "Lophura_rufa"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lybius_leucocephalus"
S2 <- "Lybius_leucogaster"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Lybius_leucocephalus"
S2 <- "Lybius_senex"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Lyncornis_macrotis", "Eurostopodus_macrotis", breeding_point[,1])
breeding_point[,1] <- gsub("Lyncornis_temminckii", "Eurostopodus_temminckii", breeding_point[,1])
breeding_point[,1] <- gsub("Lyrurus_mlokosiewiczi", "Tetrao_mlokosiewiczi", breeding_point[,1])
breeding_point[,1] <- gsub("Lyrurus_tetrix", "Tetrao_tetrix", breeding_point[,1])

S1 <- "Machaeropterus_regulus"
S2 <- "Machaeropterus_striolatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Machlolophus_nuchalis", "Parus_nuchalis", breeding_point[,1])
breeding_point[,1] <- gsub("Machlolophus_spilonotus", "Parus_spilonotus", breeding_point[,1])
breeding_point[,1] <- gsub("Machlolophus_xanthogenys", "Parus_xanthogenys", breeding_point[,1])
breeding_point[,1] <- gsub("Macronus_ptilosus", "Macronous_ptilosus", breeding_point[,1])
breeding_point[,1] <- gsub("Macronyx_fuelleborni", "Macronyx_fuellebornii", breeding_point[,1])

S1 <- "Malacoptila_striata"
S2 <- "Malacoptila_minor"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Mareca_", "Anas_", breeding_point[,1])
breeding_point[,1] <- gsub("Maschalethraupis_surinama", "Tachyphonus_surinamus", breeding_point[,1])
breeding_point[,1] <- gsub("Mazaria_propinqua", "Synallaxis_propinqua", breeding_point[,1])
breeding_point[,1] <- gsub("Megalampitta_gigantea", "Melampitta_gigantea", breeding_point[,1])
breeding_point[,1] <- gsub("Megaloprepia_magnifica", "Ptilinopus_magnificus", breeding_point[,1])
breeding_point[,1] <- gsub("Megapodius_decollatus", "Megapodius_affinis", breeding_point[,1])

S1 <- "Megascops_guatemalae"
S2 <- "Megascops_vermiculatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Meiglyptes_tristis"
S2 <- "Meiglyptes_grammithorax"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Melaenornis_brunneus", "Dioptrornis_brunneus", breeding_point[,1])
breeding_point[,1] <- gsub("Melaenornis_chocolatinus", "Dioptrornis_chocolatinus", breeding_point[,1])
breeding_point[,1] <- gsub("Melaenornis_fischeri", "Dioptrornis_fischeri", breeding_point[,1])
breeding_point[,1] <- gsub("Melaenornis_herero", "Namibornis_herero", breeding_point[,1])
breeding_point[,1] <- gsub("Melaenornis_semipartitus", "Empidornis_semipartitus", breeding_point[,1])
breeding_point[,1] <- gsub("Melaenornis_silens", "Sigelus_silens", breeding_point[,1])
breeding_point[,1] <- gsub("Melaniparus_", "Parus_", breeding_point[,1])

S1 <- "Melanitta_fusca"
S2 <- "Melanitta_deglandi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Melanitta_nigra"
S2 <- "Melanitta_americana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Melanitta_fusca"
S2 <- "Melanitta_stejnegeri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Melanorectes_nigrescens", "Pitohui_nigrescens", breeding_point[,1])
breeding_point[,1] <- gsub("Melanospiza_bicolor", "Tiaris_bicolor", breeding_point[,1])

S1 <- "Melipotes_fumigatus"
S2 <- "Melipotes_carolae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Melithreptus_lunatus"
S2 <- "Melithreptus_chloropsis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Melithreptus_gularis"
S2 <- "Melithreptus_laetior"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Melloria_quoyi", "Cracticus_quoyi", breeding_point[,1])
breeding_point[,1] <- gsub("Melozone_aberti", "Pipilo_aberti", breeding_point[,1])
breeding_point[,1] <- gsub("Melozone_albicollis", "Pipilo_albicollis", breeding_point[,1])
breeding_point[,1] <- gsub("Melozone_crissalis", "Pipilo_crissalis", breeding_point[,1])
breeding_point[,1] <- gsub("Melozone_fusca", "Pipilo_fuscus", breeding_point[,1])

S1 <- "Melozone_leucotis"
S2 <- "Melozone_occipitalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Merops_orientalis"
S2 <- "Merops_cyanophrys"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Merops_orientalis"
S2 <- "Merops_viridissimus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Merops_variegatus"
S2 <- "Merops_lafresnayii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Merops_muelleri"
S2 <- "Merops_mentalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Microcarbo_", "Phalacrocorax_", breeding_point[,1])
breeding_point[,1] <- gsub("Phalacrocorax_pygmaeus", "Phalacrocorax_pygmeus", breeding_point[,1])

S1 <- "Microeca_flavigaster"
S2 <- "Microeca_tormenti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Micronisus_gabar", "Melierax_gabar", breeding_point[,1])
breeding_point[,1] <- gsub("Micropternus_brachyurus", "Celeus_brachyurus", breeding_point[,1])
breeding_point[,1] <- gsub("Microptilotis_", "Meliphaga_", breeding_point[,1])
breeding_point[,1] <- gsub("Meliphaga_albilineatus", "Meliphaga_albilineata", breeding_point[,1])
breeding_point[,1] <- gsub("Meliphaga_albonotatus", "Meliphaga_albonotata", breeding_point[,1])
breeding_point[,1] <- gsub("Meliphaga_analogus", "Meliphaga_analoga", breeding_point[,1])

S1 <- "Meliphaga_gracilis"
S2 <- "Meliphaga_cinereifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Meliphaga_fordianus", "Meliphaga_fordiana", breeding_point[,1])

S1 <- "Meliphaga_albilineata"
S2 <- "Meliphaga_fordiana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Meliphaga_montanus", "Meliphaga_montana", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_alticola", "Poospiza_alticola", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_cabanisi", "Poospiza_cabanisi", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_cinereus", "Poospiza_cinerea", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_erythrophrys", "Poospiza_erythrophrys", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_lateralis", "Poospiza_lateralis", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_melanoleucus", "Poospiza_melanoleuca", breeding_point[,1])
breeding_point[,1] <- gsub("Microspingus_torquatus", "Poospiza_torquata", breeding_point[,1])

S1 <- "Poospiza_torquata"
S2 <- "Microspingus_pectoralis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Microspingus_trifasciatus", "Hemispingus_trifasciatus", breeding_point[,1])
breeding_point[,1] <- gsub("Microtarsus_melanoleucos", "Pycnonotus_melanoleucos", breeding_point[,1])

S1 <- "Mionectes_olivaceus"
S2 <- "Mionectes_galbinus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Mionectes_macconnelli"
S2 <- "Mionectes_roraimae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Mirafra_apiata"
S2 <- "Mirafra_fasciolata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Mirafra_africana"
S2 <- "Mirafra_sharpii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Mixornis_bornensis", "Macronous_bornensis", breeding_point[,1])
breeding_point[,1] <- gsub("Mixornis_flavicollis", "Macronous_flavicollis", breeding_point[,1])
breeding_point[,1] <- gsub("Mixornis_gularis", "Macronous_gularis", breeding_point[,1])
breeding_point[,1] <- gsub("Mixornis_kelleyi", "Macronous_kelleyi", breeding_point[,1])

S1 <- "Momotus_momota"
S2 <- "Momotus_coeruliceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Momotus_momota"
S2 <- "Momotus_lessonii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Momotus_momota"
S2 <- "Momotus_subrufescens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Monticola_cinclorhyncha", "Monticola_cinclorhynchus", breeding_point[,1])
breeding_point[,1] <- gsub("Monticola_semirufus", "Myrmecocichla_semirufa", breeding_point[,1])

S1 <- "Montifringilla_nivalis"
S2 <- "Montifringilla_henrici"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Morphnarchus_", "Leucopternis_", breeding_point[,1])
breeding_point[,1] <- gsub("Motacilla_maderaspatensis", "Motacilla_madaraspatensis", breeding_point[,1])

S1 <- "Motacilla_flava"
S2 <- "Motacilla_tschutschensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Moupinia_poecilotis", "Chrysomma_poecilotis", breeding_point[,1])

S1 <- "Muscisaxicola_rufivertex"
S2 <- "Muscisaxicola_occipitalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiagra_inquieta"
S2 <- "Myiagra_nana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myioborus_ornatus"
S2 <- "Myioborus_chrysops"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiodynastes_maculatus"
S2 <- "Myiodynastes_solitarius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiomela_leucura"
S2 <- "Myiomela_cambodiana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Myiomela_diana", "Cinclidium_diana", breeding_point[,1])
breeding_point[,1] <- gsub("Myiomela_leucura", "Cinclidium_leucurum", breeding_point[,1])

S1 <- "Myiopagis_caniceps"
S2 <- "Myiopagis_cinerea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiopagis_caniceps"
S2 <- "Myiopagis_parambae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiophobus_fasciatus"
S2 <- "Myiophobus_crypterythrus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiophobus_fasciatus"
S2 <- "Myiophobus_rufescens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiopsitta_monachus"
S2 <- "Myiopsitta_luchsi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Myiothlypis_", "Basileuterus_", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_bivittata", "Basileuterus_bivittatus", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_conspicillata", "Basileuterus_conspicillatus", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_coronata", "Basileuterus_coronatus", breeding_point[,1])

S1 <- "Basileuterus_luteoviridis"
S2 <- "Basileuterus_euophrys"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Basileuterus_flaveola", "Basileuterus_flaveolus", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_fulvicauda", "Phaeothlypis_fulvicauda", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_leucoblephara", "Basileuterus_leucoblepharus", breeding_point[,1])
breeding_point[,1] <- gsub("Basileuterus_rivularis", "Phaeothlypis_rivularis", breeding_point[,1])

S1 <- "Phaeothlypis_rivularis"
S2 <- "Basileuterus_mesoleucus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Basileuterus_nigrocristata", "Basileuterus_nigrocristatus", breeding_point[,1])

S1 <- "Basileuterus_bivittatus"
S2 <- "Basileuterus_roraimae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Basileuterus_signata", "Basileuterus_signatus", breeding_point[,1])

S1 <- "Basileuterus_luteoviridis"
S2 <- "Basileuterus_striaticeps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myiotriccus_ornatus"
S2 <- "Myiotriccus_phoenicurus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Myrmeciza_immaculata"
S2 <- "Myrmeciza_zeledoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Myrmecocichla_arnotti", "Myrmecocichla_arnoti", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmecocichla_monticola", "Oenanthe_monticola", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_brunneiceps", "Schistocichla_brunneiceps", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_caurensis", "Schistocichla_caurensis", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_humaythae", "Schistocichla_humaythae", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_hyperythrus", "Myrmeciza_hyperythra", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_leucostigma", "Schistocichla_leucostigma", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_rufifacies", "Schistocichla_rufifacies", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_saturatus", "Schistocichla_saturata", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmelastes_schistaceus", "Schistocichla_schistacea", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmoborus_lophotes", "Percnostola_lophotes", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmoderus_", "Myrmeciza_", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmeciza_ferrugineus", "Myrmeciza_ferruginea", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmeciza_loricatus", "Myrmeciza_loricata", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmeciza_squamosus", "Myrmeciza_squamosa", breeding_point[,1])
breeding_point[,1] <- gsub("Myrmophylax_atrothorax", "Myrmeciza_atrothorax", breeding_point[,1])

S1 <- "Myrmornis_torquata"
S2 <- "Myrmornis_stictoptera"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nectarinia_afer", "Nectarinia_afra", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_asiaticus", "Nectarinia_asiatica", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_bifasciatus", "Nectarinia_bifasciata", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_brasiliana", "Nectarinia_sperata", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_chalybeus", "Nectarinia_chalybea", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_chloropygius", "Nectarinia_chloropygia", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_coccinigastrus", "Nectarinia_coccinigaster", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_cupreus", "Nectarinia_cuprea", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_erythrocercus", "Nectarinia_erythrocerca", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_fuscus", "Nectarinia_fusca", breeding_point[,1])

S1 <- "Nectarinia_manoensis"
S2 <- "Nectarinia_gertrudis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nectarinia_habessinicus", "Nectarinia_habessinica", breeding_point[,1])

S1 <- "Nectarinia_habessinica"
S2 <- "Nectarinia_hellmayri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Nectarinia_jugularis"
S2 <- "Nectarinia_idenburgi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nectarinia_lotenius", "Nectarinia_lotenia", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_pulchellus", "Nectarinia_pulchella", breeding_point[,1])

S1 <- "Nectarinia_pulchella"
S2 <- "Nectarinia_melanogastrus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nectarinia_minullus", "Nectarinia_minulla", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_neergaardi", "Nectarinia_neergardi", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_regius", "Nectarinia_regia", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_superbus", "Nectarinia_superba", breeding_point[,1])

S1 <- "Nectarinia_mediocris"
S2 <- "Nectarinia_usambaricus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nectarinia_venustus", "Nectarinia_venusta", breeding_point[,1])
breeding_point[,1] <- gsub("Nectarinia_verreauxii", "Nectarinia_veroxii", breeding_point[,1])

S1 <- "Neochmia_phaeton"
S2 <- "Neochmia_evangelinae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Neophedina_cincta", "Riparia_cincta", breeding_point[,1])
breeding_point[,1] <- gsub("Neosuthora_davidiana", "Paradoxornis_davidianus", breeding_point[,1])
breeding_point[,1] <- gsub("Nephelomyias_", "Myiophobus_", breeding_point[,1])
breeding_point[,1] <- gsub("Nesoptilotis_leucotis", "Lichenostomus_leucotis", breeding_point[,1])
breeding_point[,1] <- gsub("Niltava_oatesi", "Niltava_vivida", breeding_point[,1])

S1 <- "Ninox_scutulata"
S2 <- "Ninox_japonica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Notharchus_tectus"
S2 <- "Notharchus_subtectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Notopholia_corusca", "Lamprotornis_corruscus", breeding_point[,1])

S1 <- "Nucifraga_caryocatactes"
S2 <- "Nucifraga_hemispila"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Nucifraga_caryocatactes"
S2 <- "Nucifraga_multipunctata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Nycticryphes_", "Rostratula_", breeding_point[,1])
breeding_point[,1] <- gsub("Nyctidromus_anthonyi", "Caprimulgus_anthonyi", breeding_point[,1])
breeding_point[,1] <- gsub("Nyctipolus", "Caprimulgus", breeding_point[,1])
breeding_point[,1] <- gsub("Nystactes_noanamae", "Bucco_noanamae", breeding_point[,1])
breeding_point[,1] <- gsub("Nystactes_tamatia", "Bucco_tamatia", breeding_point[,1])
breeding_point[,1] <- gsub("Ochetorhynchus_melanurus", "Ochetorhynchus_melanura", breeding_point[,1])
breeding_point[,1] <- gsub("Ochthoeca_salvini", "Tumbezia_salvini", breeding_point[,1])
breeding_point[,1] <- gsub("Ochthoeca_superciliosa", "Ochthoeca_fumicolor", breeding_point[,1])
breeding_point[,1] <- gsub("Odontospiza_griseicapilla", "Lonchura_griseicapilla", breeding_point[,1])
breeding_point[,1] <- gsub("Oedistoma_iliolophus", "Toxorhamphus_iliolophus", breeding_point[,1])
breeding_point[,1] <- gsub("Oenanthe_albifrons", "Myrmecocichla_albifrons", breeding_point[,1])
breeding_point[,1] <- gsub("Oenanthe_dubia", "Cercomela_dubia", breeding_point[,1])
breeding_point[,1] <- gsub("Oenanthe_familiaris", "Cercomela_familiaris", breeding_point[,1])

S1 <- "Oenanthe_bottae"
S2 <- "Oenanthe_frenata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Oenanthe_fusca", "Cercomela_fusca", breeding_point[,1])

S1 <- "Oenanthe_bottae"
S2 <- "Oenanthe_heuglinii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Oenanthe_melanura", "Cercomela_melanura", breeding_point[,1])
breeding_point[,1] <- gsub("Oenanthe_scotocerca", "Cercomela_scotocerca", breeding_point[,1])

S1 <- "Oenanthe_oenanthe"
S2 <- "Oenanthe_seebohmi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Oneillornis_lunulatus", "Gymnopithys_lunulatus", breeding_point[,1])
breeding_point[,1] <- gsub("Oneillornis_salvini", "Gymnopithys_salvini", breeding_point[,1])
breeding_point[,1] <- gsub("Onychoprion_anaethetus", "Sterna_anaethetus", breeding_point[,1])
breeding_point[,1] <- gsub("Onychostruthus_taczanowskii", "Montifringilla_taczanowskii", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Ophrysia_superciliosa"), ]

breeding_point[,1] <- gsub("Oreocossypha_isabellae", "Cossypha_isabellae", breeding_point[,1])
breeding_point[,1] <- gsub("Oreolais_pulcher", "Oreolais_pulchra", breeding_point[,1])

S1 <- "Oreonympha_nobilis"
S2 <- "Oreonympha_albolimbata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Oreophilais_robertsi", "Prinia_robertsi", breeding_point[,1])
breeding_point[,1] <- gsub("Oreothlypis_gutturalis", "Parula_gutturalis", breeding_point[,1])
breeding_point[,1] <- gsub("Oreothlypis_superciliosa", "Parula_superciliosa", breeding_point[,1])

S1 <- "Oreotrochilus_estella"
S2 <- "Oreotrochilus_stolzmanni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Oriolus_brachyrynchus", "Oriolus_brachyrhynchus", breeding_point[,1])

S1 <- "Oriolus_cruentus"
S2 <- "Oriolus_consanguineus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Oriolus_oriolus"
S2 <- "Oriolus_kundoo"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ornorectes_cristatus", "Pitohui_cristatus", breeding_point[,1])
breeding_point[,1] <- gsub("Orochelidon_andecola", "Haplochelidon_andecola", breeding_point[,1])
breeding_point[,1] <- gsub("Orochelidon_flavipes", "Notiochelidon_flavipes", breeding_point[,1])
breeding_point[,1] <- gsub("Orochelidon_murina", "Notiochelidon_murina", breeding_point[,1])

S1 <- "Ortalis_guttata"
S2 <- "Ortalis_araucuan"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ortalis_guttata"
S2 <- "Ortalis_columbiana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ortalis_guttata"
S2 <- "Ortalis_squamata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Orthopsittaca_manilatus", "Orthopsittaca_manilata", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Orthotomus_chaktomuk"), ]

S1 <- "Otidiphaps_nobilis"
S2 <- "Otidiphaps_cervicalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Otocichla_mupinensis", "Turdus_mupinensis", breeding_point[,1])

S1 <- "Otus_senegalensis"
S2 <- "Otus_pamelae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Oxypogon_guerinii"
S2 <- "Oxypogon_cyanolaemus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Oxypogon_guerinii"
S2 <- "Oxypogon_lindenii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Oxyura_jamaicensis"
S2 <- "Oxyura_ferruginea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pachycephala_cinerea", "Pachycephala_grisola", breeding_point[,1])

S1 <- "Pachycephala_pectoralis"
S2 <- "Pachycephala_fulvotincta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pachycephala_simplex"
S2 <- "Pachycephala_griseiceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pachyramphus_viridis"
S2 <- "Pachyramphus_griseigularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pachyramphus_major"
S2 <- "Pachyramphus_uropygialis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pachysylvia_", "Hylophilus_", breeding_point[,1])
breeding_point[,1] <- gsub("Hylophilus_decurtata", "Hylophilus_decurtatus", breeding_point[,1])
breeding_point[,1] <- gsub("Hylophilus_hypochrysea", "Vireo_hypochryseus", breeding_point[,1])
breeding_point[,1] <- gsub("Hylophilus_hypoxantha", "Hylophilus_hypoxanthus", breeding_point[,1])
breeding_point[,1] <- gsub("Hylophilus_muscicapina", "Hylophilus_muscicapinus", breeding_point[,1])
breeding_point[,1] <- gsub("Hylophilus_semibrunnea", "Hylophilus_semibrunneus", breeding_point[,1])
breeding_point[,1] <- gsub("Paludipasser_locustella", "Ortygospiza_locustella", breeding_point[,1])
breeding_point[,1] <- gsub("Parabuteo_leucorrhous", "Buteo_leucorrhous", breeding_point[,1])
breeding_point[,1] <- gsub("Paradisornis_rudolphi", "Paradisaea_rudolphi", breeding_point[,1])

S1 <- "Paramythia_montium"
S2 <- "Paramythia_olivacea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pardaliparus_venustulus", "Parus_venustulus", breeding_point[,1])

breeding_point[,1] <- gsub("Parkesia", "Seiurus", breeding_point[,1])

S1 <- "Paroaria_gularis"
S2 <- "Paroaria_nigrogenis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Parus_leucomelas"
S2 <- "Parus_guineensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Parus_rufiventris"
S2 <- "Parus_pallidiventris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Passer_cinnamomeus", "Passer_rutilans", breeding_point[,1])

S1 <- "Passer_domesticus"
S2 <- "Passer_italiae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Passer_simplex"
S2 <- "Passer_zarudnyi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Passerculus_", "Ammodramus_", breeding_point[,1])
breeding_point[,1] <- gsub("Ammodramus_sandwichensis", "Passerculus_sandwichensis", breeding_point[,1])

S1 <- "Passerculus_sandwichensis"
S2 <- "Ammodramus_guttatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Passerculus_sandwichensis"
S2 <- "Ammodramus_rostratus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Passerella_arborea", "Spizella_arborea", breeding_point[,1])

S1 <- "Passerella_iliaca"
S2 <- "Passerella_megarhyncha"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Passerella_iliaca"
S2 <- "Passerella_schistacea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Passerella_iliaca"
S2 <- "Passerella_unalaschcensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pastor_roseus", "Sturnus_roseus", breeding_point[,1])

S1 <- "Patagioenas_fasciata"
S2 <- "Patagioenas_albilinea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Patagioenas_maculosa"
S2 <- "Patagioenas_albipennis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Peliperdix_", "Francolinus_", breeding_point[,1])
breeding_point[,1] <- gsub("Pellorneum_nigrocapitatum", "Pellorneum_capistratum", breeding_point[,1])
breeding_point[,1] <- gsub("Peltohyas_australis", "Charadrius_australis", breeding_point[,1])
breeding_point[,1] <- gsub("Peneoenanthe_pulverulenta", "Eopsaltria_pulverulenta", breeding_point[,1])
breeding_point[,1] <- gsub("Peneothello_sigillata", "Peneothello_sigillatus", breeding_point[,1])

S1 <- "Pericrocotus_erythropygius"
S2 <- "Pericrocotus_albifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pericrocotus_solaris"
S2 <- "Pericrocotus_montanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Periparus_ater", "Parus_ater", breeding_point[,1])
breeding_point[,1] <- gsub("Periparus_rubidiventris", "Parus_rubidiventris", breeding_point[,1])
breeding_point[,1] <- gsub("Periparus_rufonuchalis", "Parus_rufonuchalis", breeding_point[,1])
breeding_point[,1] <- gsub("Pernis_ptilorhynchus", "Pernis_ptilorhyncus", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_ariel", "Hirundo_ariel", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_fluvicola", "Hirundo_fluvicola", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_fuliginosa", "Hirundo_fuliginosa", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_nigricans", "Hirundo_nigricans", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_preussi", "Hirundo_preussi", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_rufigula", "Hirundo_rufigula", breeding_point[,1])
breeding_point[,1] <- gsub("Petrochelidon_spilodera", "Hirundo_spilodera", breeding_point[,1])
breeding_point[,1] <- gsub("Petroica_boodang", "Petroica_multicolor", breeding_point[,1])
breeding_point[,1] <- gsub("Peucaea", "Aimophila", breeding_point[,1])

S1 <- "Phacellodomus_ferrugineigula"
S2 <- "Phacellodomus_erythrophthalmus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phacellodomus_rufifrons"
S2 <- "Phacellodomus_inornatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phaethornis_aethopygus", "Phaethornis_aethopyga", breeding_point[,1])

S1 <- "Phaethornis_longuemareus"
S2 <- "Phaethornis_aethopyga"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phaethornis_longirostris"
S2 <- "Phaethornis_baroni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phaethornis_longirostris"
S2 <- "Phaethornis_mexicanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phaethornis_griseogularis"
S2 <- "Phaethornis_porcullae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phalacrocorax_bougainvilliorum", "Phalacrocorax_bougainvillii", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Phalacrocorax_perspicillatus"), ]

breeding_point[,1] <- gsub("Phalcoboenus_chimango", "Milvago_chimango", breeding_point[,1])
breeding_point[,1] <- gsub("Phedinopsis_brazzae", "Phedina_brazzae", breeding_point[,1])
breeding_point[,1] <- gsub("Phelpsia_inornata", "Phelpsia_inornatus", breeding_point[,1])
breeding_point[,1] <- gsub("Pheugopedius_", "Thryothorus_", breeding_point[,1])

S1 <- "Phibalura_flavirostris"
S2 <- "Phibalura_boliviana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Philohydor_lictor", "Pitangus_lictor", breeding_point[,1])

breeding_point[,1] <- gsub("Phlegopsis_borbae", "Skutchia_borbae", breeding_point[,1])

S1 <- "Phodilus_badius"
S2 <- "Phodilus_assimilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phoenicurus_coeruleocephala", "Phoenicurus_caeruleocephala", breeding_point[,1])
breeding_point[,1] <- gsub("Phoenicurus_fuliginosus", "Rhyacornis_fuliginosa", breeding_point[,1])
breeding_point[,1] <- gsub("Phoenicurus_leucocephalus", "Chaimarrornis_leucocephalus", breeding_point[,1])
breeding_point[,1] <- gsub("Pholia_sharpii", "Cinnyricinclus_sharpii", breeding_point[,1])
breeding_point[,1] <- gsub("Phonygammus_keraudrenii", "Manucodia_keraudrenii", breeding_point[,1])

S1 <- "Phyllanthus_atripennis"
S2 <- "Phyllanthus_bohndorffi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phyllanthus_atripennis"
S2 <- "Phyllanthus_rubiginosus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phyllastrephus_debilis"
S2 <- "Phyllastrephus_albigula"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phyllastrephus_albigularis"
S2 <- "Phyllastrephus_viridiceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phyllergates_cucullatus", "Orthotomus_cuculatus", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_burkii", "Seicercus_burkii", breeding_point[,1])

S1 <- "Phylloscopus_ricketti"
S2 <- "Phylloscopus_calciatilis"    #Lumping to the presumed closest relative
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phylloscopus_castaniceps", "Seicercus_castaniceps", breeding_point[,1])

S1 <- "Phylloscopus_borealis"
S2 <- "Phylloscopus_examinandus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Phylloscopus_borealis"
S2 <- "Phylloscopus_xanthodryas"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phylloscopus_grammiceps", "Seicercus_grammiceps", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_intensior", "Phylloscopus_davisoni", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_intermedius", "Seicercus_affinis", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_montis", "Seicercus_montis", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_omeiensis", "Seicercus_omeiensis", breeding_point[,1])

S1 <- "Phylloscopus_trochiloides"
S2 <- "Phylloscopus_plumbeitarsus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phylloscopus_poliogenys", "Seicercus_poliogenys", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_soror", "Seicercus_soror", breeding_point[,1])

S1 <- "Seicercus_grammiceps"
S2 <- "Phylloscopus_sumatrensis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phylloscopus_tephrocephalus", "Seicercus_tephrocephalus", breeding_point[,1])

S1 <- "Phylloscopus_collybita"
S2 <- "Phylloscopus_tristis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Phylloscopus_valentini", "Seicercus_valentini", breeding_point[,1])
breeding_point[,1] <- gsub("Phylloscopus_whistleri", "Seicercus_whistleri", breeding_point[,1])

S1 <- "Piaya_cayana"
S2 <- "Piaya_mexicana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pica_pica"
S2 <- "Pica_asirensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pica_pica"
S2 <- "Pica_mauritanica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pica_nutalli", "Pica_nuttalli", breeding_point[,1])
breeding_point[,1] <- gsub("Picoides_canicapillus", "Dendrocopos_canicapillus", breeding_point[,1])

S1 <- "Picoides_tridactylus"
S2 <- "Picoides_funebris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Picoides_kizuki", "Dendrocopos_kizuki", breeding_point[,1])
breeding_point[,1] <- gsub("Picoides_moluccensis", "Dendrocopos_moluccensis", breeding_point[,1])
breeding_point[,1] <- gsub("Picoides_nanus", "Dendrocopos_nanus", breeding_point[,1])

S1 <- "Picus_canus"
S2 <- "Picus_dedemi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Picus_canus"
S2 <- "Picus_guerini"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Picus_viridis"
S2 <- "Picus_sharpei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Piezorina_cinerea", "Piezorhina_cinerea", breeding_point[,1])
breeding_point[,1] <- gsub("Pinarochroa_sordida", "Cercomela_sordida", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Pinguinus_impennis"), ]

S1 <- "Pionites_leucogaster"
S2 <- "Pionites_xanthomerius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pionites_leucogaster"
S2 <- "Pionites_xanthurus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pionus_menstruus"
S2 <- "Pionus_reichenowi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pionus_tumultuosus"
S2 <- "Pionus_seniloides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pipile_cumanensis"
S2 <- "Pipile_grayi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pipraeidea_bonariensis", "Thraupis_bonariensis", breeding_point[,1])

S1 <- "Thraupis_bonariensis"
S2 <- "Pipraeidea_darwinii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pipreola_frontalis"
S2 <- "Pipreola_squamipectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pitohui_kirhocephalus"
S2 <- "Pitohui_uropygialis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pitta_arquata", "Pitta_arcuata", breeding_point[,1])
breeding_point[,1] <- gsub("Pitta_caeruleus", "Pitta_caerulea", breeding_point[,1])
breeding_point[,1] <- gsub("Pitta_cyaneus", "Pitta_cyanea", breeding_point[,1])
breeding_point[,1] <- gsub("Pitta_guajanus", "Pitta_guajana", breeding_point[,1])

S1 <- "Pitta_guajana"
S2 <- "Pitta_irena"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pitta_guajana"
S2 <- "Pitta_schwaneri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pitta_macklotii", "Pitta_erythrogaster", breeding_point[,1])

S1 <- "Pitta_sordida"
S2 <- "Pitta_novaeguineae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Platyrinchus_mystaceus"
S2 <- "Platyrinchus_albogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Platysmurus_leucopterus"
S2 <- "Platysmurus_aterrimus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Platysteira_concreta"
S2 <- "Platysteira_ansorgei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Platysteira_castanea"
S2 <- "Platysteira_hormophora"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Plesiodryas_albonotata", "Poecilodryas_albonotata", breeding_point[,1])

S1 <- "Ploceus_nigricollis"
S2 <- "Ploceus_brachypterus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ploceus_nigerrimus"
S2 <- "Ploceus_castaneofuscus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ploceus_katangae"
S2 <- "Ploceus_upembae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Podiceps_andinus"), ]

S1 <- "Podiceps_occipitalis"
S2 <- "Podiceps_juninensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Poecile_", "Parus_", breeding_point[,1])

S1 <- "Parus_lugubris"
S2 <- "Parus_hyrcanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Poeoptera_femoralis", "Cinnyricinclus_femoralis", breeding_point[,1])
breeding_point[,1] <- gsub("Pogoniulus_coryphaea", "Pogoniulus_coryphaeus", breeding_point[,1])
breeding_point[,1] <- gsub("Pogonornis_", "Lybius_", breeding_point[,1])

S1 <- "Lybius_minor"
S2 <- "Lybius_macclounii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pogonotriccus_", "Phylloscartes_", breeding_point[,1])
breeding_point[,1] <- gsub("Poliocrania_exsul", "Myrmeciza_exsul", breeding_point[,1])

S1 <- "Myrmeciza_exsul"
S2 <- "Poliocrania_maculifer"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Polioptila_plumbea"
S2 <- "Polioptila_maior"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pomatorhinus_ferruginosus"
S2 <- "Pomatorhinus_phayrei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pomatorhinus_superciliaris", "Xiphirhynchus_superciliaris", breeding_point[,1])
breeding_point[,1] <- gsub("Poodytes_albolimbatus", "Megalurus_albolimbatus", breeding_point[,1])
breeding_point[,1] <- gsub("Poodytes_carteri", "Eremiornis_carteri", breeding_point[,1])
breeding_point[,1] <- gsub("Poodytes_gramineus", "Megalurus_gramineus", breeding_point[,1])
breeding_point[,1] <- gsub("Poospiza_baeri", "Compsospiza_baeri", breeding_point[,1])
breeding_point[,1] <- gsub("Poospiza_garleppi", "Compsospiza_garleppi", breeding_point[,1])
breeding_point[,1] <- gsub("Poospiza_goeringi", "Hemispingus_goeringi", breeding_point[,1])
breeding_point[,1] <- gsub("Poospiza_rufosuperciliaris", "Hemispingus_rufosuperciliaris", breeding_point[,1])
breeding_point[,1] <- gsub("Poospizopsis_caesar", "Poospiza_caesar", breeding_point[,1])
breeding_point[,1] <- gsub("Poospizopsis_hypocondria", "Poospiza_hypochondria", breeding_point[,1])
breeding_point[,1] <- gsub("Porphyrio_martinicus", "Porphyrio_martinica", breeding_point[,1])
breeding_point[,1] <- gsub("Porzana_fasciata", "Anurolimnas_fasciatus", breeding_point[,1])
breeding_point[,1] <- gsub("Premnornis_guttuliger", "Premnornis_guttuligera", breeding_point[,1])
breeding_point[,1] <- gsub("Prinia_erythroptera", "Heliolais_erythropterus", breeding_point[,1])

S1 <- "Prinia_atrogularis"
S2 <- "Prinia_khasiana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Prinia_rufifrons", "Spiloptila_rufifrons", breeding_point[,1])

S1 <- "Prinia_flaviventris"
S2 <- "Prinia_sonitans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Prinia_atrogularis"
S2 <- "Prinia_superciliaris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Procarduelis_nipalensis", "Carpodacus_nipalensis", breeding_point[,1])

S1 <- "Psarocolius_angustifrons"
S2 <- "Psarocolius_oleagineus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psarocolius_bifasciatus"
S2 <- "Psarocolius_yuracares"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Psephotellus_", "Psephotus_", breeding_point[,1])
breeding_point <- breeding_point[-which(breeding_point[,1]=="Psephotus_pulcherrimus"), ]
breeding_point[,1] <- gsub("Pseudastur_", "Leucopternis_", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudocolaptes_boissonneauii", "Pseudocolaptes_boissonneautii", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudocolaptes_johnsoni", "Pseudocolaptes_lawrencii_johnsoni", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudocolaptes_lawrencii", "Pseudocolaptes_lawrencii_lawrencii", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudocolaptes_lawrencii_lawrencii_johnsoni", "Pseudocolaptes_lawrencii_johnsoni", breeding_point[,1])

S1 <- "Pseudocolopteryx_flaviventris"
S2 <- "Pseudocolopteryx_citreola"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pseudopipra_pipra", "Pipra_pipra", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudorectes_ferrugineus", "Pitohui_ferrugineus", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudorectes_incertus", "Pitohui_incertus", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudosaltator_rufiventris", "Saltator_rufiventris", breeding_point[,1])

S1 <- "Pseudoseisura_unirufa"
S2 <- "Pseudoseisura_cristata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pseudospingus_verticalis", "Hemispingus_verticalis", breeding_point[,1])
breeding_point[,1] <- gsub("Pseudospingus_xanthophthalmus", "Hemispingus_xanthophthalmus", breeding_point[,1])
breeding_point[,1] <- gsub("Psilopogon_", "Megalaima_", breeding_point[,1])

S1 <- "Megalaima_oorti"
S2 <- "Megalaima_annamensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Megalaima_asiaticus", "Megalaima_asiatica", breeding_point[,1])

S1 <- "Megalaima_franklinii"
S2 <- "Megalaima_auricularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Megalaima_corvinus", "Megalaima_corvina", breeding_point[,1])

S1 <- "Megalaima_australis"
S2 <- "Megalaima_cyanotis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Megalaima_australis"
S2 <- "Megalaima_duvaucelii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Megalaima_eximius", "Megalaima_eximia", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_faiostrictus", "Megalaima_faiostricta", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_haemacephalus", "Megalaima_haemacephala", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_incognitus", "Megalaima_incognita", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_lineatus", "Megalaima_lineata", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_malabaricus", "Megalaima_malabarica", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_pulcherrimus", "Megalaima_pulcherrima", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_pyrolophus", "Megalaima_pyrolopha", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_zeylanicus", "Megalaima_zeylanica", breeding_point[,1])
breeding_point[,1] <- gsub("Megalaima_malabarica", "Megalaima_rubricapillus", breeding_point[,1])  # rubricapillus doesn't occur in our points
breeding_point[,1] <- gsub("Megalaima_pyrolopha", "Psilopogon_pyrolophus", breeding_point[,1])
breeding_point[,1] <- gsub("Psiloscops_f", "Otus_f", breeding_point[,1])
breeding_point[,1] <- gsub("Psittacara_", "Aratinga_", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_mitratus", "Aratinga_mitrata", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_leucophthalmus", "Aratinga_leucophthalma", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_holochlorus", "Aratinga_holochlora", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_frontatus", "Aratinga_frontata", breeding_point[,1])
breeding_point[,1] <- gsub("Aratinga_acuticaudatus", "Aratinga_acuticaudata", breeding_point[,1])

S1 <- "Aratinga_wagleri"
S2 <- "Aratinga_frontata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psittacella_picta"
S2 <- "Psittacella_lorentzi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psittaculirostris_desmarestii"
S2 <- "Psittaculirostris_cervicalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psittaculirostris_desmarestii"
S2 <- "Psittaculirostris_godmani"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psittacus_erithacus"
S2 <- "Psittacus_timneh"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Psittiparus_ruficeps", "Paradoxornis_ruficeps", breeding_point[,1])

S1 <- "Paradoxornis_ruficeps"
S2 <- "Psittiparus_bakeri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Psittiparus_gularis", "Paradoxornis_gularis", breeding_point[,1])
breeding_point[,1] <- gsub("Psittiparus_margaritae", "Paradoxornis_margaritae", breeding_point[,1])

S1 <- "Psophia_viridis"
S2 <- "Psophia_dextralis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psophia_viridis"
S2 <- "Psophia_obscura"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psophia_crepitans"
S2 <- "Psophia_ochroptera"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Psophocichla_litsitsirupa"
S2 <- "Psophocichla_simensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pternistis_", "Francolinus_", breeding_point[,1])

S1 <- "Francolinus_castaneicollis"
S2 <- "Francolinus_atrifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pterodroma_deserta", "Pterodroma_feae", breeding_point[,1])

S1 <- "Pterodroma_macroptera"
S2 <- "Pterodroma_gouldi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pteroglossus_torquatus"
S2 <- "Pteroglossus_erythropygius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pteroglossus_inscriptus"
S2 <- "Pteroglossus_humboldti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pteroglossus_torquatus"
S2 <- "Pteroglossus_sanguineus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pteroglossus_bitorquatus"
S2 <- "Pteroglossus_sturmii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pteruthius_flaviscapis"
S2 <- "Pteruthius_aeralatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ptilinopus_alligator", "Ptilinopus_cinctus", breeding_point[,1]) 
S1 <- "Ptilinopus_ornatus"
S2 <- "Ptilinopus_gestroi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ptilinopus_nainus", "Ptilinopus_naina", breeding_point[,1]) 
breeding_point[,1] <- gsub("Ptiliogonys_caudatus", "Ptilogonys_caudatus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Ptiliogonys_cinereus", "Ptilogonys_cinereus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Ptilopachus_nahani", "Francolinus_nahani", breeding_point[,1]) 
breeding_point[,1] <- gsub("Ptilopsis_leucotis", "Otus_leucotis", breeding_point[,1]) 

S1 <- "Ptilorrhoa_caerulescens"
S2 <- "Ptilorrhoa_geislerorum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ptilotula_", "Lichenostomus_", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lichenostomus_fusca", "Lichenostomus_fuscus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lichenostomus_ornata", "Lichenostomus_ornatus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lichenostomus_penicillata", "Lichenostomus_penicillatus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Lichenostomus_plumula", "Lichenostomus_plumulus", breeding_point[,1]) 
breeding_point[,1] <- gsub("Ptyonoprogne_", "Hirundo_", breeding_point[,1]) 

S1 <- "Puffinus_assimilis"
S2 <- "Puffinus_elegans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Puffinus_lherminieri"
S2 <- "Puffinus_persicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Puffinus_lherminieri"
S2 <- "Puffinus_subalaris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Purnella_albifrons", "Phylidonyris_albifrons", breeding_point[,1])

S1 <- "Pycnonotus_finlaysoni"
S2 <- "Pycnonotus_davisoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pycnonotus_dispar", "Pycnonotus_melanicterus", breeding_point[,1])

S1 <- "Pycnonotus_melanicterus"
S2 <- "Pycnonotus_flaviventris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pycnonotus_melanicterus"
S2 <- "Pycnonotus_gularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pycnonotus_flavescens"
S2 <- "Pycnonotus_leucops"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pycnonotus_melanicterus"
S2 <- "Pycnonotus_montis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pycnonotus_bimaculatus"
S2 <- "Pycnonotus_snouckaerti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Pygochelidon_melanoleuca", "Atticora_melanoleuca", breeding_point[,1])
breeding_point[,1] <- gsub("Pyrgilauda_blanfordi", "Montifringilla_blanfordi", breeding_point[,1])
breeding_point[,1] <- gsub("Pyrgilauda_davidiana", "Montifringilla_davidiana", breeding_point[,1])
breeding_point[,1] <- gsub("Pyrgilauda_ruficollis", "Montifringilla_ruficollis", breeding_point[,1])
breeding_point[,1] <- gsub("Pyrgilauda_theresae", "Montifringilla_theresae", breeding_point[,1])
breeding_point[,1] <- gsub("Pyrrholaemus_sagittatus", "Chthonicola_sagittatus", breeding_point[,1])

S1 <- "Pyrrhula_nipalensis"
S2 <- "Pyrrhula_waterstradti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_amazonum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_caeruleiceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_eisenmanni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_leucotis"
S2 <- "Pyrrhura_emma"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_lucianii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_melanura"
S2 <- "Pyrrhura_pacifica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_parvifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_peruviana"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_roseifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_snethlageae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Pyrrhura_picta"
S2 <- "Pyrrhura_subandina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Quoyornis_georgianus", "Eopsaltria_georgiana", breeding_point[,1])
breeding_point[,1] <- gsub("Radjah_radjah", "Tadorna_radjah", breeding_point[,1])
breeding_point[,1] <- gsub("Rallicula_forbesi", "Rallina_forbesi", breeding_point[,1])
breeding_point[,1] <- gsub("Rallicula_leucospila", "Rallina_leucospila", breeding_point[,1])
breeding_point[,1] <- gsub("Rallicula_mayri", "Rallina_mayri", breeding_point[,1])
breeding_point[,1] <- gsub("Rallicula_rubra", "Rallina_rubra", breeding_point[,1])

S1 <- "Rallus_longirostris"
S2 <- "Rallus_crepitans"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rallus_aquaticus"
S2 <- "Rallus_indicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rallus_longirostris"
S2 <- "Rallus_obsoletus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rallus_elegans"
S2 <- "Rallus_tenuirostris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ramphastos_vitellinus"
S2 <- "Ramphastos_ariel"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ramphastos_vitellinus"
S2 <- "Ramphastos_citrolaemus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ramphastos_vitellinus"
S2 <- "Ramphastos_culminatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Ramphastos_tucanus"
S2 <- "Ramphastos_cuvieri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Ramphiculus_jambu", "Ptilinopus_jambu", breeding_point[,1])
breeding_point[,1] <- gsub("Ramphocoris_clotbey", "Rhamphocoris_clotbey", breeding_point[,1])
breeding_point[,1] <- gsub("Reinwardtoena_reinwardti", "Reinwardtoena_reinwardtsi", breeding_point[,1])
breeding_point[,1] <- gsub("Rhabdotorrhinus_corrugatus", "Aceros_corrugatus", breeding_point[,1])
breeding_point[,1] <- gsub("Rhamphocharis_crassirostris", "Melanocharis_crassirostris", breeding_point[,1])

S1 <- "Melanocharis_crassirostris"
S2 <- "Rhamphocharis_piperata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rhea_pennata"
S2 <- "Rhea_tarapacensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rhinopomastus_castaneiceps", "Phoeniculus_castaneiceps", breeding_point[,1])
breeding_point[,1] <- gsub("Rhinortha_chlorophaea", "Phaenicophaeus_chlorophaeus", breeding_point[,1])
breeding_point[,1] <- gsub("Rhipidura_albiscapa", "Rhipidura_fuliginosa", breeding_point[,1])

S1 <- "Rhipidura_albicollis"
S2 <- "Rhipidura_albogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rhipidura_rufifrons"
S2 <- "Rhipidura_dryas"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rhipidura_isura", "Rhipidura_rufiventris", breeding_point[,1])

S1 <- "Rhizothera_longirostris"
S2 <- "Rhizothera_dulitensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Rhodopechys_sanguineus"
S2 <- "Rhodopechys_alienus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rhodospiza_obsoleta", "Rhodopechys_obsoletus", breeding_point[,1])
breeding_point[,1] <- gsub("Rhopias_gularis", "Myrmotherula_gularis", breeding_point[,1])

S1 <- "Rhopophilus_pekinensis"
S2 <- "Rhopophilus_albosuperciliaris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rhopospina_fruticeti", "Phrygilus_fruticeti", breeding_point[,1])

S1 <- "Rhynchocyclus_olivaceus"
S2 <- "Rhynchocyclus_aequinoctialis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rhynchophanes_", "Calcarius_", breeding_point[,1])
breeding_point[,1] <- gsub("Rhynchospiza_", "Aimophila_", breeding_point[,1])
breeding_point[,1] <- gsub("Rhyticeros_", "Aceros_", breeding_point[,1])
breeding_point[,1] <- gsub("Ridgwayia_pinicola", "Zoothera_pinicola", breeding_point[,1])
breeding_point[,1] <- gsub("Rimator_danjoui", "Jabouilleia_danjoui", breeding_point[,1])

S1 <- "Jabouilleia_danjoui"
S2 <- "Rimator_naungmungensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Riparia_paludicola"
S2 <- "Riparia_chinensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Riparia_riparia"
S2 <- "Riparia_diluta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Rufirallus_", "Anurolimnas_", breeding_point[,1])
breeding_point[,1] <- gsub("Rupornis_", "Buteo_", breeding_point[,1])

S1 <- "Sakesphorus_canadensis"
S2 <- "Sakesphorus_pulchellus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Salpornis_spilonota", "Salpornis_spilonotus", breeding_point[,1])

S1 <- "Salpornis_spilonotus"
S2 <- "Salpornis_salvadori"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Saltator_coerulescens"
S2 <- "Saltator_grandis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Saltator_multicolor", "Saltatricula_multicolor", breeding_point[,1])

S1 <- "Saltator_coerulescens"
S2 <- "Saltator_plumbeus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sappho_sparganurus", "Sappho_sparganura", breeding_point[,1])

S1 <- "Sarkidiornis_melanotos"
S2 <- "Sarkidiornis_sylvicola"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Saroglossa_spilopterus", "Saroglossa_spiloptera", breeding_point[,1])
breeding_point[,1] <- gsub("Saundersilarus_saundersi", "Larus_saundersi", breeding_point[,1])
breeding_point[,1] <- gsub("Scepomycter_winifredae", "Bathmocercus_winifredae", breeding_point[,1])

S1 <- "Schiffornis_turdina"
S2 <- "Schiffornis_aenea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Schiffornis_turdina"
S2 <- "Schiffornis_olivacea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Schiffornis_turdina"
S2 <- "Schiffornis_stenorhyncha"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Schiffornis_turdina"
S2 <- "Schiffornis_veraepacis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Schistes_geoffroyi"
S2 <- "Schistes_albogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Schistolais_leontica", "Prinia_leontica", breeding_point[,1])
breeding_point[,1] <- gsub("Schistolais_leucopogon", "Prinia_leucopogon", breeding_point[,1])
breeding_point[,1] <- gsub("Schoeniparus_brunneus", "Alcippe_brunnea", breeding_point[,1])
breeding_point[,1] <- gsub("Schoeniparus_", "Alcippe_", breeding_point[,1])
breeding_point[,1] <- gsub("Alcippe_cinereus", "Alcippe_cinerea", breeding_point[,1])
breeding_point[,1] <- gsub("Alcippe_dubius", "Alcippe_dubia", breeding_point[,1])
breeding_point[,1] <- gsub("Sciaphylax_", "Myrmeciza_", breeding_point[,1])
breeding_point[,1] <- gsub("Scleroptila_", "Francolinus_", breeding_point[,1])
breeding_point[,1] <- gsub("Francolinus_afra", "Francolinus_africanus", breeding_point[,1])
breeding_point[,1] <- gsub("Francolinus_psilolaema", "Francolinus_psilolaemus", breeding_point[,1])

S1 <- "Francolinus_psilolaemus"
S2 <- "Francolinus_elgonensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Francolinus_gutturalis", "Francolinus_levaillantoides", breeding_point[,1])
breeding_point[,1] <- gsub("Francolinus_streptophora", "Francolinus_streptophorus", breeding_point[,1])

S1 <- "Francolinus_shelleyi"
S2 <- "Francolinus_whytei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sclerurus_scansor"
S2 <- "Sclerurus_cearensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Scytalopus_speluncae"
S2 <- "Scytalopus_diamantinensis"
# Note, this wasn't a split, but diamantina is known to be of the speluncae group
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Scytalopus_gonzagai"), ]

breeding_point[,1] <- gsub("Scytalopus_opacus", "Scytalopus_canus", breeding_point[,1])
# Note, the above was a split, but the range-restricted Scytalopus canus, from which opaca was split, doesn't show up in our points

breeding_point <- breeding_point[-which(breeding_point[,1]=="Scytalopus_perijanus"), ]

breeding_point[,1] <- gsub("Selasphorus_calliope", "Stellula_calliope", breeding_point[,1])

S1 <- "Scytalopus_speluncae"
S2 <- "Scytalopus_petrophilus"
# Note, this wasn't a split, but petrophilus is known to be of the speluncae group
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Selenidera_reinwardtii"
S2 <- "Selenidera_langsdorffii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sericulus_aureus"
S2 <- "Sericulus_ardens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Serilophus_lunatus"
S2 <- "Serilophus_rubropygius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Serinus_gularis"
S2 <- "Serinus_canicapilla"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Serinus_capistrata", "Serinus_capistratus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_dorsostriata", "Serinus_dorsostriatus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_hyposticta", "Serinus_hypostictus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_leucoptera", "Serinus_leucopterus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_leucopygia", "Serinus_leucopygius", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_melanochroa", "Serinus_melanochrous", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_mozambica", "Serinus_mozambicus", breeding_point[,1])

S1 <- "Serinus_atrogularis"
S2 <- "Serinus_reichenowi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Serinus_reichardi"
S2 <- "Serinus_striatipectus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Serinus_striolata", "Serinus_striolatus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_sulphurata", "Serinus_sulphuratus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_tristriata", "Serinus_tristriatus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_xantholaema", "Serinus_xantholaemus", breeding_point[,1])
breeding_point[,1] <- gsub("Serinus_xanthopygia", "Serinus_xanthopygius", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Serpophaga_griseicapilla"), ]

breeding_point[,1] <- gsub("Setopagis_", "Caprimulgus_", breeding_point[,1])
breeding_point[,1] <- gsub("Caprimulgus_heterura", "Caprimulgus_heterurus", breeding_point[,1])
breeding_point[,1] <- gsub("Caprimulgus_parvula", "Caprimulgus_parvulus", breeding_point[,1])
breeding_point[,1] <- gsub("Setophaga_", "Dendroica_", breeding_point[,1])

S1 <- "Dendroica_coronata"
S2 <- "Dendroica_auduboni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Dendroica_americana", "Parula_americana", breeding_point[,1])
breeding_point[,1] <- gsub("Dendroica_citrina", "Wilsona_citrina", breeding_point[,1])
breeding_point[,1] <- gsub("Wilsona_citrina", "Wilsonia_citrina", breeding_point[,1])
breeding_point[,1] <- gsub("Dendroica_pitiayumi", "Parula_pitiayumi", breeding_point[,1])
breeding_point[,1] <- gsub("Dendroica_ruticilla", "Setophaga_ruticilla", breeding_point[,1])

S1 <- "Sheppardia_bocagei"
S2 <- "Sheppardia_poensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sheppardia_polioptera", "Cossypha_polioptera", breeding_point[,1])
breeding_point[,1] <- gsub("Sibia_nipalensis", "Actinodura_nipalensis", breeding_point[,1])
breeding_point[,1] <- gsub("Sibia_souliei", "Actinodura_souliei", breeding_point[,1])
breeding_point[,1] <- gsub("Sibia_waldeni", "Actinodura_waldeni", breeding_point[,1])
breeding_point[,1] <- gsub("Sibirionetta_formosa", "Anas_formosa", breeding_point[,1])

S1 <- "Sicalis_olivascens"
S2 <- "Sicalis_mendozae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sicalis_uropigyalis", "Sicalis_uropygialis", breeding_point[,1])
breeding_point[,1] <- gsub("Silvicultrix_", "Ochthoeca_", breeding_point[,1])
breeding_point[,1] <- gsub("Sinosuthora_alphonsiana", "Paradoxornis_alphonsianus", breeding_point[,1])
breeding_point[,1] <- gsub("Sinosuthora_brunnea", "Paradoxornis_brunneus", breeding_point[,1])
breeding_point[,1] <- gsub("Sinosuthora_conspicillata", "Paradoxornis_conspicillatus", breeding_point[,1])
breeding_point[,1] <- gsub("Sinosuthora_przewalskii", "Paradoxornis_przewalskii", breeding_point[,1])

S1 <- "Paradoxornis_brunneus"
S2 <- "Sinosuthora_ricketti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sinosuthora_webbiana", "Paradoxornis_webbianus", breeding_point[,1])
breeding_point[,1] <- gsub("Sinosuthora_zappeyi", "Paradoxornis_zappeyi", breeding_point[,1])
breeding_point[,1] <- gsub("Sipia_", "Myrmeciza_", breeding_point[,1])

S1 <- "Myrmeciza_laemosticta"
S2 <- "Myrmeciza_palliata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sirystes_sibilator"
S2 <- "Sirystes_albocinereus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sirystes_sibilator"
S2 <- "Sirystes_subcanescens"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sitta_europaea"
S2 <- "Sitta_arctica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sitta_castanea"
S2 <- "Sitta_cinnamoventris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sitta_europaea"
S2 <- "Sitta_neglecta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sitta_leucopsis"
S2 <- "Sitta_przewalskii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sittasomus_griseicapillus"
S2 <- "Sittasomus_griseus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sittiparus_varius", "Parus_varius", breeding_point[,1])
breeding_point[,1] <- gsub("Siva_cyanouroptera", "Minla_cyanouroptera", breeding_point[,1])
breeding_point[,1] <- gsub("Smutsornis_africanus", "Rhinoptilus_africanus", breeding_point[,1])
breeding_point[,1] <- gsub("Spatula_", "Anas_", breeding_point[,1])
breeding_point[,1] <- gsub("Spermestes_bicolor", "Lonchura_bicolor", breeding_point[,1])
breeding_point[,1] <- gsub("Spermestes_cucullata", "Lonchura_cucullata", breeding_point[,1])
breeding_point[,1] <- gsub("Spermestes_fringilloides", "Lonchura_fringilloides", breeding_point[,1])
breeding_point[,1] <- gsub("Sphenopsis_", "Hemispingus_", breeding_point[,1])
breeding_point[,1] <- gsub("Spilopelia_chinensis", "Stigmatopelia_chinensis", breeding_point[,1])

S1 <- "Stigmatopelia_chinensis"
S2 <- "Spilopelia_suratensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Spilopelia_senegalensis", "Stigmatopelia_senegalensis", breeding_point[,1])
breeding_point[,1] <- gsub("Spinus_atratus", "Carduelis_atrata", breeding_point[,1])
breeding_point[,1] <- gsub("Spinus_", "Carduelis_", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_barbatus", "Carduelis_barbata", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_cucullatus", "Carduelis_cucullata", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_magellanicus", "Carduelis_magellanica", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_notatus", "Carduelis_notata", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_olivaceus", "Carduelis_olivacea", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_thibetanus", "Serinus_thibetanus", breeding_point[,1])
breeding_point[,1] <- gsub("Carduelis_xanthogastrus", "Carduelis_xanthogastra", breeding_point[,1])
breeding_point[,1] <- gsub("Spizocorys_fremantlii", "Pseudalaemon_fremantlii", breeding_point[,1])
breeding_point[,1] <- gsub("Spizocorys_starki", "Eremalauda_starki", breeding_point[,1])
breeding_point[,1] <- gsub("Spodiopsar_cineraceus", "Sturnus_cineraceus", breeding_point[,1])
breeding_point[,1] <- gsub("Spodiopsar_sericeus", "Sturnus_sericeus", breeding_point[,1])
breeding_point[,1] <- gsub("Spodiornis_rusticus", "Haplospiza_rustica", breeding_point[,1])
breeding_point[,1] <- gsub("Sporathraupis_cyanocephala", "Thraupis_cyanocephala", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_angolensis", "Oryzoborus_angolensis", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_atrirostris", "Oryzoborus_atrirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_crassirostris", "Oryzoborus_crassirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_funerea", "Oryzoborus_funereus", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_maximiliani", "Oryzoborus_maximiliani", breeding_point[,1])
breeding_point[,1] <- gsub("Sporophila_nuttingi", "Oryzoborus_nuttingi", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Sporophila_beltoni"), ]

breeding_point[,1] <- gsub("Sporophila_fringilloides", "Dolospingus_fringilloides", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Sporophila_iberaensis"), ]

S1 <- "Sporophila_torqueola"
S2 <- "Sporophila_morelleti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sporophila_corvina"
S2 <- "Sporophila_ophthalmica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sporophila_bouvreuil"
S2 <- "Sporophila_pileata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Stachyris_erythropterum", "Stachyris_erythroptera", breeding_point[,1])
breeding_point[,1] <- gsub("Stachyris_humei", "Sphenocichla_humei", breeding_point[,1])
breeding_point[,1] <- gsub("Stachyris_roberti", "Sphenocichla_roberti", breeding_point[,1])
breeding_point[,1] <- gsub("Stachyris_strialata", "Stachyris_striolata", breeding_point[,1])
breeding_point[,1] <- gsub("Stachyris_chrysaeum", "Stachyris_chrysaea", breeding_point[,1])

S1 <- "Stachyris_erythroptera"
S2 <- "Stachyris_bicolor"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Stactolaema_leucotis"
S2 <- "Stactolaema_leucogrammica"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Stelgidillas_gracilirostris", "Andropadus_gracilirostris", breeding_point[,1])

S1 <- "Stelgidopteryx_serripennis"
S2 <- "Stelgidopteryx_ridgwayi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Stephanoxis_lalandi"
S2 <- "Stephanoxis_loddigesii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sternula_", "Sterna_", breeding_point[,1])

S1 <- "Stigmatura_napensis"
S2 <- "Stigmatura_bahiae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Stiphrornis_erythrothorax"
S2 <- "Stiphrornis_pyrrholaemus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Stiphrornis_erythrothorax"
S2 <- "Stiphrornis_xanthogaster"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Stomiopera_flava", "Lichenostomus_flavus", breeding_point[,1])
breeding_point[,1] <- gsub("Stomiopera_unicolor", "Lichenostomus_unicolor", breeding_point[,1])

S1 <- "Streptopelia_decaocto"
S2 <- "Streptopelia_xanthocycla"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Strix_aluco"
S2 <- "Strix_nivicolum"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Strix_omanensis"), ]

S1 <- "Struthio_camelus"
S2 <- "Struthio_molybdophanes"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sturnia_malabarica", "Sturnus_malabaricus", breeding_point[,1])
breeding_point[,1] <- gsub("Sturnia_pagodarum", "Sturnus_pagodarum", breeding_point[,1])
breeding_point[,1] <- gsub("Sturnia_sinensis", "Sturnus_sinensis", breeding_point[,1])
breeding_point[,1] <- gsub("Sugomel_nigrum", "Certhionyx_niger", breeding_point[,1])
breeding_point[,1] <- gsub("Suiriri_affinis", "Suiriri_islerorum", breeding_point[,1])

S1 <- "Surniculus_lugubris"
S2 <- "Surniculus_dicruroides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Suthora_fulvifrons", "Paradoxornis_fulvifrons", breeding_point[,1])
breeding_point[,1] <- gsub("Suthora_nipalensis", "Paradoxornis_nipalensis", breeding_point[,1])
breeding_point[,1] <- gsub("Suthora_verreauxi", "Paradoxornis_verreauxi", breeding_point[,1])
breeding_point[,1] <- gsub("Sylvia_abyssinica", "Pseudoalcippe_abyssinica", breeding_point[,1])

S1 <- "Pseudoalcippe_abyssinica"
S2 <- "Sylvia_atriceps"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sylvia_hortensis"
S2 <- "Sylvia_crassirostris"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sylvia_nana"
S2 <- "Sylvia_deserti"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sylvia_nigricapillus", "Lioptilus_nigricapillus", breeding_point[,1])
breeding_point[,1] <- gsub("Sylvia_ruppeli", "Sylvia_rueppelli", breeding_point[,1])

S1 <- "Sylvia_cantillans"
S2 <- "Sylvia_subalpina"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sylvia_subcoerulea", "Sylvia_subcaerulea", breeding_point[,1])

S1 <- "Sylvietta_leucophrys"
S2 <- "Sylvietta_chapini"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Sylvietta_brachyura"
S2 <- "Sylvietta_leucopsis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Sylviorthorhynchus_desmurii", "Sylviorthorhynchus_desmursii", breeding_point[,1])
breeding_point[,1] <- gsub("Sylviorthorhynchus_yanacensis", "Leptasthenura_yanacensis", breeding_point[,1])
breeding_point[,1] <- gsub("Symposiachrus_axillaris", "Monarcha_axillaris", breeding_point[,1])
breeding_point[,1] <- gsub("Symposiachrus_guttula", "Monarcha_guttula", breeding_point[,1])
breeding_point[,1] <- gsub("Symposiachrus_manadensis", "Monarcha_manadensis", breeding_point[,1])
breeding_point[,1] <- gsub("Symposiachrus_rubiensis", "Monarcha_rubiensis", breeding_point[,1])
breeding_point[,1] <- gsub("Symposiachrus_trivirgatus", "Monarcha_trivirgatus", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Synallaxis_beverlyae"), ]

S1 <- "Synallaxis_stictothorax"
S2 <- "Synallaxis_chinchipensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Synallaxis_cinerea"), ]
breeding_point <- breeding_point[-which(breeding_point[,1]=="Synallaxis_fuscorufa"), ]

breeding_point[,1] <- gsub("Synallaxis_hellmayri", "Gyalophylax_hellmayri", breeding_point[,1])
breeding_point[,1] <- gsub("Synallaxis_hypochondriaca", "Siptornopsis_hypochondriaca", breeding_point[,1])

S1 <- "Synallaxis_ruficapilla"
S2 <- "Synallaxis_infuscata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Synallaxis_albilora"
S2 <- "Synallaxis_simoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Syndactyla_striata", "Simoxenops_striatus", breeding_point[,1])
breeding_point[,1] <- gsub("Syndactyla_ucayalae", "Simoxenops_ucayalae", breeding_point[,1])
breeding_point[,1] <- gsub("Synoicus_chinensis", "Coturnix_chinensis", breeding_point[,1])

S1 <- "Coturnix_chinensis"
S2 <- "Synoicus_adansonii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Synoicus_ypsilophorus", "Coturnix_ypsilophora", breeding_point[,1])
breeding_point[,1] <- gsub("Systellura", "Caprimulgus", breeding_point[,1])

S1 <- "Caprimulgus_longirostris"
S2 <- "Caprimulgus_decussata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Caprimulgus_longirostris"
S2 <- "Caprimulgus_roraimae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Taccocua_leschenaultii", "Phaenicophaeus_leschenaultii", breeding_point[,1])
breeding_point[,1] <- gsub("Tachyphonus_cristata", "Tachyphonus_cristatus", breeding_point[,1])
breeding_point[,1] <- gsub("Tachyphonus_luctuosa", "Tachyphonus_luctuosus", breeding_point[,1])
breeding_point[,1] <- gsub("Taeniopygia_castanotis", "Taeniopygia_guttata", breeding_point[,1])
breeding_point[,1] <- gsub("Tangara_abbas", "Thraupis_abbas", breeding_point[,1])
breeding_point[,1] <- gsub("Tangara_cyanoptera", "Thraupis_cyanoptera", breeding_point[,1])
breeding_point[,1] <- gsub("Tangara_argentea", "Tangara_cyanoptera", breeding_point[,1])

S1 <- "Tangara_vassorii"
S2 <- "Tangara_atrocoerulea"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tangara_arthus"
S2 <- "Tangara_aurulenta"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tangara_mexicana"
S2 <- "Tangara_brasiliensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tangara_velia"
S2 <- "Tangara_cyanomelas"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tangara_episcopus", "Thraupis_episcopus", breeding_point[,1])

S1 <- "Tangara_cayana"
S2 <- "Tangara_flava"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tangara_ruficervix"
S2 <- "Tangara_fulvicervix"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tangara_glaucocolpa", "Thraupis_glaucocolpa", breeding_point[,1])

S1 <- "Tangara_parzudakii"
S2 <- "Tangara_lunigera"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tangara_ornata", "Thraupis_ornata", breeding_point[,1])
breeding_point[,1] <- gsub("Tangara_palmarum", "Thraupis_palmarum", breeding_point[,1])
breeding_point[,1] <- gsub("Tangara_sayaca", "Thraupis_sayaca", breeding_point[,1])

S1 <- "Tangara_cyanoptera"
S2 <- "Tangara_whitelyi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tarsiger_cyanurus"
S2 <- "Tarsiger_rufilatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tauraco_schuettii", "Tauraco_schuetti", breeding_point[,1])
breeding_point[,1] <- gsub("Tephrodornis_sylvicola", "Tephrodornis_gularis", breeding_point[,1])

S1 <- "Tephrodornis_gularis"
S2 <- "Tephrodornis_virgatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tephrophilus_wetmorei", "Buthraupis_wetmorei", breeding_point[,1])

S1 <- "Terpsiphone_paradisi"
S2 <- "Terpsiphone_affinis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Terpsiphone_rufocinerea"
S2 <- "Terpsiphone_batesi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Terpsiphone_paradisi"
S2 <- "Terpsiphone_incei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tetrao_urogalloides", "Tetrao_parvirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Thalassarche_melanophris", "Thalassarche_melanophrys", breeding_point[,1])
breeding_point[,1] <- gsub("Thalasseus_", "Sterna_", breeding_point[,1])
breeding_point[,1] <- gsub("Sterna_maximus", "Sterna_maxima", breeding_point[,1])

S1 <- "Thamnistes_anabatinus"
S2 <- "Thamnistes_aequatorialis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Thamnolaea_cinnamomeiventris", "Myrmecocichla_cinnamomeiventris", breeding_point[,1])
breeding_point[,1] <- gsub("Thamnophilus_bernardi", "Sakesphorus_bernardi", breeding_point[,1])
breeding_point[,1] <- gsub("Thamnophilus_melanonotus", "Sakesphorus_melanonotus", breeding_point[,1])
breeding_point[,1] <- gsub("Thamnophilus_melanothorax", "Sakesphorus_melanothorax", breeding_point[,1])

S1 <- "Sakesphorus_bernardi"
S2 <- "Thamnophilus_shumbae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Thamnophilus_ruficapillus"
S2 <- "Thamnophilus_subfasciatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Theristicus_melanopis"
S2 <- "Theristicus_branickii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Thinornis_cucullatus", "Thinornis_rubricollis", breeding_point[,1])
breeding_point[,1] <- gsub("Thlypopsis_pyrrhocoma", "Pyrrhocoma_ruficeps", breeding_point[,1])
breeding_point[,1] <- gsub("Thlypopsis_superciliaris", "Hemispingus_superciliaris", breeding_point[,1])
breeding_point[,1] <- gsub("Threskiornis_moluccus", "Threskiornis_molucca", breeding_point[,1])
breeding_point[,1] <- gsub("Thripophaga_gutturata", "Cranioleuca_gutturata", breeding_point[,1])

breeding_point <- breeding_point[-which(breeding_point[,1]=="Thripophaga_macroura"), ]

breeding_point[,1] <- gsub("Thryophilus_", "Thryothorus_", breeding_point[,1])

S1 <- "Thryothorus_ludovicianus"
S2 <- "Thryothorus_albinucha"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Thryothorus_sclateri"
S2 <- "Thryothorus_columbianus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Thryothorus_sclateri"
S2 <- "Thryothorus_paucimaculatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Thryothorus_euophrys"
S2 <- "Thryothorus_schulenbergi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Thryothorus_sernai"), ]

S1 <- "Thryothorus_modestus"
S2 <- "Thryothorus_zeledoni"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tityra_cayana"
S2 <- "Tityra_braziliensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tityra_inquisitor"
S2 <- "Tityra_leucura"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Tockus_erythrorhynchus"
S2 <- "Tockus_damarensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Todiramphus_saurophagus", "Todiramphus_saurophaga", breeding_point[,1])
breeding_point[,1] <- gsub("Trachylaemus_purpuratus", "Trachyphonus_purpuratus", breeding_point[,1])

S1 <- "Trachyphonus_purpuratus"
S2 <- "Trachylaemus_goffinii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]


S1 <- "Trachyphonus_purpuratus"
S2 <- "Trachylaemus_togoensis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Trachyphonus_darnaudii"
S2 <- "Trachyphonus_emini"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Treron_affinis", "Treron_pompadora", breeding_point[,1]) # pompadora not in our points

S1 <- "Treron_pompadora"
S2 <- "Treron_phayrei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Treron_calvus"
S2 <- "Treron_delalandii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tribonyx_ventralis", "Gallinula_ventralis", breeding_point[,1]) # pompadora not in our points
breeding_point[,1] <- gsub("Trichastoma_malaccense", "Malacocincla_malaccensis", breeding_point[,1]) # pompadora not in our points

S1 <- "Trichoglossus_haematodus"
S2 <- "Trichoglossus_moluccanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Trichoglossus_haematodus"
S2 <- "Trichoglossus_rubritorquis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tringa_brevipes", "Heteroscelus_brevipes", breeding_point[,1])
breeding_point[,1] <- gsub("Tringa_incana", "Heteroscelus_incanus", breeding_point[,1])
breeding_point[,1] <- gsub("Tringa_semipalmata", "Catoptrophorus_semipalmatus", breeding_point[,1])
breeding_point[,1] <- gsub("Trochalopteron_affine", "Garrulax_affinis", breeding_point[,1])
breeding_point[,1] <- gsub("Trochalopteron_", "Garrulax_", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_cachinnans", "Strophocincla_cachinnans", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_chrysopterum", "Garrulax_chrysopterus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_erythrocephalum", "Garrulax_erythrocephalus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_fairbanki", "Strophocincla_fairbanki", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_formosum", "Garrulax_formosus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_imbricatum", "Garrulax_imbricatus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_lineatum", "Garrulax_lineatus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_squamatum", "Garrulax_squamatus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_variegatum", "Garrulax_variegatus", breeding_point[,1])
breeding_point[,1] <- gsub("Garrulax_virgatum", "Garrulax_virgatus", breeding_point[,1])

S1 <- "Trochocercus_cyanomelas"
S2 <- "Trochocercus_bivittatus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Troglodytes_troglodytes"
S2 <- "Troglodytes_hiemalis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Troglodytes_troglodytes"
S2 <- "Troglodytes_pacificus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Trogon_elegans"
S2 <- "Trogon_ambiguus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Trogon_surrucura"
S2 <- "Trogon_aurantius"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tunchiornis_", "Hylophilus_", breeding_point[,1])

S1 <- "Hylophilus_ochraceiceps"
S2 <- "Hylophilus_luteifrons"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Turdinus_brevicaudatus", "Napothera_brevicaudata", breeding_point[,1])
breeding_point[,1] <- gsub("Turdinus_crispifrons", "Gypsophila_crispifrons", breeding_point[,1])

S1 <- "Gypsophila_crispifrons"
S2 <- "Turdinus_calcicola"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Turdinus_crassus", "Napothera_crassa", breeding_point[,1])
breeding_point[,1] <- gsub("Turdinus_marmoratus", "Turdinus_marmorata", breeding_point[,1])
breeding_point[,1] <- gsub("Turdoides_reinwardtii", "Turdoides_reinwardii", breeding_point[,1])

S1 <- "Turdoides_striata"
S2 <- "Turdoides_somervillei"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_olivaceus"
S2 <- "Turdus_abyssinicus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_chiguanco"
S2 <- "Turdus_anthracinus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_ignobilis"
S2 <- "Turdus_arthuri"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_ruficollis"
S2 <- "Turdus_atrogularis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_migratorius"
S2 <- "Turdus_confinis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_ignobilis"
S2 <- "Turdus_debilis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_naumanni"
S2 <- "Turdus_eunomus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Turdus_libonyana", "Turdus_libonyanus", breeding_point[,1])

S1 <- "Turdus_merula"
S2 <- "Turdus_mandarinus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_merula"
S2 <- "Turdus_maximus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point <- breeding_point[-which(breeding_point[,1]=="Turdus_sanchezorum"), ]

S1 <- "Turdus_merula"
S2 <- "Turdus_simillimus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turdus_olivaceus"
S2 <- "Turdus_smithi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Turnix_hottentottus"
S2 <- "Turnix_nanus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Tychaedon_barbata", "Erythropygia_barbata", breeding_point[,1])
breeding_point[,1] <- gsub("Tychaedon_coryphoeus", "Erythropygia_coryphaeus", breeding_point[,1])
breeding_point[,1] <- gsub("Tychaedon_leucosticta", "Erythropygia_leucosticta", breeding_point[,1])
breeding_point[,1] <- gsub("Tychaedon_quadrivirgata", "Erythropygia_quadrivirgata", breeding_point[,1])
breeding_point[,1] <- gsub("Tychaedon_signata", "Erythropygia_signata", breeding_point[,1])

S1 <- "Tyto_tenebricosa"
S2 <- "Tyto_multipunctata"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Urochroa_bougueri"
S2 <- "Urochroa_leucura"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Urocissa_erythroryncha", "Urocissa_erythrorhyncha", breeding_point[,1])
breeding_point[,1] <- gsub("Urocissa_xanthomelana", "Urocissa_whiteheadi", breeding_point[,1]) # Whiteheadi is island of Hainan only
breeding_point[,1] <- gsub("Uromyias_agilis", "Anairetes_agilis", breeding_point[,1]) 
breeding_point[,1] <- gsub("Uromyias_agraphia", "Anairetes_agraphia", breeding_point[,1]) 

breeding_point <- breeding_point[-which(breeding_point[,1]=="Vanellus_macropterus"), ]

breeding_point[,1] <- gsub("Vanellus_malabaricus", "Vanellus_malarbaricus", breeding_point[,1]) 

S1 <- "Vanellus_miles"
S2 <- "Vanellus_novaehollandiae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Vauriella_gularis", "Rhinomyias_gularis", breeding_point[,1])
breeding_point[,1] <- gsub("Veles_binotatus", "Caprimulgus_binotatus", breeding_point[,1])
breeding_point[,1] <- gsub("Vermivora_cyanoptera", "Vermivora_pinus", breeding_point[,1])
breeding_point[,1] <- gsub("Verreauxia_africana", "Sasia_africana", breeding_point[,1])
breeding_point[,1] <- gsub("Vireo_sclateri", "Hylophilus_sclateri", breeding_point[,1])

S1 <- "Vireolanius_leucotis"
S2 <- "Vireolanius_mikettae"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Willisornis_poecilinotus"
S2 <- "Willisornis_vidua"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Willisornis_poecilinotus"
S2 <- "Willisornis_nigrigula"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Xenodacnis_parina"
S2 <- "Xenodacnis_petersi"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Xenops_minutus", "Xenops_minutus_minutus", breeding_point[,1])
breeding_point[,1] <- gsub("Xenops_genibarbis", "Xenops_minutus_littoralis", breeding_point[,1])
breeding_point[,1] <- gsub("Xenops_rutilus", "Xenops_rutilans", breeding_point[,1])

S1 <- "Xiphorhynchus_erythropygius"
S2 <- "Xiphorhynchus_aequatorialis"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Xiphorhynchus_fuscus"
S2 <- "Xiphorhynchus_atlanticus"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Xiphorhynchus_ocellatus"
S2 <- "Xiphorhynchus_beauperthuysii"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Xiphorhynchus_guttatus"
S2 <- "Xiphorhynchus_guttatoides"
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

breeding_point[,1] <- gsub("Zanclostomus_javanicus", "Phaenicophaeus_javanicus", breeding_point[,1])
breeding_point[,1] <- gsub("Zanda_baudinii", "Calyptorhynchus_baudinii", breeding_point[,1])
breeding_point[,1] <- gsub("Zanda_funerea", "Calyptorhynchus_funereus", breeding_point[,1])
breeding_point[,1] <- gsub("Zanda_latirostris", "Calyptorhynchus_latirostris", breeding_point[,1])
breeding_point[,1] <- gsub("Zapornia_", "Amaurornis_", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_fusca", "Porzana_fusca", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_parva", "Porzana_parva", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_paykullii", "Porzana_paykullii", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_pusilla", "Porzana_pusilla", breeding_point[,1])
breeding_point[,1] <- gsub("Amaurornis_tabuensis", "Porzana_tabuensis", breeding_point[,1])
breeding_point[,1] <- gsub("Zentrygon", "Geotrygon", breeding_point[,1])

S1 <- "Zimmerius_villarejoi"
S2 <- "Zimmerius_chicomendesi"   # Strongly suspected to be sister taxa
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zimmerius_vilissimus"
S2 <- "Zimmerius_parvus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zimmerius_improbus"
S2 <- "Zimmerius_petersi"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zoothera_dauma"
S2 <- "Zoothera_aurea"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zoothera_mollissima"
S2 <- "Zoothera_griseiceps"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zoothera_mollissima"
S2 <- "Zoothera_salimalii"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_minor"
S2 <- "Zosterops_chrysolaemus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_eurycricotus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_abyssinicus"
S2 <- "Zosterops_flavilateralis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_kaffensis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_kikuyuensis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_mbuluensis"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_silvanus"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_pallidus"
S2 <- "Zosterops_virens"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]

S1 <- "Zosterops_poliogastrus"
S2 <- "Zosterops_winifredae"  
breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] <-
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] +
  breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]] -
  breeding_point[which(breeding_point[,1]==S1), 2:dim(breeding_point)[2]] * breeding_point[which(breeding_point[,1]==S2), 2:dim(breeding_point)[2]]
breeding_point <- breeding_point[-which(breeding_point[,1]==S2), ]
#########################  #####################


breeding_point_continental <- breeding_point
write.csv(breeding_point_continental, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_continental_new.csv")
save(breeding_point_continental, file="/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_continental_new.Rdata")
