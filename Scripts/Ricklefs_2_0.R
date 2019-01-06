## Diversification analysis
setwd("/Users/JacobSocolar/Dropbox/Work/Diversity_accum")
library(ape)
library(ggplot2)
"%ni%" <- Negate("%in%")

##### 1: For each continental species, what fraction of the range is tropical? #####
load("/Users/JacobSocolar/Dropbox/Work/Diversity_accum/breeding_point_continental_new.Rdata")
load("/Users/JacobSocolar/Dropbox/Work/Diversity_accum/points.Rdata") 
load("PTrees.Rdata")
# These objects come from Diversity1_new.R

points2 <- points[2:length(points)]
HTrees1 <- PTrees[[1]]


breeding_data <- breeding_point_continental[,-2]  # Dropping the first data column (this is a relic of earlier code, and 
# is not necessary, but is handled consistently in the current code.  The point dropped is near Punta Arenas, Chile.)
which(rowSums(breeding_data[,2:ncol(breeding_data)])==0)

# Splitting lineages based on whether they are tropical or temperate
trop_frac <- data.frame(species = breeding_data[,1], trop.frac = NA)
trop_points <- which(points2@coords[,2] < 23.3 & points2@coords[,2] > -23.3)
for(i in 1:nrow(trop_frac)){
  print(i)
  bdi <- breeding_data[i, 2:34185]
  trop_frac$trop.frac[i] <- sum(bdi[trop_points])/sum(bdi)
}
save(trop_frac, file="trop_frac.Rdata")

##### 2: For a given depth in the phylogeny, what are the clades, and what are the tropical fractions of the members? #####
# Store information as a list of vectors. Each vector corresponds to a clade, its length is the clade richness,
# and its entries are the tropical fractions of the species.
wSpecies <- breeding_data[,1] # All species in our geographic domain
nwSpecies <- HTrees1$tip.label[which(HTrees1$tip.label %ni% wSpecies)]  # Species outside domain (to drop)
wTree <- drop.tip(PTrees[[1]], nwSpecies)  # drop species outside of geographic domain

mow.depth <- 40
cpm <- cophenetic(wTree)
clade_frac <- list()
i <- 0
while(nrow(cpm) > 0){
  i <- i+1
  print(i)
  clade.members <- which(cpm[1,] < mow.depth)
  clade_frac[[i]] <- trop_frac$trop.frac[which(trop_frac$species %in% rownames(cpm)[clade.members])]
  cpm <- cpm[-clade.members, -clade.members]
}

save(clade_frac, file="clade_frac.Rdata")

range_prop <- .75
rpt <- 1 - range_prop
species_prop <- .75

troplr <- vector()
templr <- vector()
cs <- rep(NA, length(clade_frac))
for(i in 1:length(clade_frac)){
  cs[i] <- length(clade_frac[[i]])
  if(length(which(clade_frac[[i]] > range_prop))/cs[i] > species_prop){
    troplr <- c(troplr, cs[i])
  }else if(length(which(clade_frac[[i]] < rpt))/cs[i] > species_prop){
    templr <- c(templr, cs[i])
  }
}

mean(templr)
mean(troplr)
median(templr)
median(troplr)
wilcox.test(troplr, templr)

trop <- data.frame(x = log(troplr), group = "trop")
temp <- data.frame(x = log(templr), group = "temp")
combined <- rbind(trop, temp)
ggplot(combined, aes(x, fill=group, colour=group)) +
  geom_density(alpha=0.4, lwd=0.8) 

trop2 <- data.frame(x = log(troplr[which(troplr > 1)]), group = "trop")
temp2 <- data.frame(x = log(templr[which(templr > 1)]), group = "temp")
combined2 <- rbind(trop2, temp2)
ggplot(combined2, aes(x, fill=group, colour=group)) +
  geom_density(alpha=0.4, lwd=0.8) 
wilcox.test(troplr[which(troplr > 1)], templr[which(templr > 1)])


# Splitting lineages based on whether they are in species-rich or species-poor locations
richness <- colSums(breeding_data[, 2:34185])

rich_frac <- data.frame(species = breeding_data[,1], rich.frac = NA)
rich_points <- which(richness > median(richness))
for(i in 1:nrow(rich_frac)){
  print(i)
  bdi <- breeding_data[i, 2:34185]
  rich_frac$rich.frac[i] <- sum(bdi[rich_points])/sum(bdi)
}
save(rich_frac, file="rich_frac.Rdata")

##### 2: For a given depth in the phylogeny, what are the clades, and what are the tropical fractions of the members? #####
# Store information as a list of vectors. Each vector corresponds to a clade, its length is the clade richness,
# and its entries are the tropical fractions of the species.
wSpecies <- breeding_data[,1] # All species in our geographic domain
nwSpecies <- HTrees1$tip.label[which(HTrees1$tip.label %ni% wSpecies)]  # Species outside domain (to drop)
wTree <- drop.tip(PTrees[[1]], nwSpecies)  # drop species outside of geographic domain

mow.depth <- 40
cpm <- cophenetic(wTree)
clade_frac <- list()
i <- 0
while(nrow(cpm) > 0){
  i <- i+1
  print(i)
  clade.members <- which(cpm[1,] < mow.depth)
  clade_frac[[i]] <- rich_frac$rich.frac[which(rich_frac$species %in% rownames(cpm)[clade.members])]
  cpm <- cpm[-clade.members, -clade.members]
}
clade_rich_frac <- clade_frac
save(clade_rich_frac, file="clade_rich_frac.Rdata")

range_prop <- .5
rpt <- 1 - range_prop
species_prop <- .6

richlr <- vector()
poorlr <- vector()
cs <- rep(NA, length(clade_rich_frac))
for(i in 1:length(clade_rich_frac)){
  cs[i] <- length(clade_rich_frac[[i]])
  if(length(which(clade_rich_frac[[i]] > range_prop))/cs[i] > species_prop){
    richlr <- c(richlr, cs[i])
  }else if(length(which(clade_rich_frac[[i]] < rpt))/cs[i] > species_prop){
    poorlr <- c(poorlr, cs[i])
  }
}

mean(poorlr)
mean(richlr)
median(poorlr)
median(richlr)
wilcox.test(richlr, poorlr)

rich <- data.frame(x = log(richlr), group = "rich")
poor <- data.frame(x = log(poorlr), group = "poor")
combined <- rbind(rich, poor)
ggplot(combined, aes(x, fill=group, colour=group)) +
  geom_density(alpha=0.4, lwd=0.8) 

rich2 <- data.frame(x = log(richlr[which(richlr > 1)]), group = "rich")
poor2 <- data.frame(x = log(poorlr[which(poorlr > 1)]), group = "poor")
combined2 <- rbind(rich2, poor2)
ggplot(combined2, aes(x, fill=group, colour=group)) +
  geom_density(alpha=0.4, lwd=0.8) 
wilcox.test(richlr[which(richlr > 1)], poorlr[which(poorlr > 1)])



