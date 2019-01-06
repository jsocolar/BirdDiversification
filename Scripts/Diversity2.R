## Diversification contribution analysis part 2: calculate diversification contributions

setwd("/Users/TingleyLab/Dropbox/Work/Diversity_accum")
library(ape)
"%ni%" <- Negate("%in%")

# breeding_point_continental, points, and PTrees are produced by Diversity1.R
load("/Users/TingleyLab/Dropbox/Work/Diversity_accum/breeding_point_continental_new.Rdata")
load("/Users/TingleyLab/Dropbox/Work/Diversity_accum/points.Rdata") 
#load("HTrees100.Rdata")
load("PTrees.Rdata")

points2 <- points[2:length(points)]
HTrees1 <- PTrees[[1]]

breeding_data <- breeding_point_continental[,-2]  # Dropping the first data column (this is a relic of earlier code, and 
# is not necessary, but is handled consistently in the current code.  The point dropped is near Punta Arenas, Chile.)
save(breeding_data, file="breeding_data.Rdata")
which(rowSums(breeding_data[,2:ncol(breeding_data)])==0)

###### Pruned phylogeny for each point, based on all species, just insectivores, and just non-insectivores IN CURRENT CODE, RUN ONLY ALL SPECIES#####

wSpecies <- breeding_data[,1] # All species in our geographic domain
nwSpecies <- HTrees1$tip.label[which(HTrees1$tip.label %ni% wSpecies)]  # Species outside domain (to drop)
which(wSpecies %ni% HTrees1$tip.label)
#traits <- read.delim("EltonTraits/BirdFuncDat.txt", header=T)   # traits data (includes diet)
#traits$sciName <- gsub(" ", "_", traits$Scientific)
#traits <- traits[1:9993,]
#Invertebrate <- traits$sciName[traits$Diet.5Cat=="Invertebrate"]   # list of insectivores
#Invert <- wSpecies[which(wSpecies %in% Invertebrate)]    # list of insectivores in the geographic points under analysis
#nInvert <- wSpecies[which(wSpecies %ni% Invertebrate)]    # list of non-insectivores in the geographic points under analysis

diet.assemblages.meta <- list()
xy.prop.meta.1 <- list()
xy.prop.meta.2 <- list()
cs <- colSums(breeding_data[,2:dim(breeding_data)[2]])

# The code below was originally written to analyze multiple diet groups separately (indexed by dG).
# As currently constituted, it analyzes only the "all" diet group.

cs2.meta <- list()
for(tree_i in 1:30){  # Repeat the following for 30 independent, equiprobable Hackett-backbone trees.
  wTree <- drop.tip(PTrees[[tree_i]], nwSpecies)  # drop species outside of geographic domain
  diet.assemblages.meta[[tree_i]] <- list()
  diet.assemblages.meta[[tree_i]]$all <- wSpecies
  xy.prop.meta.1[[tree_i]] <- list()
  xy.prop.meta.2[[tree_i]] <- list()
  cs2.meta[[tree_i]] <- list()
  for(dG in 1:1){ #length(diet.assemblages.meta[[tree_i]])){  # Analyze all 5 diet groups
    species <- vector(dim(breeding_data)[2]-1, mode="list")
    phylo <- vector(dim(breeding_data)[2]-1, mode="list")
    xy <- vector(dim(breeding_data)[2]-1, mode="list")
    xy.prop.meta.2[[tree_i]][[dG]] <- vector(dim(breeding_data)[2]-1, mode="list")
    bData <- breeding_data[which(breeding_data[,1] %in% diet.assemblages.meta[[tree_i]][[dG]]), ]
    cs2.meta[[tree_i]][[dG]] <- colSums(bData[,2:dim(bData)[2]])
    print("tree_i, dg")
    print(tree_i)
    print(dG)
    for(i in 2:dim(bData)[2]){
      species[[i-1]] <- bData[which(bData[,i]==1),1] # species list for the ith site
      nspecies <- as.character(breeding_data[which(breeding_data[,1] %ni% species[[i-1]]), 1])    # species not occurring at the ith site
      if(length(species[[i-1]])!=0){
        phylo[[i-1]] <- ape::drop.tip(wTree, nspecies)      # reduced phylogeny for the ith site
        xy[[i-1]] <- ape::ltt.plot.coords(phylo[[i-1]])     # coordinates of lineages through time plot for the ith site
        xy.prop.meta.2[[tree_i]][[dG]][[i-1]] <- xy[[i-1]]
        xy.prop.meta.2[[tree_i]][[dG]][[i-1]][,2] <- xy.prop.meta.2[[tree_i]][[dG]][[i-1]][,2]/cs2.meta[[tree_i]][[dG]][i-1]    # convert number of lineages to proportion of lineages
      }
      else{
        phylo[[i-1]] <- NA
        xy[[i-1]] <- NA
        xy.prop.meta.2[[tree_i]][[dG]][[i-1]] <- NA
      }
    }
  }
}

save(xy.prop.meta.2, file="xy_prop_meta_2_new.Rdata")
save(cs2.meta, file="cs2_meta_new.Rdata")