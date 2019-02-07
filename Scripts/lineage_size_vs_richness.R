# setwd('/Users/Jacob/Dropbox/Work/Diversity_accum')
setwd('~/Socolar/Data')

#devtools::install_github('jsocolar/BirdDiversification/phylosympatry')
library(phylosympatry)
library(data.table)
library(Matrix)
library(magrittr)

load("PTrees.Rdata")
load("breeding_data.Rdata")
'%ni%' <- Negate('%in%')

tree <- PTrees[[1]]
breeding_data <- breeding_data[-which(duplicated(breeding_data[,1])), ]
rownames(breeding_data) <- breeding_data[,1]
breeding_data <- breeding_data[, -1]

which(rownames(breeding_data) %ni% tree$tip.label)
which(tree$tip.label %ni% rownames(breeding_data))

trimmed_tree <- ape::drop.tip(tree, which(tree$tip.label %ni% rownames(breeding_data)))
lineages <- get_lineages(trimmed_tree, 20)

# Surely we can come up with a way to do the below faster.
# For each species, we are computing its range size (number of points occupied) and
# the average richness over those points
rangesizes <- rowSums(breeding_data)
richnesses <- colSums(breeding_data)

lineages <- as.data.table(lineages)
setkey(lineages,ID,species)
species_stats <- data.table('species'=rownames(breeding_data),
                            'rangesize'=rangesizes)

setkey(lineages,species)

D <- Matrix(as.matrix(breeding_data),sparse=T)


spp.map <- data.table('species'=D@Dimnames[[1]],
                      'i'=1:D@Dim[1])
col.map <- data.table('sample'=D@Dimnames[[2]],
                      'j'=1:D@Dim[2])
D <- as.data.table(summary(D)[,c('i','j')])
setkey(D,i)
setkey(spp.map,i)
D <- D[spp.map]

setkey(D,j)
setkey(col.map,j)
D <- D[col.map]

richness.map <- data.table('sample'=colnames(breeding_data),
                           'richness'=richnesses)
setkey(D,sample)
setkey(richness.map,sample)
D <- D[richness.map]

D <- D[,c('species','sample','richness')]

setkey(species_stats,species)
setkey(D,species)

species_stats <- species_stats[D,nomatch=0]

setkey(species_stats,species)


setkey(lineages,species)
S <- species_stats[lineages]

S[,richicality:=mean(richness),by=species]


### the data.table S has everything you need. If you want the average richness in a lineage:
setkey(S,ID)
S[,richicality.lineage:=mean(richness),by=ID]
