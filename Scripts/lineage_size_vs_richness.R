setwd('/Users/Jacob/Dropbox/Work/Diversity_accum')

#devtools::install_github('jsocolar/BirdDiversification/phylosympatry')
library(phylosympatry)

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
lineages$richicality <- NA
lineages$rangesize <- NA
rangesizes <- rowSums(breeding_data)
richnesses <- colSums(breeding_data)
for(i in 1:nrow(lineages)){
  print(i)
  lineages$rangesize[i] <- rangesizes[which(row.names(breeding_data) == lineages$species[i])]
  lineages$richicality[i] <- mean(richnesses[which(breeding_data[which(row.names(breeding_data) == lineages$species[i]), ] == 1)])
}

# Now we build a dataframe where rows are lineages and columns give the lineage size,
# richicality, total range size, and average (species-specific) range size

