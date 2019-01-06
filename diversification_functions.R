# Take tree and depth as input and yield lineage assigments as output
get_lineages <- function(tree, depth){
  nsp <- length(tree$tip.label)
  cc <- ape::cophenetic.phylo(tree)
  
  rList <- list()
  for(i in 1:nsp){
    lineage_i <- rownames(cc)[cc[,i]<(2*depth)]
    rList[[i]] <- lineage_i[order(lineage_i)]
  }
  rList <- unique(rList)
  
  lineages <- reshape2::melt(rList)
  names(lineages) <- c("species", "ID")
  return(lineages)
}
  
# Function to assign each lineage as occurring predominantly in species-rich areas, species-poor areas, or neither
lineage_assign <- function(lineages, species_prop, c1, c2, c3, c4){
  # lineages is the output of get_lineages
  # species_prop is a list giving the proportion of the range that is in species-rich areas for each species, in the
  #     order of lineages$species
  # c1 is the fraction of the range in species-rich areas that qualifies a species as rich-associated. c2 is the fraction
  #     of rich-associated species that qualifies a lineage as rich-associated. c3 is the fraction of the range in species-
  #     poor areas that qualifies a species as poor-associated. c4 is the fraction of species-poor associated species 
  #     that qualifies a lineage as poor-associated.
  uL <- unique(lineages$ID)
  lineages$species_prop <- species_prop
  lineages$lineage_class <- NA
  for(i in uL){
    linProps <- species_prop[which(lineages$ID) == i]
    if(length(which(linProps >= c1))/length(linProps) >= c2){lineages$lineage_class[which(lineages$ID == i)] <- "R"}
    else if(length(which(linProps <= (1-c3)))/length(linProps) >= (1-c4)){lineages$lineage_class[which(lineages$ID == i)] <- "P"}
    else(lineages$lineage_class[which(lineages$ID == i)] <- "N")
  }
  return(lineages)
}


# Function to calculate rH, rC, and rT, and extract the contributions of tropical/temp/neither lineages
rstats <- function(lineages2, presence_matrix){
  # lineages2 is the output of lineage_assign()
  # presence_matrix is the presence_absence matrix of distributional data, where the first column is the species names
  r <- as.data.frame(matrix(data=NA, nrow = dim(presence_matrix)[2] - 1, ncol = 12))
  names(r) <- c("C", "H", "T", "CR", "HR", "TR", "CP", "HP", "TP", "CN", "HN", "TN")
  
  r$C <- colSums(presence_matrix[, 2:dim(presence_matrix)[2]])
  r$CR <- colSums(presence_matrix[which(presence_matrix[,1] %in% lineages2$species[which(lineages2$lineage_class == "R")]), 2:dim(presence_matrix)[2]])
  r$CP <- colSums(presence_matrix[which(presence_matrix[,1] %in% lineages2$species[which(lineages2$lineage_class == "P")]), 2:dim(presence_matrix)[2]])
  r$CN <- colSums(presence_matrix[which(presence_matrix[,1] %in% lineages2$species[which(lineages2$lineage_class == "N")]), 2:dim(presence_matrix)[2]])
  
  for(i in 2:dim(presence_matrix)[2]]){
    lH <- unique(lineages2$ID[which(lineages2$species %in% presence_matrix[which(presence_matrix[,i] == 1), 1])])
    lHR <- unique(lineages2$ID[which(lineages2$lineage_class == "R" & lineages2$species %in% presence_matrix[which(presence_matrix[,i] == 1), 1])])
    lHP <- unique(lineages2$ID[which(lineages2$lineage_class == "P" & lineages2$species %in% presence_matrix[which(presence_matrix[,i] == 1), 1])])
    lHN <- unique(lineages2$ID[which(lineages2$lineage_class == "N" & lineages2$species %in% presence_matrix[which(presence_matrix[,i] == 1), 1])])
    
    r$H[i-1] <- length(lH)
    r$HR[i-1] <- length(lHR)
    r$HP[i-1] <- length(lHP)
    r$HN[i-1] <- length(lHN)
    
    r$T[i-1] <- length(which(lineages2$ID %in% lH))
    r$TR[i-1] <- length(which(lineages2$ID %in% lHR))
    r$TP[i-1] <- length(which(lineages2$ID %in% lHP))
    r$TN[i-1] <- length(which(lineages2$ID %in% lHN))
  }
  
  return(r)
}