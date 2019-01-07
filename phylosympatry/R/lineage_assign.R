#' Assign Lineages based on proportional membership to categories
#'
#' @param lineages output from \code{\link{get_lineages}}
#' @param species_prop vector of the proportions of the range that is in species-rich areas for each species, in the order of lineages$species
#' @param cutoff_1 fraction of the range in species-rich areas that qualifies a species as rich-associated.
#' @param cutoff_2 fraction of rich-associated species that qualifies a lineage as rich-associated
#' @param cutoff_3 fraction of the range in species-poor areas that qualifies a species as poor-associated
#' @param cutoff_4 fraction of species-poor associated species that qualifies a lineage as poor-associated.
#' @param cutoff_5 richness value (if >1) or quantile (if <1) at which to call a site species-rich vs. species poor, for use only with presence_matrix (and thus species_prop=NA)
#' @export
#' @examples
#' library(phylosympatry)
#' tree <- ape::rtree(100)
#' lineages <- get_lineages(tree,1)
#' species_prop <- runif(nrow(lineages))
#' lineage_assign(lineages,species_prop,.7,.1,.3,.2)
#'
lineage_assign <- function(lineages, species_prop=NA, presence_matrix=NA, cutoff_1, cutoff_2, cutoff_3, cutoff_4, cutoff_5=.5){
  if(!is.na(species_prop) & !is.na(presence_matrix)){
    warning("species_prop and presence_matrix are both set.  Using species_prop.")
  }
  if(cutoff_5 == 1){
    stop("cutoff_5 must be strictly less than 1 if a quantile, or strictly greater than 1 if a richness")
  }
  if(cutoff_5 <= 0){
    stop("cutoff_5 must be strictly greater than zero")
  }

  if(is.na(species_prop)){
    presence_matrix <- presence_matrix[match(lineages$species, rownames(presence_matrix)), ]
    richnesses <- colSums(presence_matrix)
    if(cutoff_5 > 1){
      richness_cutoff <- cutoff_5
    }else{
      richness_cutoff <- quantile(richnesses, cutoff_5)
    }
    species_prop <- rep(NA, nrow(lineages))
    for(i in 1:length(species_prop)){
      species_prop[i] <- sum(presence_matrix[i, richnesses > richness_cutoff])/sum(presence_matrix[i])
    }
  }

  lineage_IDs <- unique(lineages$ID)
  lineages$species_prop <- species_prop
  lineages$lineage_class <- NA
  for(id in lineage_IDs){
    linProps <- species_prop[which(lineages$ID == id)]
    if(length(which(linProps >= cutoff_1))/length(linProps) >= cutoff_2){lineages$lineage_class[which(lineages$ID == id)] <- "R"}
    else if(length(which(linProps <= (1-cutoff_3)))/length(linProps) >= cutoff_4){lineages$lineage_class[which(lineages$ID == id)] <- "P"}
    else(lineages$lineage_class[which(lineages$ID == id)] <- "N")
  }
  attr(lineages, 'cutoffs') <- list("cutoff_1"=cutoff_1, "cutoff_2"=cutoff_2, "cutoff_3"=cutoff_3, "cutoff_4"=cutoff_4,
                                    "cutoff_5"=cutoff_5)
  return(lineages)
}
