#' Assign Lineages based on proportional membership to categories
#'
#' @param lineages output from \code{\link{get_lineages}}
#' @param species_prop vector of the proportions of the range that is in species-rich areas for each species, in the order of lineages$species
#' @param cutoff_1 fraction of the range in species-rich areas that qualifies a species as rich-associated.
#' @param cutoff_2 fraction of rich-associated species that qualifies a lineage as rich-associated
#' @param cutoff_3 fraction of the range in species-poor areas that qualifies a species as poor-associated
#' @param cutoff_4 fraction of species-poor associated species that qualifies a lineage as poor-associated.
#' @examples
#' library(phylosympatry)
#' tree <- rtree(100)
#' lineages <- get_lineages(tree,1)
#' species_prop <- runif(nrow(lineages))
#' lineage_assign(lineages,species_prop,.7,.1,.3,.2)
#'
lineage_assign <- function(lineages, species_prop, cutoff_1, cutoff_2, cutoff_3, cutoff_4){
  lineage_IDs <- unique(lineages$ID)
  lineages$species_prop <- species_prop
  lineages$lineage_class <- NA
  for(id in lineage_IDs){
    linProps <- species_prop[which(lineages$ID == id)]
    if(length(which(linProps >= cutoff_1))/length(linProps) >= cutoff_2){lineages$lineage_class[which(lineages$ID == id)] <- "R"}
    else if(length(which(linProps <= (1-cutoff_3)))/length(linProps) >= cutoff_4){lineages$lineage_class[which(lineages$ID == id)] <- "P"}
    else(lineages$lineage_class[which(lineages$ID == id)] <- "N")
  }
  return(lineages)
}
