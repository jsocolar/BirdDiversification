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
lineage_assign <- function(lineages, species_prop=NA, presence_matrix=NULL, cutoff_1, cutoff_2, cutoff_3, cutoff_4, cutoff_5=.5){
  if(cutoff_5 == 1){
    stop("cutoff_5 must be strictly less than 1 if a quantile, or strictly greater than 1 if a richness")
  }
  if(cutoff_5 <= 0){
    stop("cutoff_5 must be strictly greater than zero")
  }
  if(is.na(species_prop)){
    '%ni%' <- Negate('%in%')
    if(sum(rownames(presence_matrix) %ni% lineages$species) > 0){
      stop("Some species in presence_matrix are not represented in lineages.")
    }
    if(sum(lineages$species %ni% rownames(presence_matrix)) > 0){
      stop("Some species in lineages are not represented in presence_matrix. Either add all-zero rows for missing species, or prune the phylogeny to exclude these species.")
    }
  }
  
  if(!is.na(species_prop) & !is.null(presence_matrix)){
    warning("species_prop and presence_matrix are both set.  Using species_prop.")
  }
  
  if(is.na(species_prop)){
    species_order <- match(lineages$species, rownames(presence_matrix))
    species_order <- species_order[!is.na(species_order)]
    presence_matrix <- presence_matrix[species_order, ]
    richnesses <- colSums(presence_matrix)
    if(cutoff_5 > 1){
      richness_cutoff <- cutoff_5
    }else{
      richness_cutoff <- quantile(richnesses, cutoff_5)
    }
    species_prop <- rowSums(presence_matrix[, richnesses > richness_cutoff])/rowSums(presence_matrix)
  }

  lineage_IDs <- unique(lineages$ID)
  lineages$species_prop <- species_prop
  lineages$lineage_class <- "N"
  for(id in lineage_IDs){
    linProps <- species_prop[which(lineages$ID == id)]
    if(sum(is.nan(linProps)) > 0){
      warning(paste0('No richness proportion could be calculated for the following species:\n', paste(names(linProps)[is.nan(linProps)], collapse = ' ')))
      linProps <- linProps[!is.nan(linProps)]
    }
    if(length(linProps) > 0){
      if(sum(linProps >= cutoff_1)/length(linProps) >= cutoff_2){lineages$lineage_class[which(lineages$ID == id)] <- "R"}
      else if(sum(linProps <= (1-cutoff_3))/length(linProps) >= cutoff_4){lineages$lineage_class[which(lineages$ID == id)] <- "P"}
    }else{
      lineages$lineage_class[which(lineages$ID == id)] <- "NA"
    }
    
  }
  attr(lineages, 'cutoffs') <- list("cutoff_1"=cutoff_1, "cutoff_2"=cutoff_2, "cutoff_3"=cutoff_3, "cutoff_4"=cutoff_4,
                                    "cutoff_5"=cutoff_5)
  return(lineages)
}
