#' Collapse lineages in presence-matrix
#'
#' @export
#' @param lineages lineages object, see \code{\link{get_lineages}}. Must have columns \code{species} and \code{ID}, where all species are present in the rownames of the presence matrix
#' @param presence_matrix Matrix whose rownames contain all the species in \code{lineages}
#' @param collapse.fcn Aggregatin function for collapsing lineages. Takes as input vectors of data (may have length 1) from presence_matrix and outputs numerical value. Default is \code{\link{max}}
#' @return matrix of numeric values output from \code{collapse.fcn} with number of rows equal to \code{length(unique(lineages$ID))}, and columns equal to \code{ncol(presence_matrix)}
#' @examples
#' library(phylosympatry)
#' set.seed(1)
#' m=20 #number of species
#' n=10  #number of sites
#' tree <- ape::rtree(m)
#' lineages <- get_lineages(tree,1)
#' presence_matrix <- matrix(rbinom(m*n,1,.1),nrow=m,ncol=n)
#' rownames(presence_matrix) <- tree$tip.label
#' colnames(presence_matrix) <- letters[1:n]
#'
#' Mat <- lineage_collapse(lineages,presence_matrix)
lineage_collapse <- function(lineages,presence_matrix,collapse.fcn=max){
  if (!(all(as.character(lineages$species) %in% rownames(presence_matrix)))){
    stop('not all lineages$species are found in rownames(presence_matrix).')
  }
  cc <- function(id,presence_matrix,collapse.fcn){
    return(apply(presence_matrix[lineages$species[lineages$ID==id],,drop=F],2,collapse.fcn))
  }
  IDs <- unique(lineages$ID)
  C <- t(sapply(IDs,cc,presence_matrix,collapse.fcn))
  rownames(C) <- IDs
  colnames(C) <- colnames(presence_matrix)
  return(C)
}
