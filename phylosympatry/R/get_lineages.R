#' Get lineages
#'
#' @param tree phylo class object
#' @param depth depth, in same units as branch length, at which lineages will be obtained
#' @return data frame of with columns 'species' containing all species in the tree and "ID" indicating the lineage ID a species is in.
#' @export
#' @examples
#' library(phylosympatry)
#' tree <- ape::rtree(100)
#' get_lineages(tree,.3)
#' get_lineages(tree,1)
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
  attr(lineages,'depth') <- depth
  return(lineages)
}
