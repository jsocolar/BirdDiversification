#' Richness statistics for multiple communities
#'
#' @param assigned_lineages output from \code{\link{lineage_assign}}
#' @param presence_matrix presence_absence matrix of distributional data, species names in the rownames
#' @export
#' @examples
#' library(phylosympatry)
#' set.seed(1)
#' m=100
#' tree <- ape::rtree(m)
#' lineages <- get_lineages(tree,1)
#' assigned_lineages <- lineage_assign(lineages,runif(nrow(lineages)),.7,.2,.1,.3)
#'
#' n=5
#' presence_matrix <- matrix(rbinom(m*n,1,.1),nrow=m)
#' rownames(presence_matrix) <- tree$tip.label
#'
#' Richness_stats(assigned_lineages,presence_matrix)
Richness_stats <- function(assigned_lineages, presence_matrix){


  if (is.null(rownames(presence_matrix)) | !all(rownames(presence_matrix) %in% assigned_lineages$species)){
    stop('No rownames for presence_matrix or some rownames not found in assigned_lineages$species. \n All rownames of presence_matrix must be contained in assigned_lineages$species')
  }
  n <- ncol(presence_matrix)
  r <- as.data.frame(matrix(data=NA, nrow = n, ncol = 12))
  names(r) <- c("C", "H", "A", "CR", "HR", "AR", "CP", "HP", "AP", "CN", "HN", "AN")
  spp <- rownames(presence_matrix)
  IDs <- assigned_lineages$ID
  assigned_spp <- assigned_lineages$species
  lin_class <- assigned_lineages$lineage_class



  r$C <- colSums(presence_matrix)
  r$CR <- colSums(presence_matrix[spp %in% assigned_spp[lin_class == "R"],])
  r$CP <- colSums(presence_matrix[spp %in% assigned_spp[lin_class == "P"],])
  r$CN <- colSums(presence_matrix[spp %in% assigned_spp[lin_class == "N"],])

  for(i in 1:n){
    lH <- unique(IDs[assigned_spp %in% spp[presence_matrix[,i] == 1]])
    lHR <- unique(IDs[lin_class == "R" & assigned_spp %in% spp[presence_matrix[,i] == 1]])
    lHP <- unique(IDs[lin_class == "P" & assigned_spp %in% spp[presence_matrix[,i] == 1]])
    lHN <- unique(IDs[lin_class == "N" & assigned_spp %in% spp[presence_matrix[,i] == 1]])

    r$H[i] <- length(lH)
    r$HR[i] <- length(lHR)
    r$HP[i] <- length(lHP)
    r$HN[i] <- length(lHN)

    r$A[i] <- sum(IDs %in% lH)
    r$AR[i] <- sum(IDs %in% lHR)
    r$AP[i] <- sum(IDs %in% lHP)
    r$AN[i] <- sum(IDs %in% lHN)
  }

  return(r)
}
