#' Compute whether lineages that characteristically occur at species-rich sites have different average richness that lineages that characteristically occur at species-poor sites
#'
#' @param assigned_lineages output from \code{\link{lineage_assign}}
#' @export
#' @examples
#' library(phylosympatry)
#' tree <- ape::rtree(100)
#' lineages <- get_lineages(tree,1)
#' species_prop <- runif(nrow(lineages))
#' assigned_lineages <- lineage_assign(lineages,species_prop,.7,.1,.3,.2)
#' ricklefs_analysis(assigned_lineages)

ricklefs_analysis <- function(assigned_lineages){
  nlin <- max(assigned_lineages$ID)
  df <- as.data.frame(matrix(data=NA, nrow=nlin, ncol=2))
  names(df) <- c("richness", "class")
  for(i in 1:nlin){
    df$richness[i] <- sum(assigned_lineages$ID == as.character(i)) 
    df$class[i] <- unique(assigned_lineages$lineage_class[assigned_lineages$ID == as.character(i)])
  }
  n <- table(df$class)
  means <- list(R = mean(df$richness[df$class=="R"]), P = mean(df$richness[df$class=="P"]))
  medians <- list(R = median(df$richness[df$class=="R"]), P = median(df$richness[df$class=="P"]))
  df2 <- df[df$class %in% c("R", "P"), ]
  wilcox <- wilcox.test(richness ~ class, data = df2)
  
  output <- list(df=df, n=n, means=means, medians=medians, wilcox=wilcox)
  return(output)
}
