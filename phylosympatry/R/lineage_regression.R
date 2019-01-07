#' Regression of covariate, x, on lineage data in presence matrix, aggregated via collapse.fcn
#'
#' @export
#' @param lineages lineages object output from \code{\link{get_lineages}}, with columns \code{species} and \code{ID}
#' @param presence_matrix Matrix whose rownames contain all the species in \code{lineages}
#' @param x covariate of length equal to \code{ncol(presence_matrix)}. Default is the richness, or \code{colSums(presence_matrix)}
#' @param collapse.fcn Aggregatin function for collapsing lineages. Takes as input vectors of data (may have length 1) from presence_matrix and outputs numerical value. Default is \code{\link{max}}
#' @param ncores optional input number of cores for parallelized computation of many glm objects. May be memory-intensive with large \code{presence_matrix}
#' @return named list, with each element being a generalized linear model \code{glm(y~x,...)} where \code{y} is the collapsed data for a given lineage
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
#' GLMs <- lineage_regression(lineages,presence_matrix)
lineage_regression <- function(lineages,presence_matrix,x='richness',collapse.fcn=max,ncores=NULL,...){
  if (x=='richness'){
    x <- apply(presence_matrix,2,sum)
  } else{
    if (length(x)!=ncol(presence_matrix)){
      stop('length of meta-data, x, must be equal to ncol of presence_matrix')
    }
  }
  if (class(x) %in% c('character','factor')){
    if (length(unique(x))==1){
      stop('x has only one unique element')
    }
  } else {
    if (var(x)==0){
      stop('x has variance 0')
    }
  }

  C <- lineage_collapse(lineages,presence_matrix,collapse.fcn)

  if (is.null(ncores)){
    GLMs <- apply(C,1,FUN=function(y,x) tryCatch(glm(y~x,...),error=function(e) e),x=x)
  } else {
    cl <- parallel::makeCluster(ncores)
    GLMs <- parallel::parApply(cl,C,1,FUN=function(y,x) tryCatch(glm(y~x,...),error=function(e) e),x=x)
    parallel::stopCluster(cl)
    rm('cl')
  }
  names(GLMs) <- rownames(C)
  rm('C')
  gc()
  return(GLMs)
}
