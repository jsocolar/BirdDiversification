### This script will analyze the probability of presence across lineages

library(phylosympatry)
library(magrittr)
library(ggplot2)


# Functions ---------------------------------------------------------------

lineage_collapse <- function(lineages,presence_matrix,collapse.fcn=max){
  IDs <- unique(lineages$ID)
  C <- matrix(NA,nrow=length(IDs),ncol=ncol(presence_matrix))
  rownames(C) <- IDs
  for (id in IDs){
    C[id,] <- apply(presence_matrix[lineages$species[lineages$ID==id],,drop=F],
                    2,collapse.fcn)
  }
  return(C)
}

lineage_regressions <- function(lineages,presence_matrix,x='richness',collapse.fcn=max,...){
  if (x=='richness'){
    x <- apply(presence_matrix,2,sum)
  } else{
    if (length(x)!=ncol(presence_matrix)){
      stop('length of meta-data, x, must be equal to ncol of presence_matrix')
    }
  }

  C <- lineage_collapse(lineages,presence_matrix,collapse.fcn)

  GLMs <- apply(C,1,FUN=function(y,x) glm(y~x,...),x=x)
  names(GLMs) <- rownames(C)
  rm('C')
  gc()
  return(GLMs)
}


# Analysis ------------------------------------------------------------------------




set.seed(1)
m <- 100
n <- 30
tree <- rtree(m)
presence_matrix <- matrix(rbinom(m*n,1,.1),nrow=m)
rownames(presence_matrix) <- tree$tip.label
lineages <- get_lineages(tree,1)

props <- runif(nrow(lineages))
assigned_lineages <- lineage_assign(lineages,props,.7,.3,.7,.3)

table(assigned_lineages$lineage_class)
# N   P   R
# 52  66 162


GLMs <- lineage_regressions(lineages,presence_matrix)

tstats <- lapply(GLMs,summary) %>% lapply(getElement,'coefficients') %>%
  sapply(FUN=function(x) x['x','t value'])

richness <- apply(presence_matrix,2,sum)
C <- lineage_collapse(lineages,presence_matrix)


tstats <- sort(tstats,decreasing=T)
GLMs <- GLMs[names(tstats)]
C <- C[names(tstats),]

classes <- c(by(assigned_lineages$lineage_class,assigned_lineages$ID,FUN=function(x) unique(as.character(x))))
D <- data.frame('class'=classes,
                'ID'=as.numeric(names(classes)),
                stringsAsFactors = F)

D$tstat <- tstats[as.character(D$ID)]


ggplot(D,aes(class,tstat,color=class))+
  geom_boxplot()+
  geom_jitter(pch=16,cex=2)

glm(tstat~class,data=D) %>% summary
