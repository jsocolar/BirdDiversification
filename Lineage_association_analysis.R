### This script will analyze the probability of presence across lineages

library(phylosympatry)
library(magrittr)
library(ggplot2)
library(ggpubr)


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
assigned_lineages <- lineage_assign(lineages,species_prop=props,
                                    cutoff_1=.7,cutoff_2=.1,cutoff_3=.7,cutoff_4=.3)

table(assigned_lineages$lineage_class)
# N   P   R
# 52  66 162


GLMs <- lineage_regressions(lineages,presence_matrix,family=binomial)

zstats <- lapply(GLMs,summary) %>% lapply(getElement,'coefficients') %>%
  sapply(FUN=function(x) x['x','z value'])

richness <- apply(presence_matrix,2,sum)
C <- lineage_collapse(lineages,presence_matrix)


zstats <- sort(zstats,decreasing=T)
coefs <- sapply(GLMs,coef)['x',]
GLMs <- GLMs[names(zstats)]
C <- C[names(zstats),]

classes <- c(by(assigned_lineages$lineage_class,assigned_lineages$ID,FUN=function(x) unique(as.character(x))))
N <- c(by(assigned_lineages$lineage_class,assigned_lineages$ID,length))

D <- data.frame('class'=classes,
                'ID'=as.numeric(names(classes)),
                'N'=N,
                stringsAsFactors = F)

D$z <- zstats[as.character(D$ID)]
D$coefs <- coefs[as.character(D$ID)]


g1 <- ggplot(D,aes(class,z,color=class))+
  geom_boxplot()+
  geom_jitter(pch=16,cex=2)

g2 <- ggplot(D,aes(N,z,color=class,fill=class))+
  geom_point()+
  geom_smooth()

ggarrange(g1,g2,nrow=2)

glm(z~class,data=D) %>% summary
