library(phylosympatry)
library(phylofactor)
library(Matrix)
library(ggplot2)
library(ggpubr)
library(xlsx)


# Workspace Prep ----------------------------------------------------------

setwd('~/Socolar/')
load("Data/PTrees.Rdata")
load("Data/breeding_data.Rdata")
'%ni%' <- Negate('%in%')


tree <- PTrees[[1]]
breeding_data <- breeding_data[-which(duplicated(breeding_data[,1])), ]
rownames(breeding_data) <- breeding_data[,1]
breeding_data <- breeding_data[, -1]
lineages <- get_lineages(tree,20)

missing_species <- as.data.frame(matrix(data = 0, nrow=sum(lineages$species %ni% rownames(breeding_data)), ncol=ncol(breeding_data)))
rownames(missing_species) <- lineages$species[which(lineages$species %ni% rownames(breeding_data))]
colnames(missing_species) <- colnames(breeding_data)
breeding_data <- rbind(breeding_data, missing_species)

nms <- rownames(breeding_data)
m <- nrow(breeding_data)
n <- ncol(breeding_data)

set.seed(1)
sample.size <- 3e3
samples <- sample(n,sample.size,replace=F)

Mat <- as.matrix(breeding_data[,samples])
Mat <- Mat[rowSums(Mat)>0,]
Mat <- Matrix(Mat,sparse=T)


rm('breeding_data')
gc()

saveRDS(Mat,file = 'Data/breeding_data_subsampled_and_trimmed')





# phylofactorization ------------------------------------------------------
rm(list=ls())
gc()

Mat <- readRDS('Data/breeding_data_subsampled_and_trimmed')
load('Data/PTrees.Rdata')
tree <- PTrees[[1]]


R <- data.table('Sample'=colnames(Mat),'richness'=colSums(Mat))
setkey(R,Sample)
frmla.phylo <- cbind(Successes,Failures)~phylo*richness
tree <- drop.tip(tree,setdiff(tree$tip.label,rownames(Mat)))
Mat <- Mat[tree$tip.label,]
Data <- list('Successes'=Mat,
             'Failures'=1-Mat)

### Demo of data
demo.data <- mAggregation(Data,list(1,2:Ntip(tree)),tree,R,'binomial',frmla.phylo)
## Successes is the sum of presences. Lineage present in site if Successes > 0
demo.data[,Successes:=as.numeric(Successes>0)]
demo.data[,Failures:=1-Successes]
rm('demo.data')



myglm <- function(formula,data,...){
  data[,Successes:=as.numeric(Successes>0)]
  data[,Failures:=1-Successes]
  return(tryCatch(glm(formula,data=data,...)))
}



pf <- gpf(Data,tree,frmla.phylo,algorithm = 'mStable',MetaData = R,
          PartitioningVariables = 'richness',family=binomial,
          nfactors=100,ncores=7,model.fcn = myglm,min.group.size = 2,
          cluster.depends = 'library(Matrix)')

#7.12 minutes per factor for 1,000 samples with min.group.size=1 (default - include tips)
#3.54 minutes per factor for 1,000 samples with min.group.size=2
## With sparse matrix, R maxes out at ~15GB RAM for 7-core phylofactorization

save(pf,file='Data/phylofactor_richness_3000samples')






# Analysis of phylofactor object ------------------------------------------

load('Data/phylofactor_richness_3000samples')
pf$models[[1]]
##The parameter phyloS:richness was used for partitioning.
##When it is positive, the species are more often found in rare / low-richness environments

rplot <- function(n=1,pf.=pf){
  dd <- mAggregation(pf$Data,pf$groups[[n]],pf$tree,pf$MetaData,'binomial',cbind(Successes,Failures)~Richness)

  dd[,Present:=as.numeric(Successes>0)]
  dd[,fit:=pf$models[[n]]$family$linkinv(pf$models[[n]]$fitted.values)]
  setkey(dd,phylo,richness)
  gg <- ggplot(dd[phylo=='R'],aes(richness,Present))+
    geom_point(alpha=0.3,cex=2)+
    geom_line(aes(richness,fit))+
    ggtitle(as.character(pf$factors$Group1[n]))
  return(gg)
}



sum(sapply(pf$models,FUN=function(gg) coef(gg)['phyloS:richness'])>0)
##all 50 factors are positive
##the most rich sites have an average D of ~2.
## Thus, adding any new lineage with >2 species will increase the average D in species-poor sites
group.sizes <- sapply(pf$groups,FUN=function(gg) length(gg[[1]]))
table(group.sizes)
#  2  3  4  5  6  7  8 12 14 17 19
# 28  7  3  2  2  2  1  1  1  2  1
## 22 lineages have group size >2


big.groups <- which(group.sizes>3)
ggs <- lapply(big.groups,rplot)

ggarrange(plotlist = ggs,ncol=5,nrow=3)


species.list <- phylofactor::pf.groupsTospecies(pf)


spp <- species.list[big.groups[order(group.sizes[big.groups],decreasing=T)]] %>%
  lapply('[[',1)
names(spp) <- pf$factors$Group1[big.groups[order(group.sizes[big.groups],decreasing=T)]]
spp
for (i in 1:length(spp)){
  write.xlsx(x=spp[i],file='Bird_lineages_biased_towards_low_richness.xlsx',
             sheetName = paste(i,'_',names(spp)[i],sep=''),append=(i>1))
}


pp=pf.tree(pf,factors=big.groups,top.layer = T,top.alpha = 1)
pp
ggsave(filename='BirdDiversification/Figures/Large_clades_with_low_richness_bias.png',height=20,width=20,units='in')

pvals <- lapply(pf$models,aov) %>% lapply(summary) %>%
  sapply(FUN=function(x) x[[1]]['phylo:richness','Pr(>F)'])


plot(1:100,c(pvals,rep(NA,50)),log='y',ylim=c(1e-89,1e-4),
     main='Pvalue decay',xlab='Factor',ylab='Pr(>F)')
lines(1:100,.01/seq(7441-2*(0:99)))
