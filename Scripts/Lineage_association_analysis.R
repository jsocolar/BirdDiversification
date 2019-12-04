### This script will analyze the probability of presence across lineages
# setwd('/Users/JacobSocolar/Dropbox/Work/Diversity_accum')

#devtools::install_github('jsocolar/BirdDiversification/phylosympatry')

library(phylosympatry)
library(magrittr)
library(ggplot2)
library(ggpubr)

load("PTrees.Rdata")
load("breeding_data.Rdata")
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

assigned_lineages <- lineage_assign(lineages,presence_matrix=breeding_data,
                                    cutoff_1=.7,cutoff_2=.7,cutoff_3=.7,cutoff_4=.7, cutoff_5 = .85)

table(assigned_lineages$lineage_class)

GLMs <- lineage_regression(lineages,breeding_data,family=binomial)

zstats <- lapply(GLMs,summary) %>% lapply(getElement,'coefficients') %>%
  sapply(FUN=function(x) x['x','z value'])

richness <- colSums(breeding_data)

C <- lineage_collapse(lineages,breeding_data)


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
  geom_smooth()+
  scale_x_log10()

ggarrange(g1,g2,nrow=2)

glm(z~class,data=D) %>% summary

rst <- Richness_stats(assigned_lineages, breeding_data)

scatter.smooth(log(rst$A/rst$H) ~ rst$C,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness",
               ylab= "Divers. contrib.", main = "All lineages")

scatter.smooth(log(rst$AR/rst$HR) ~ rst$C,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness",
               ylab= "Divers. contrib.", main = "Rich-areas lineages")

scatter.smooth(log(rst$AP/rst$HP) ~ rst$C,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness",
               ylab= "Divers. contrib.", main = "Poor-areas lineages")

scatter.smooth(log(rst$AN/rst$HN) ~ rst$C,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness",
               ylab= "Divers. contrib.", main = "Neither lineages")








scatter.smooth((rst$A/rst$H - 1) ~ rst$CR,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of rich-areas lineages",
               ylab= "Divers. contrib.", main = "All lineages")

scatter.smooth((rst$AR/rst$HR - 1) ~ rst$CR,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of rich-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Rich-areas lineages")

scatter.smooth((rst$AP/rst$HP - 1) ~ rst$CR,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of rich-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Poor-areas lineages")

scatter.smooth((rst$AN/rst$HN - 1) ~ rst$CR,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of rich-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Neither lineages")






scatter.smooth((rst$A/rst$H - 1) ~ rst$CP,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of poor-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "All lineages")

scatter.smooth((rst$AR/rst$HR - 1) ~ rst$CP,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of poor-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Rich-areas lineages")

scatter.smooth((rst$AP/rst$HP - 1) ~ rst$CP,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of poor-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Poor-areas lineages")

scatter.smooth((rst$AN/rst$HN - 1) ~ rst$CP,
               pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness of poor-areas lineages",
               ylab= "Divers. contrib.", xaxt='n', yaxt='n', main = "Neither lineages")




ricklefs <- ricklefs_analysis(assigned_lineages)
