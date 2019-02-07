setwd('/Users/Jacob/Dropbox/Work/Diversity_accum')
#setwd('~/Socolar/Data')

#devtools::install_github('jsocolar/BirdDiversification/phylosympatry')
library(phylosympatry)
library(data.table)
library(Matrix)
library(magrittr)

load("PTrees.Rdata")
load("breeding_data.Rdata")
'%ni%' <- Negate('%in%')

tree <- PTrees[[1]]
breeding_data <- breeding_data[-which(duplicated(breeding_data[,1])), ]
rownames(breeding_data) <- breeding_data[,1]
breeding_data <- breeding_data[, -1]

#which(rownames(breeding_data) %ni% tree$tip.label)
#which(tree$tip.label %ni% rownames(breeding_data))

trimmed_tree <- ape::drop.tip(tree, which(tree$tip.label %ni% rownames(breeding_data)))
lineages <- get_lineages(trimmed_tree, 20)

rangesizes <- rowSums(breeding_data)
richnesses <- colSums(breeding_data)

lineages <- as.data.table(lineages)
setkey(lineages,ID,species)
species_stats <- data.table('species'=rownames(breeding_data),
                            'rangesize.species'=rangesizes)

setkey(lineages,species)

D <- Matrix(as.matrix(breeding_data),sparse=T)


spp.map <- data.table('species'=D@Dimnames[[1]],
                      'i'=1:D@Dim[1])
col.map <- data.table('sample'=D@Dimnames[[2]],
                      'j'=1:D@Dim[2])
D <- as.data.table(summary(D)[,c('i','j')])
setkey(D,i)
setkey(spp.map,i)
D <- D[spp.map]

setkey(D,j)
setkey(col.map,j)
D <- D[col.map]

richness.map <- data.table('sample'=colnames(breeding_data),
                           'richness.site'=richnesses)
setkey(D,sample)
setkey(richness.map,sample)
D <- D[richness.map]

D <- D[,c('species','sample','richness.site')]

setkey(species_stats,species)
setkey(D,species)

species_stats <- species_stats[D,nomatch=0]

setkey(species_stats,species)


setkey(lineages,species)
S <- species_stats[lineages]

S[,richicality.species:=mean(richness.site),by=species]

S[,rangesize.lineage:=length(unique(sample)),by=ID]

S[,richness.lineage:=length(unique(species)), by=ID]

### the data.table S has everything you need. If you want the average richness in a lineage:
setkey(S,ID,species)
S1.1 <- S[,list(x=unique(richicality.species),rangesize.lineage=unique(rangesize.lineage),
                richness.lineage=unique(richness.lineage)),by=c('ID','species')]
S1 <- S1.1[,list(richicality.lineage=mean(x), rangesize.lineage=unique(rangesize.lineage),
                 richness.lineage=unique(richness.lineage)), by=ID]

# richicality2: mean richness over the lineage's range
setkey(S,ID,sample)
S2.1 <- S[,list(x=unique(richicality.species),rangesize.lineage=unique(rangesize.lineage),
                richness.lineage=unique(richness.lineage)),by=c('ID','sample')]
S2 <- S2.1[,list(richicality.lineage=mean(x), rangesize.lineage=unique(rangesize.lineage),
                 richness.lineage=unique(richness.lineage)), by=ID]

# richicality3: mean richness over the lineage's range, weighted by the lineage richness
S3 <- S[,list(richicality.lineage=mean(richness.site), rangesize.lineage=unique(rangesize.lineage),
              richness.lineage=unique(richness.lineage)), by=ID]

richicality_frame <- S1[,c(1,4,3,2)]
names(richicality_frame)[4] <- "richicality.lineage.1"
richicality_frame$richicality.lineage.2 <- S2$richicality.lineage
richicality_frame$richicality.lineage.3 <- S3$richicality.lineage
pairs(richicality_frame)

richicality_frame2 <- richicality_frame
richicality_frame2$richness.lineage <- scale(richicality_frame$richness.lineage)
richicality_frame2$rangesize.lineage <- scale(richicality_frame$rangesize.lineage)
richicality_frame2$richicality.lineage.1 <- scale(richicality_frame$richicality.lineage.1)
richicality_frame2$richicality.lineage.2 <- scale(richicality_frame$richicality.lineage.2)
richicality_frame2$richicality.lineage.3 <- scale(richicality_frame$richicality.lineage.3)


summary(lm(rangesize.lineage ~ richness.lineage*richicality.lineage.1, data = richicality_frame2))
summary(lm(rangesize.lineage ~ richness.lineage*richicality.lineage.2, data = richicality_frame2))
summary(lm(rangesize.lineage ~ richness.lineage*richicality.lineage.3, data = richicality_frame2))
