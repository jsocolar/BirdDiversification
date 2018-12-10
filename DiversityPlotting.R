library(raster)
library(ape)
library(phytools)
"%ni%" <- Negate("%in%")
setwd("/Users/JacobSocolar/Dropbox/Work/Diversity_accum")
bpdf_list <- list()
for(i in 1:30){
  load(paste(paste("bpdf_reproj_", i, ".Rdata", sep="")))
  bpdf_list[[i]] <- bpdf_reproj
}

for(tree_i in 1:30){

HTrees1 <- PTrees[[tree_i]]

cc <- cophenetic(HTrees1)

time <- 5

rList <- list()
for(i in 1:dim(cc)[1]){
  rList[[i]] <- rownames(cc)[cc[,i]<(2*time)]
}

Tsize <- rep(NA, dim(breeding_data)[2]-1)
for(i in 2:dim(breeding_data)[2]){
  Tsize[i-1] <- length(unique(unlist(rList[which(rownames(cc) %in% breeding_data[which(breeding_data[,i]==1),1])])))
}

Csize <- bpdf_list[[tree_i]]$allrich

Hsize <- Csize

for(i in 1:length(Hsize)){
  print(i)
  sList <- breeding_data[which(breeding_data[,i+1]==1),1]
  ri <- length(sList)
  for(j in 1:max(1,ri)){
    if(length(sList) >= j){
      current_species <- sList[j]
      related_species <- rList[[which(rownames(cc)==current_species)]]
      related_species <- related_species[-which(related_species==current_species)]
      if(length(which(sList %in% related_species)) > 0){
        sList <- sList[-which(sList %in% related_species)]
      }
    }
  }
  Hsize[i] <- length(sList)
}

tC <- Csize
tH <- Hsize
tT <- Tsize

tC <- tC[-which(is.na(Csize))]
tC2 <- tC
tH <- tH[-which(is.na(Csize))]
tT <- tT[-which(is.na(Csize))]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]


pdf(file = paste0("Plots/World_smooth5my_", tree_i, ".pdf"),width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n', cex.axis=.5)
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.05,0.35,.05), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Combined", line=1.7)
dev.off()

pdf(file = paste0("Plots/World_smooth_divers_5my_", tree_i, ".pdf"),width=4, height=3.6, family="sans")
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(2,8,2), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Diversification-rate", line=1.7)
dev.off()

pdf(file = paste0("Plots/World_smooth_sym_5my_", tree_i, ".pdf"),width=4, height=3.6, family="sans")
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.02,0.1,.02), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Sympatry-based", line=1.7)
dev.off()

}


