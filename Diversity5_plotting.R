library(raster)
library(ape)
library(phytools)
"%ni%" <- Negate("%in%")
setwd("/Users/TingleyLab/Dropbox/Work/Diversity_accum")

masked.rasters <- list()
for(i in 1:2){
  masked.rasters[[i]] <- raster(paste("msr2NEW_",i,".grd", sep=""))
  print(i)
}

load("continents.Rdata")
load("tfs.Rdata")
load("breeding_data.Rdata")
load("points_cont.Rdata")
load("PTrees.Rdata")
bpdf_list <- list()
for(i in 1:30){
  load(paste(paste("bpdf_reproj_", i, ".Rdata", sep="")))
  bpdf_list[[i]] <- bpdf_reproj
}



d.contrib <- 1/masked.rasters[[1]] - 1  # Diversification contribution
sr2 <- masked.rasters[[2]]   # All richness


Am.d.contrib <- mask(d.contrib,tfs$Americas.trop.for)
Af.d.contrib <- mask(d.contrib,tfs$Africa.trop.for)
Au.d.contrib <- mask(d.contrib,tfs$Australia.trop.for)
Eu.d.contrib <- mask(d.contrib,tfs$Eurasia.trop.for)
tf.d.contrib <- mask(d.contrib, tfs$mainland.trop.for)

Am.sr2 <- mask(sr2,tfs$Americas.trop.for)
Af.sr2 <- mask(sr2,tfs$Africa.trop.for)
Au.sr2 <- mask(sr2,tfs$Australia.trop.for)
Eu.sr2 <- mask(sr2,tfs$Eurasia.trop.for)
tf.sr2 <- mask(sr2,tfs$mainland.trop.for)

d20 <- d.contrib
d20[sr2<21] <- NA

col=colorRampPalette(c("navy", "coral", "gold1"))(120)
col2=colorRampPalette(c("navy", "coral"))(120)
col3=viridis::viridis(120)
col4=viridis::magma(120)


dev.off()
pdf(file = "Plots/richness_global.pdf",width=7, height=4.1, family="sans")
plot(sr2, interpolate=T, col=col3, axes=F, box=F, maxpixels=50000000)
dev.off()

dev.off()
pdf(file = "Plots/richness_tropics.pdf",width=7, height=4.1, family="sans")
plot(d.contrib, col="gray70", axes=F, box=F, legend=F, maxpixels=50000000)
par(new=T)
plot(tf.sr2, col=col3, interpolate=T, axes=F, box=F, maxpixels=50000000)
dev.off()

dev.off()
pdf(file = "Plots/dcont_global.pdf",width=7, height=4.1, family="sans")
plot(d.contrib, col=col4, interpolate=T, axes=F, box=F, maxpixels=50000000)
dev.off()

dev.off()
pdf(file = "Plots/dcont_global20.pdf",width=7, height=4.1, family="sans")
plot(d.contrib, col="gray70", axes=F, box=F, legend=F, maxpixels=50000000)
par(new=T)
plot(d20, interpolate=T, col=col4, axes=F, box=F, maxpixels=50000000)
dev.off()


dev.off()
pdf(file = "Plots/dcont_tropics.pdf",width=7, height=4.1, family="sans")
plot(d.contrib, col="gray70", axes=F, box=F, legend=F, maxpixels=50000000)
par(new=T)
plot(tf.d.contrib, interpolate=T, col=col4, axes=F, box=F, maxpixels=50000000)
dev.off()




plot(d.contrib, col="gray70", axes=F, box=F, legend=F)
par(new=T)
plot(Au.d.contrib, col=col2, axes=F, box=F, legend=F)
par(new=T)
plot(Am.d.contrib, col=col2, axes=F, box=F, legend=F)
par(new=T)
plot(Af.d.contrib, col=col2, axes=F, box=F, legend=F)
par(new=T)
plot(Eu.d.contrib, col=col2, axes=F, box=F, legend=F)

plot(d.contrib, col="gray70", axes=F, box=F, legend=F)
par(new=T)
plot(Am.sr2, col=col3, axes=F, box=F, legend=F)
par(new=T)
plot(Af.sr2, col=col3, axes=F, box=F, legend=F)
par(new=T)
plot(Au.sr2, col=col3, axes=F, box=F, legend=F)
par(new=T)
plot(Eu.sr2, col=col3, axes=F, box=F, legend=F)




dev.off()
pdf(file = "Plots/scatter_worldAll.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])
x <- 1/bpdf_reproj$L5ma - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32", lpars = list(col="dodgerblue1", lwd=3), ylim=c(0,560), 
               ylab="", xlab="", yaxt='n', xaxt='n')
axis(side=2, at=seq(100,500,100), padj=.7)
axis(side=1, at=seq(0,30,.1), padj=-.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()



dev.off()
pdf(file = "Plots/scatter_world20.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[which(colSums(breeding_data[,2:ncol(breeding_data)])>20)]
x <- 1/bpdf_reproj$L5ma[which(colSums(breeding_data[,2:ncol(breeding_data)])>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32", lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", ylim=c(20,560), yaxt='n', xaxt='n')
axis(side=2, at=seq(100,500,100), padj=.7)
axis(side=1, at=seq(0,30,.1), padj=-.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/scatter_pantropical.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$all.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$all.Ptf]>20)]
x <- 1/bpdf_reproj$L5ma[points_cont$all.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$all.Ptf]>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", ylim=c(20,560), yaxt='n', xaxt='n')
axis(side=2, at=seq(100,500,100), padj=.7)
axis(side=1, at=seq(0.05,0.15,.05), padj=-.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/scatter_americas.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Am.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Am.Ptf]>20)]
x <- 1/bpdf_reproj$L5ma[points_cont$Am.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Am.Ptf]>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=2, at=seq(100,500,100), padj=.7)
axis(side=1, at=seq(0.05,0.15,.05), padj=-.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/scatter_africa.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Af.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Af.Ptf]>20)]
x <- 1/bpdf_reproj$L5ma[points_cont$Af.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Af.Ptf]>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", xaxt='n', yaxt='n')
axis(side=1, at=c(.05,.1,.15), padj=-.7)
axis(side=2, at=seq(100,500,100), padj=.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/scatter_australia.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Au.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Au.Ptf]>20)]
x <- 1/bpdf_reproj$L5ma[points_cont$Au.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Au.Ptf]>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", xaxt='n', yaxt='n')
axis(side=1, at=c(.05,.1,.15), padj=-.7)
axis(side=2, at=seq(150,250,50), padj=.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/scatter_asia.pdf",width=3.6, height=3.3, family="sans")
y <- colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Eu.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Eu.Ptf]>20)]
x <- 1/bpdf_reproj$L5ma[points_cont$Eu.Ptf][which(colSums(breeding_data[,2:ncol(breeding_data)])[points_cont$Eu.Ptf]>20)] - 1
#plot(x,y, pch=".")
#lines(ksmooth(x, y, "normal", bandwidth = .04), col = "purple", lty=7)
scatter.smooth(y~x,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=1, at=c(.04,.08,.12), padj=-.7)
axis(side=2, at=seq(100,500,100), padj=.7)
title(xlab = "Diversification Contribution",
      ylab = "Species Richness", line=1.7)
dev.off()






close.screen()
close.screen(all.screens = T)
dev.off()


dev.off()
for(tree_i in 1:30){
pdf(file = paste("/Users/TingleyLab/Dropbox/Work/Diversity_accum/smooth_figure_tree", tree_i, ".pdf", sep=""), width=9.5, height=7, family="sans")
close.screen(all=T)

split.screen(rbind(
  #Row 1
  c(0/18, 2/18, 12/14, 14/14),
  c(2/18, 3/18, 13/14, 14/14),
  c(2/18, 3/18, 12/14, 13/14),
  c(3/18, 5/18, 12/14, 14/14),
  c(5/18, 6/18, 13/14, 14/14),
  c(5/18, 6/18, 12/14, 13/14),
  c(6/18, 8/18, 12/14, 14/14),
  c(8/18, 9/18, 13/14, 14/14),
  c(8/18, 9/18, 12/14, 13/14),
  c(9/18, 11/18, 12/14, 14/14),
  c(11/18, 12/18, 13/14, 14/14),
  c(11/18, 12/18, 12/14, 13/14),
  c(12/18, 14/18, 12/14, 14/14),
  c(14/18, 15/18, 13/14, 14/14),
  c(14/18, 15/18, 12/14, 13/14),
  c(15/18, 17/18, 12/14, 14/14),
  c(17/18, 18/18, 13/14, 14/14),
  c(17/18, 18/18, 12/14, 13/14),
  
  # Row 2
  c(0/18, 2/18, 10/14, 12/14),
  c(2/18, 3/18, 11/14, 12/14),
  c(2/18, 3/18, 10/14, 11/14),
  c(3/18, 5/18, 10/14, 12/14),
  c(5/18, 6/18, 11/14, 12/14),
  c(5/18, 6/18, 10/14, 11/14),
  c(6/18, 8/18, 10/14, 12/14),
  c(8/18, 9/18, 11/14, 12/14),
  c(8/18, 9/18, 10/14, 11/14),
  c(9/18, 11/18, 10/14, 12/14),
  c(11/18, 12/18, 11/14, 12/14),
  c(11/18, 12/18, 10/14, 11/14),
  c(12/18, 14/18, 10/14, 12/14),
  c(14/18, 15/18, 11/14, 12/14),
  c(14/18, 15/18, 10/14, 11/14),
  c(15/18, 17/18, 10/14, 12/14),
  c(17/18, 18/18, 11/14, 12/14),
  c(17/18, 18/18, 10/14, 11/14),
  
  # Row 3
  c(0/18, 2/18, 8/14, 10/14),
  c(2/18, 3/18, 9/14, 10/14),
  c(2/18, 3/18, 8/14, 9/14),
  c(3/18, 5/18, 8/14, 10/14),
  c(5/18, 6/18, 9/14, 10/14),
  c(5/18, 6/18, 8/14, 9/14),
  c(6/18, 8/18, 8/14, 10/14),
  c(8/18, 9/18, 9/14, 10/14),
  c(8/18, 9/18, 8/14, 9/14),
  c(9/18, 11/18, 8/14, 10/14),
  c(11/18, 12/18, 9/14, 10/14),
  c(11/18, 12/18, 8/14, 9/14),
  c(12/18, 14/18, 8/14, 10/14),
  c(14/18, 15/18, 9/14, 10/14),
  c(14/18, 15/18, 8/14, 9/14),
  c(15/18, 17/18, 8/14, 10/14),
  c(17/18, 18/18, 9/14, 10/14),
  c(17/18, 18/18, 8/14, 9/14),
  
  # Row 4
  c(0/18, 2/18, 6/14, 8/14),
  c(2/18, 3/18, 7/14, 8/14),
  c(2/18, 3/18, 6/14, 7/14),
  c(3/18, 5/18, 6/14, 8/14),
  c(5/18, 6/18, 7/14, 8/14),
  c(5/18, 6/18, 6/14, 7/14),
  c(6/18, 8/18, 6/14, 8/14),
  c(8/18, 9/18, 7/14, 8/14),
  c(8/18, 9/18, 6/14, 7/14),
  c(9/18, 11/18, 6/14, 8/14),
  c(11/18, 12/18, 7/14, 8/14),
  c(11/18, 12/18, 6/14, 7/14),
  c(12/18, 14/18, 6/14, 8/14),
  c(14/18, 15/18, 7/14, 8/14),
  c(14/18, 15/18, 6/14, 7/14),
  c(15/18, 17/18, 6/14, 8/14),
  c(17/18, 18/18, 7/14, 8/14),
  c(17/18, 18/18, 6/14, 7/14),
  
  # Row 5
  c(0/18, 2/18, 4/14, 6/14),
  c(2/18, 3/18, 5/14, 6/14),
  c(2/18, 3/18, 4/14, 5/14),
  c(3/18, 5/18, 4/14, 6/14),
  c(5/18, 6/18, 5/14, 6/14),
  c(5/18, 6/18, 4/14, 5/14),
  c(6/18, 8/18, 4/14, 6/14),
  c(8/18, 9/18, 5/14, 6/14),
  c(8/18, 9/18, 4/14, 5/14),
  c(9/18, 11/18, 4/14, 6/14),
  c(11/18, 12/18, 5/14, 6/14),
  c(11/18, 12/18, 4/14, 5/14),
  c(12/18, 14/18, 4/14, 6/14),
  c(14/18, 15/18, 5/14, 6/14),
  c(14/18, 15/18, 4/14, 5/14),
  c(15/18, 17/18, 4/14, 6/14),
  c(17/18, 18/18, 5/14, 6/14),
  c(17/18, 18/18, 4/14, 5/14),
  
  # Row 6
  c(0/18, 2/18, 2/14, 4/14),
  c(2/18, 3/18, 3/14, 4/14),
  c(2/18, 3/18, 2/14, 3/14),
  c(3/18, 5/18, 2/14, 4/14),
  c(5/18, 6/18, 3/14, 4/14),
  c(5/18, 6/18, 2/14, 3/14),
  c(6/18, 8/18, 2/14, 4/14),
  c(8/18, 9/18, 3/14, 4/14),
  c(8/18, 9/18, 2/14, 3/14),
  c(9/18, 11/18, 2/14, 4/14),
  c(11/18, 12/18, 3/14, 4/14),
  c(11/18, 12/18, 2/14, 3/14),
  c(12/18, 14/18, 2/14, 4/14),
  c(14/18, 15/18, 3/14, 4/14),
  c(14/18, 15/18, 2/14, 3/14),
  c(15/18, 17/18, 2/14, 4/14),
  c(17/18, 18/18, 3/14, 4/14),
  c(17/18, 18/18, 2/14, 3/14),
  
  
  # Row 7
  c(0/18, 2/18, 0/14, 2/14),
  c(2/18, 3/18, 1/14, 2/14),
  c(2/18, 3/18, 0/14, 1/14),
  c(3/18, 5/18, 0/14, 2/14),
  c(5/18, 6/18, 1/14, 2/14),
  c(5/18, 6/18, 0/14, 1/14),
  c(6/18, 8/18, 0/14, 2/14),
  c(8/18, 9/18, 1/14, 2/14),
  c(8/18, 9/18, 0/14, 1/14),
  c(9/18, 11/18, 0/14, 2/14),
  c(11/18, 12/18, 1/14, 2/14),
  c(11/18, 12/18, 0/14, 1/14),
  c(12/18, 14/18, 0/14, 2/14),
  c(14/18, 15/18, 1/14, 2/14),
  c(14/18, 15/18, 0/14, 1/14),
  c(15/18, 17/18, 0/14, 2/14),
  c(17/18, 18/18, 1/14, 2/14),
  c(17/18, 18/18, 0/14, 1/14)
))


HTrees1 <- PTrees[[tree_i]]

cc <- cophenetic(HTrees1)

rList <- list()
for(i in 1:dim(cc)[1]){
  rList[[i]] <- rownames(cc)[cc[,i]<10]
}

Tsize <- rep(NA, dim(breeding_data)[2]-1)
for(i in 2:dim(breeding_data)[2]){
  Tsize[i-1] <- length(unique(unlist(rList[which(breeding_data[,i]==1)])))
}

Csize <- bpdf_list[[tree_i]]$allrich
Hsize <- Csize*bpdf_list[[tree_i]]$L5ma


place <- points_cont$Am.All

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

#tC <- tC[which(tC2 > 20)]
#tH <- tH[which(tC2 > 20)]
#tT <- tT[which(tC2 > 20)]

screen(1)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(2)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(3)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Af.All

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(10)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(11)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(12)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Eu.All

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(13)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(14)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(15)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Au.All

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(16)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(17)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(18)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Am.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(19)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(20)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(21)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Af.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(28)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(29)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(30)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Eu.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(31)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(32)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(33)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Au.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(34)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(35)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(36)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$N.Am.Temp

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(40)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(41)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(42)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$S.Am.Temp

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(43)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(44)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(45)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Eurasia.Temp

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(49)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(50)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(51)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Australia.Temp

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 41)] # The range of tC is 213-294, with a single outlying point at 41. 
tH <- tH[which(tC2 > 41)]
tT <- tT[which(tC2 > 41)]

screen(52)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(53)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(54)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Am.Tai

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(58)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(59)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(60)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Eu.Tai

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(67)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(68)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(69)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Am.Sav

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(73)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(74)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(75)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Af.Sav

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(82)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(83)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(84)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Eu.Sav

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(85)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(86)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(87)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Au.Sav

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(88)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(89)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(90)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$N.Am.Grass

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(94)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(95)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(96)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$S.Am.Grass

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(97)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(98)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(99)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Eurasia.Grass

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(103)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(104)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(105)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Australia.Grass

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(106)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(107)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(108)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$N.Am.Des

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(112)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(113)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(114)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$S.Am.Des

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(115)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(116)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(117)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Af.Des

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(118)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(119)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(120)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


#####################

place <- points_cont$Eu.Des

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(121)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(122)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(123)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)

#####################

place <- points_cont$Au.Des

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

screen(124)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM", xaxt='n', yaxt='n', cex.axis=.5)
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(125)
par(mar = c(1, 1, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)
screen(126)
par(mar = c(1, 1, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM", xaxt='n', yaxt='n')
axis(side = 1, tck=-.03, padj=-4, cex.axis=.5)
axis(side = 2, tck=-.025, padj=3, cex.axis=.5)


dev.off()

}


z <- tC
colors <- rev(heat.colors(101)) 
zcolor <- colors[(z - min(z))/diff(range(z))*100 + 1] 
scatter.smooth((tT/tH) ~ (tC/tT), col=zcolor)

scatter.smooth(log(tC/tH - 1) ~ tC, pch=".",lpars = list(col="dodgerblue1", lwd=3))
lines(ksmooth(tC,log(tC/tH - 1), bandwidth=100))
plot(ksmooth(tC,log(tT/tH - 1), bandwidth=100))
plot(ksmooth(tC, log((tC - tH)/(tT - tH)), bandwidth=100))






#####################

place <- points_cont$Am.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

close.screen(all.screens = T)
dev.off()
pdf(file = "Plots/AmericasPDF_smooth.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.05,0.15,.05), padj=.7)
title(xlab = "Species Richness",
      ylab = "Recent-time Contribution", line=1.7)
dev.off()



#####################

place <- points_cont$Af.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

dev.off()
pdf(file = "Plots/AfricaPTF_smooth.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.05,0.15,.05), padj=.7)
title(xlab = "Species Richness",
      ylab = "Recent-time Contribution", line=1.7)
dev.off()

#####################

place <- points_cont$Eu.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

dev.off()
pdf(file = "Plots/EurasiaPTF_smooth.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.05,0.15,.05), padj=.7)
title(xlab = "Species Richness",
      ylab = "Recent-time Contribution", line=1.7)
dev.off()


#####################

place <- points_cont$Au.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]

dev.off()
pdf(file = "Plots/AustralasiaPTF_smooth.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC,pch=".", col="gray32",lpars = list(col="dodgerblue1", lwd=3), xlab = "",
               ylab = "", yaxt='n', xaxt='n')
axis(side=1, at=seq(50,450,100), padj=-.7)
axis(side=2, at=seq(0.05,0.15,.05), padj=.7)
title(xlab = "Species Richness",
      ylab = "Recent-time Contribution", line=1.7)
dev.off()
#####################








tC <- Csize
tC2 <- Csize
tH <- Hsize
tT <- Tsize

tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]


dev.off()
pdf(file = "Plots/World_smooth.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n', cex.axis=.5)
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.05,0.35,.05), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Combined", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/World_smooth_divers.pdf",width=4, height=3.6, family="sans")
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(2,8,2), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Diversification-rate", line=1.7)
dev.off()

dev.off()
pdf(file = "Plots/World_smooth_sym.pdf",width=4, height=3.6, family="sans")
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab="", ylab="", xaxt='n', yaxt='n')
axis(side=1, at=seq(100,500,100), padj=-.7)
axis(side=2, at=seq(0.02,0.1,.02), padj=.7)
#title(xlab = "Species Richness",
#      ylab = "Sympatry-based", line=1.7)
dev.off()


