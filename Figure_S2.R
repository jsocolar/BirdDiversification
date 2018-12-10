

HTrees1 <- PTrees[[tree_i]]

cc <- cophenetic(HTrees1)

rList <- list()
for(i in 1:dim(cc)[1]){
  rList[[i]] <- rownames(cc)[cc[,i]<(2*time)]
}

Tsize <- rep(NA, dim(breeding_data)[2]-1)
Csize2 <- Tsize
for(i in 2:dim(breeding_data)[2]){
  Tsize[i-1] <- length(unique(unlist(rList[which(rownames(cc) %in% breeding_data[which(breeding_data[,i]==1),1])])))
  Csize2[i-1] <- sum(breeding_data[,i])
}

Csize <- bpdf_list[[tree_i]]$allrich
Hsize <- Csize

for(i in 1:length(Hsize)){
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




dev.off()
tree_i <- 30
pdf(file = "/Users/TingleyLab/Dropbox/Work/Diversity_accum/FigureS2.pdf", width=9.5, height=7, family="sans")
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
  
  tC <- tC[-which(tC2<4)]  # Had to exclude sites with less than 4 to prevent numerical issues in LOESS fitting
  tH <- tH[-which(tC2<4)]
  tT <- tT[-which(tC2<4)]

  
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
