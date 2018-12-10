#HTrees1 <- PTrees[[30]]

#cc <- cophenetic(HTrees1)

split.screen(rbind(
  c(0, .5, .3, .9),
  c(.5, 1, .6, 1),
  c(.5, 1, .2, .6),
  c(0,1,0,.2)
))


time <- 40 # Use 3, 10, and 40


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




place <- points_cont$Au.Ptf

tC <- Csize[place]
tC2 <- Csize[place]
tH <- Hsize[place]
tT <- Tsize[place]
tC <- tC[which(tC2 > 20)]
tH <- tH[which(tC2 > 20)]
tT <- tT[which(tC2 > 20)]
close.screen(all.screens = TRUE)
split.screen(rbind(
  c(0, .5, .2, .8),
  c(.5, 1, .6, 1),
  c(.5, 1, .2, .6),
  c(0,1,0,.2)
))
screen(1)
par(mar = c(3, 3, 0, 0))
scatter.smooth((tC/tH - 1) ~ tC, pch=".", lpars = list(col="dodgerblue1", lwd=3), xlab = "Species Richness", ylab= "RTM")
screen(2)
par(mar = c(3, 3, 0, 0))
scatter.smooth((tT/tH - 1) ~ tC,pch=".",lpars = list(col="coral", lwd=3), xlab = "Species Richness", ylab= "DRM")
screen(3)
par(mar = c(3, 3, 0, 0))
scatter.smooth(((tC-tH)/(tT-tH)) ~ tC, pch=".", lpars = list(col="brown", lwd=3), xlab = "Species Richness", ylab= "SBM")
screen(4)
text(x=.5, y=.8, labels="Species Richness", cex=1.3)
#####################
