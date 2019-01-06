library(sp)
"%ni%" <- Negate("%in%")
setwd("/Users/TingleyLab/Dropbox/Work/Diversity_accum")
crs2 <- CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")

load("continents.Rdata")



attach(continents)
ecoregions <- rgdal::readOGR("official/wwf_terr_ecos.shp")

ecoregions <- spTransform(ecoregions, crs2)

tropical.forests <- ecoregions[which(ecoregions$BIOME %in% 1:3),]
tropical.forests <- rgeos::gUnaryUnion(tropical.forests)
Americas.trop.for <- rgeos::gIntersection(Americas, tropical.forests)
Africa.trop.for <- rgeos::gIntersection(Africa, tropical.forests)
Eurasia.trop.for <- rgeos::gIntersection(Eurasia2, tropical.forests)
Australia.trop.for <- rgeos::gIntersection(Australia, tropical.forests)
mainland.trop.for <- rgeos::gUnion(Americas.trop.for, Africa.trop.for)
mainland.trop.for <- rgeos::gUnion(mainland.trop.for, Australia.trop.for)
mainland.trop.for <- rgeos::gUnion(mainland.trop.for, Eurasia.trop.for)

tfs <- list(tropical.forests=tropical.forests, Americas.trop.for=Americas.trop.for,Africa.trop.for=Africa.trop.for,
            Eurasia.trop.for=Eurasia.trop.for, Australia.trop.for=Australia.trop.for, mainland.trop.for=mainland.trop.for)
save(tfs, file="tfs.Rdata")

load("tfs.Rdata")
attach(tfs)

taiga <- ecoregions[which(ecoregions$BIOME == 6),]
taiga <- rgeos::gUnaryUnion(taiga)
Americas.taiga <- rgeos::gIntersection(Americas, taiga)
Eurasia.taiga <- rgeos::gIntersection(Eurasia2, taiga)
mainland.taiga <- rgeos::gUnion(Americas.taiga, Eurasia.taiga)

taigas <- list(taiga=taiga, Americas.taiga=Americas.taiga, Eurasia.taiga=Eurasia.taiga, mainland.taiga=mainland.taiga)
save(taigas, file="taigas.Rdata")

temperate <- ecoregions[which(ecoregions$BIOME %in% c(4,5)),]
temp <- rgeos::gUnaryUnion(temperate)
N.America.temp <- rgeos::gIntersection(N.America, temp)
S.America.temp <- rgeos::gIntersection(S.America, temp)
Eurasia.temp <- rgeos::gIntersection(Eurasia2, temp)
Africa.temp <- rgeos::gIntersection(Africa, temp)
Australia.temp <- rgeos::gIntersection(Australia, temp)
mainland.temp <- rgeos::gUnion(N.America.temp, S.America.temp)
mainland.temp <- rgeos::gUnion(mainland.temp, Eurasia.temp)
mainland.temp <- rgeos::gUnion(mainland.temp, Australia.temp)  #Mainland temp does not include the Atlas mountains of Africa


temps <- list(temp=temp, N.America.temp=N.America.temp, S.America.temp=S.America.temp, Eurasia.temp=Eurasia.temp, 
              Africa.temp=Africa.temp, Australia.temp=Australia.temp, mainland.temp=mainland.temp)
save(temps, file="temps.Rdata")



grassland <- ecoregions[which(ecoregions$BIOME == 8),]
grass <- rgeos::gUnaryUnion(grassland)
N.America.grass <- rgeos::gIntersection(N.America, grass)
S.America.grass <- rgeos::gIntersection(S.America, grass)
Eurasia.grass <- rgeos::gIntersection(Eurasia2, grass)
Africa.grass <- rgeos::gIntersection(Africa, grass)  #there is no Africa grass
Australia.grass <- rgeos::gIntersection(Australia, grass)
mainland.grass <- rgeos::gUnion(N.America.grass, S.America.grass)
mainland.grass <- rgeos::gUnion(mainland.grass, Eurasia.grass)
mainland.grass <- rgeos::gUnion(mainland.grass, Australia.grass)



grasses <- list(grass=grass, N.America.grass=N.America.grass, S.America.grass=S.America.grass, Eurasia.grass=Eurasia.grass, 
                 Australia.grass=Australia.grass, mainland.grass=mainland.grass)
save(grasses, file="grasses.Rdata")

savanna <- ecoregions[which(ecoregions$BIOME %in% c(7,9)),]
savs <- rgeos::gUnaryUnion(savanna)
Americas.sav <- rgeos::gIntersection(Americas, savs)
Eurasia.sav <- rgeos::gIntersection(Eurasia2, savs)
Africa.sav <- rgeos::gIntersection(Africa, savs)
Australia.sav <- rgeos::gIntersection(Australia, savs)
mainland.sav <- rgeos::gUnion(Americas.sav, Africa.sav)
mainland.sav <- rgeos::gUnion(mainland.sav, Eurasia.sav)
mainland.sav <- rgeos::gUnion(mainland.sav, Australia.sav)



savs <- list(savs=savs, Americas.sav=Americas.sav, Eurasia.sav=Eurasia.sav, 
                Australia.sav=Australia.sav, mainland.sav=mainland.sav)
save(savs, file="savs.Rdata")


desert <- ecoregions[which(ecoregions$BIOME == 13),]
des <- rgeos::gUnaryUnion(desert)
N.Am.des <- rgeos::gIntersection(N.America, des)
S.Am.des <- rgeos::gIntersection(S.America, des)
Eu.des <- rgeos::gIntersection(Eurasia2, des)
Af.des <- rgeos::gIntersection(Africa, des)
Au.des <- rgeos::gIntersection(Australia, des)
mainland.des <- rgeos::gUnion(N.Am.des, S.Am.des)
mainland.des <- rgeos::gUnion(mainland.des, Af.des)
mainland.des <- rgeos::gUnion(mainland.des, Eu.des)
mainland.des <- rgeos::gUnion(mainland.des, Au.des)



des <- list(des=des, N.Am.des=N.Am.des, S.Am.des=S.Am.des, Eu.des=Eu.des, 
             Au.des=Au.des, mainland.des=mainland.des)
save(des, file="des.Rdata")



Am.P <- !is.na(sp::over(bpdf_reproj, Americas))
Af.P <- !is.na(sp::over(bpdf_reproj, Africa))
Eu.P <- !is.na(sp::over(bpdf_reproj, Eurasia2))
Au.P <- !is.na(sp::over(bpdf_reproj, Australia))

Am.Ptf <- !is.na(sp::over(bpdf_reproj, Americas.trop.for))
Af.Ptf <- !is.na(sp::over(bpdf_reproj, Africa.trop.for))
Eu.Ptf <- !is.na(sp::over(bpdf_reproj, Eurasia.trop.for))
Au.Ptf <- !is.na(sp::over(bpdf_reproj, Australia.trop.for))
all.Ptf <- !is.na(sp::over(bpdf_reproj, tropical.forests))

Am.Tai <- !is.na(sp::over(bpdf_reproj, Americas.taiga))
Eu.Tai <- !is.na(sp::over(bpdf_reproj, Eurasia.taiga))
all.Tai <- !is.na(sp::over(bpdf_reproj, mainland.taiga))

N.Am.Temp <- !is.na(sp::over(bpdf_reproj, N.America.temp))
S.Am.Temp <- !is.na(sp::over(bpdf_reproj, S.America.temp))
Eurasia.Temp <- !is.na(sp::over(bpdf_reproj, Eurasia.temp))
Australia.Temp <- !is.na(sp::over(bpdf_reproj, Australia.temp))
all.Temp <- !is.na(sp::over(bpdf_reproj, mainland.temp))

N.Am.Grass <- !is.na(sp::over(bpdf_reproj, N.America.grass))
S.Am.Grass <- !is.na(sp::over(bpdf_reproj, S.America.grass))
Eurasia.Grass <- !is.na(sp::over(bpdf_reproj, Eurasia.grass))
Australia.Grass <- !is.na(sp::over(bpdf_reproj, Australia.grass))
all.Grass <- !is.na(sp::over(bpdf_reproj, mainland.grass))

Am.Sav <- !is.na(sp::over(bpdf_reproj, Americas.sav))
Af.Sav <- !is.na(sp::over(bpdf_reproj, Africa.sav))
Eu.Sav <- !is.na(sp::over(bpdf_reproj, Eurasia.sav))
Au.Sav <- !is.na(sp::over(bpdf_reproj, Australia.sav))
all.Sav <- !is.na(sp::over(bpdf_reproj, mainland.sav))

N.Am.Des <- !is.na(sp::over(bpdf_reproj, N.Am.des))
S.Am.Des <- !is.na(sp::over(bpdf_reproj, S.Am.des))
Af.Des <- !is.na(sp::over(bpdf_reproj, Af.des))
Eu.Des <- !is.na(sp::over(bpdf_reproj, Eu.des))
Au.Des <- !is.na(sp::over(bpdf_reproj, Au.des))
all.Des <- !is.na(sp::over(bpdf_reproj, mainland.des))








tropical.forests.e <- ecoregions[which(ecoregions$BIOME==1),]
tropical.forests.e <- rgeos::gUnaryUnion(tropical.forests.e)
Americas.trop.for.e <- rgeos::gIntersection(Americas, tropical.forests.e)
Africa.trop.for.e <- rgeos::gIntersection(Africa, tropical.forests.e)
Eurasia.trop.for.e <- rgeos::gIntersection(Eurasia2, tropical.forests.e)
Australia.trop.for.e <- rgeos::gIntersection(Australia, tropical.forests.e)
mainland.trop.for.e <- rgeos::gUnion(Americas.trop.for.e, Africa.trop.for.e)
mainland.trop.for.e <- rgeos::gUnion(mainland.trop.for.e, Australia.trop.for.e)
mainland.trop.for.e <- rgeos::gUnion(mainland.trop.for.e, Eurasia.trop.for.e)

tfs.e <- list(tropical.forests.e=tropical.forests.e, Americas.trop.for.e=Americas.trop.for.e,Africa.trop.for.e=Africa.trop.for.e,
            Eurasia.trop.for.e=Eurasia.trop.for.e, Australia.trop.for.e=Australia.trop.for.e, mainland.trop.for.e=mainland.trop.for.e)
save(tfs.e, file="tfs_e.Rdata")

load("tfs_e.Rdata")
attach(tfs.e)

tropical.forests.d <- ecoregions[which(ecoregions$BIOME==2),]
tropical.forests.d <- rgeos::gUnaryUnion(tropical.forests.d)
Americas.trop.for.d <- rgeos::gIntersection(Americas, tropical.forests.d)
Africa.trop.for.d <- rgeos::gIntersection(Africa, tropical.forests.d)
Eurasia.trop.for.d <- rgeos::gIntersection(Eurasia2, tropical.forests.d)
#Australia.trop.for.d <- rgeos::gIntersection(Australia, tropical.forests.d)
mainland.trop.for.d <- rgeos::gUnion(Americas.trop.for.d, Africa.trop.for.d)
mainland.trop.for.d <- rgeos::gUnion(mainland.trop.for.d, Eurasia.trop.for.d)

tfs.d <- list(tropical.forests.d=tropical.forests.d, Americas.trop.for.d=Americas.trop.for.d,Africa.trop.for.d=Africa.trop.for.d,
              Eurasia.trop.for.d=Eurasia.trop.for.d, Australia.trop.for.d=Australia.trop.for.d, mainland.trop.for.d=mainland.trop.for.d)
save(tfs.d, file="tfs_d.Rdata")

load("tfs_d.Rdata")
attach(tfs.d)

tropical.forests.c <- ecoregions[which(ecoregions$BIOME==3),]
tropical.forests.c <- rgeos::gUnaryUnion(tropical.forests.c)
Americas.trop.for.c <- rgeos::gIntersection(Americas, tropical.forests.c)
#Africa.trop.for.c <- rgeos::gIntersection(Africa, tropical.forests.c)
Eurasia.trop.for.c <- rgeos::gIntersection(Eurasia2, tropical.forests.c)
#Australia.trop.for.c <- rgeos::gIntersection(Australia, tropical.forests.c)
mainland.trop.for.c <- rgeos::gUnion(Americas.trop.for.c, Eurasia.trop.for.c)

tfs.c <- list(tropical.forests.c=tropical.forests.c, Americas.trop.for.c=Americas.trop.for.c,Africa.trop.for.c=Africa.trop.for.c,
              Eurasia.trop.for.c=Eurasia.trop.for.c, Australia.trop.for.c=Australia.trop.for.c, mainland.trop.for.c=mainland.trop.for.c)
save(tfs.c, file="tfs_c.Rdata")

load("tfs_c.Rdata")
attach(tfs.c)


Am.TFe <- !is.na(sp::over(bpdf_reproj, Americas.trop.for.e))
Af.TFe <- !is.na(sp::over(bpdf_reproj, Africa.trop.for.e))
Eu.TFe <- !is.na(sp::over(bpdf_reproj, Eurasia.trop.for.e))
Au.TFe <- !is.na(sp::over(bpdf_reproj, Australia.trop.for.e))
all.TFe <- !is.na(sp::over(bpdf_reproj, mainland.trop.for.e))

Am.TFd <- !is.na(sp::over(bpdf_reproj, Americas.trop.for.d))
Af.TFd <- !is.na(sp::over(bpdf_reproj, Africa.trop.for.d))
Eu.TFd <- !is.na(sp::over(bpdf_reproj, Eurasia.trop.for.d))
#Au.TFd <- !is.na(sp::over(bpdf_reproj, Australia.trop.for.d))
all.TFd <- !is.na(sp::over(bpdf_reproj, mainland.trop.for.d))

Am.TFc <- !is.na(sp::over(bpdf_reproj, Americas.trop.for.c))
#Af.TFc <- !is.na(sp::over(bpdf_reproj, Africa.trop.for.c))
Eu.TFc <- !is.na(sp::over(bpdf_reproj, Eurasia.trop.for.c))
#Au.TFc <- !is.na(sp::over(bpdf_reproj, Australia.trop.for.c))
all.TFc <- !is.na(sp::over(bpdf_reproj, mainland.trop.for.c))


temperate.d <- ecoregions[which(ecoregions$BIOME == 4),]
temp.d <- rgeos::gUnaryUnion(temperate.d)
N.America.temp.d <- rgeos::gIntersection(N.America, temp.d)
S.America.temp.d <- rgeos::gIntersection(S.America, temp.d)
Eurasia.temp.d <- rgeos::gIntersection(Eurasia2, temp.d)
Africa.temp.d <- rgeos::gIntersection(Africa, temp.d)
Australia.temp.d <- rgeos::gIntersection(Australia, temp.d)
mainland.temp.d <- rgeos::gUnion(N.America.temp.d, S.America.temp.d)
mainland.temp.d <- rgeos::gUnion(mainland.temp.d, Eurasia.temp.d)
mainland.temp.d <- rgeos::gUnion(mainland.temp.d, Australia.temp.d)  #Mainland temp does not include the Atlas mountains of Africa

temperate.c <- ecoregions[which(ecoregions$BIOME == 5),]
temp.c <- rgeos::gUnaryUnion(temperate.c)
N.America.temp.c <- rgeos::gIntersection(N.America, temp.c)
#S.America.temp.c <- rgeos::gIntersection(S.America, temp.c)
Eurasia.temp.c <- rgeos::gIntersection(Eurasia2, temp.c)
Africa.temp.c <- rgeos::gIntersection(Africa, temp.c)
#Australia.temp.c <- rgeos::gIntersection(Australia, temp.c)
mainland.temp.c <- rgeos::gUnion(N.America.temp.c, Eurasia.temp.c)  #Mainland temp does not include the Atlas mountains of Africa


N.Am.Temp.d <- !is.na(sp::over(bpdf_reproj, N.America.temp.d))
S.Am.Temp.d <- !is.na(sp::over(bpdf_reproj, S.America.temp.d))
Eurasia.Temp.d <- !is.na(sp::over(bpdf_reproj, Eurasia.temp.d))
Australia.Temp.d <- !is.na(sp::over(bpdf_reproj, Australia.temp.d))
all.Temp.d <- !is.na(sp::over(bpdf_reproj, mainland.temp.d))

N.Am.Temp.c <- !is.na(sp::over(bpdf_reproj, N.America.temp.c))
#S.Am.Temp.c <- !is.na(sp::over(bpdf_reproj, S.America.temp.c))
Eurasia.Temp.c <- !is.na(sp::over(bpdf_reproj, Eurasia.temp.c))
#Australia.Temp.c <- !is.na(sp::over(bpdf_reproj, Australia.temp.c))
all.Temp.c <- !is.na(sp::over(bpdf_reproj, mainland.temp.c))


points_cont <- list(Am.Ptf=Am.Ptf, Eu.Ptf=Eu.Ptf, Au.Ptf=Au.Ptf, Af.Ptf=Af.Ptf, all.Ptf=all.Ptf,
                    Am.All=Am.P, Eu.All=Eu.P, Au.All=Au.P, Af.All=Af.P,
                    Am.Tai=Am.Tai, Eu.Tai=Eu.Tai, all.Tai=all.Tai,
                    N.Am.Temp=N.Am.Temp, S.Am.Temp=S.Am.Temp, Eurasia.Temp=Eurasia.Temp, Australia.Temp=Australia.Temp, all.Temp=all.Temp,
                    N.Am.Grass=N.Am.Grass, S.Am.Grass=S.Am.Grass, Eurasia.Grass=Eurasia.Grass, Australia.Grass=Australia.Grass, all.Grass=all.Grass,
                    Am.Sav=Am.Sav, Eu.Sav=Eu.Sav, Au.Sav=Au.Sav, Af.Sav=Af.Sav, all.Sav=all.Sav,
                    N.Am.Des=N.Am.Des, S.Am.Des=S.Am.Des, Af.Des=Af.Des, Eu.Des=Eu.Des, Au.Des=Au.Des, all.Des=all.Des,
                    Am.TFe=Am.TFe, Eu.TFe=Eu.TFe, Au.TFe=Au.TFe, Af.TFe=Af.TFe, all.TFe=all.TFe,
                    Am.TFd=Am.TFd, Eu.TFd=Eu.TFd, Af.TFd=Af.TFd, all.TFd=all.TFd,
                    Am.TFc=Am.TFc, Eu.TFc=Eu.TFc, all.TFc=all.TFc,
                    N.Am.Temp.d=N.Am.Temp.d, S.Am.Temp.d=S.Am.Temp.d, Eurasia.Temp.d=Eurasia.Temp.d, Australia.Temp.d=Australia.Temp.d, all.Temp.d=all.Temp.d,
                    N.Am.Temp.c.c=N.Am.Temp.c, Eurasia.Temp.c=Eurasia.Temp.c, all.Temp.c=all.Temp.c)
save(points_cont, file="points_cont.Rdata")
