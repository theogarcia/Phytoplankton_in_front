cbbPalette <- c("#000000","#009E73",
                "#0072B2","#56B4E9",
                "#D55E00","#E69F00") #Color blind palette that will be used 
library(hrbrthemes)
library(gcookbook)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(gridExtra)
library(readODS)
###############################################################
################## Phytoplankton data  ########################
###############################################################


############# Water mass A

## Biomass data from water mass A:
biomasses_hippNS_massA <- read.csv(file = "data/cyto_NS_waterA_biomasses.csv",
                                   header = TRUE,
                                   sep = ",",
                                   quote = "\"",
                                   dec = ".")
str(biomasses_hippNS_massA)


## Convert date-time characters to date-time format:
biomasses_hippNS_massA <- dplyr::mutate(biomasses_hippNS_massA, Time = as.POSIXct(biomasses_hippNS_massA$Time, 
                                                                                  format = "%Y-%m-%d %H:%M",
                                                                                  tz = "GMT"))
dim(biomasses_hippNS_massA)

############# Water mass B
biomasses_hippNS_massB <- read.csv(file = "data/cyto_NS_waterB_biomasses.csv",
                                   header = TRUE,
                                   sep = ",",
                                   quote = "\"",
                                   dec = ".")
## Convert date-time characters to date-time format:
biomasses_hippNS_massB <- dplyr::mutate(biomasses_hippNS_massB, Time = as.POSIXct(biomasses_hippNS_massB$Time, 
                                                                                  format = "%Y-%m-%d %H:%M",
                                                                                  tz = "GMT"))
dim(biomasses_hippNS_massB)

## Number of samples:
nrow(biomasses_hippNS_massA) + nrow(biomasses_hippNS_massB)

## Biomass data of the fullcruise
biomasses_world <- read.csv(file = "data/fullcruise_biomass.csv",
                            header = TRUE,
                            sep = ",",
                            quote = "\"",
                            dec = ".")
biomasses_world <- dplyr::mutate(biomasses_world, Time = as.POSIXct(biomasses_world$Time, 
                                                                    format = "%Y-%m-%d %H:%M",
                                                                    tz = "GMT"))[,c(1:10,15,16,18,19)]
biomasses_world<-na.omit(biomasses_world)
biomasses_world<-biomasses_world[-dim(biomasses_world)[1],]

##-------------------- Pre-tretment of data ---------------------##

############# Water Mass A

## Delete unused columns:
head(biomasses_hippNS_massA)
biomasses_hippNS_massA$Tot_Pico <- biomasses_hippNS_massA$Tot_Nano <- NULL
biomasses_hippNS_massA$full_FLR <- biomasses_hippNS_massA$full_FWS <- biomasses_hippNS_massA$Par <- NULL

summary(biomasses_hippNS_massA$Lon)
summary(biomasses_hippNS_massA$Lat)

## Caracterisation of water masses according to salinity criteria
biomasses_hippNS_massA_tronque <- biomasses_hippNS_massA[which(biomasses_hippNS_massA$Sal >= 37.6), ]
nrow(biomasses_hippNS_massA)
nrow(biomasses_hippNS_massA_tronque)

############### Water mass B
biomasses_hippNS_massB$Tot_Pico <- biomasses_hippNS_massB$Tot_Nano <- NULL
biomasses_hippNS_massB$full_FLR <- biomasses_hippNS_massB$full_FWS <- biomasses_hippNS_massB$Par <- NULL
summary(biomasses_hippNS_massB$Lat)
summary(biomasses_hippNS_massB$Lon)

## Caracterisation of water masses according to salinity criteria
biomasses_hippNS_massB_tronque <- biomasses_hippNS_massB[which(biomasses_hippNS_massB$Sal <= 37.3), ]
nrow(biomasses_hippNS_massB)
nrow(biomasses_hippNS_massB_tronque)

########## Summary of the new datasets:
summary(biomasses_hippNS_massA$Lat)
summary(biomasses_hippNS_massA_tronque)
summary(biomasses_hippNS_massB$Lat)
summary(biomasses_hippNS_massB_tronque)

sort(biomasses_hippNS_massA_tronque$Time)
sort(biomasses_hippNS_massB_tronque$Time)

## Definition of front data using latitudinal criteria and salinity
#Front from A
front_massA <- biomasses_hippNS_massA[which((biomasses_hippNS_massA$Lat > 38.5) & (biomasses_hippNS_massA$Lat < 38.6) &
                                              (biomasses_hippNS_massA$Sal <= 37.6) & (biomasses_hippNS_massA$Sal >= 37.3)), ]
#Transitional waters from A
entredeuxeauxA <- biomasses_hippNS_massA[which((biomasses_hippNS_massA$Lat >= 38.6) &
                                                 (biomasses_hippNS_massA$Sal <= 37.6) & (biomasses_hippNS_massA$Sal >= 37.3)), ]

#Front from B
front_massB <- biomasses_hippNS_massB[which((biomasses_hippNS_massB$Lat > 38.5) & (biomasses_hippNS_massB$Lat < 38.6) &
                                              (biomasses_hippNS_massB$Sal <= 37.6) & (biomasses_hippNS_massB$Sal >= 37.3)), ]
#Transitional waters from B
entredeuxeauxB <- biomasses_hippNS_massB[which((biomasses_hippNS_massB$Lat <= 38.5) &
                                                 (biomasses_hippNS_massB$Sal <= 37.6) & (biomasses_hippNS_massB$Sal >= 37.3)), ]

#combining front data
front <- rbind(front_massA, front_massB)
front <- front[order(front$Time), ]
sort(front$Time)
nrow(front)
nrow(front) + nrow(biomasses_hippNS_massA_tronque) + nrow(biomasses_hippNS_massB_tronque)

#combining transitional waters data
front_EDA<-rbind(entredeuxeauxA, entredeuxeauxB)
front_EDA <- front_EDA[order(front_EDA$Time), ]
sort(front_EDA$Time)
nrow(front_EDA)

## Salinite:

#Figure A1
#jpeg("FigureA1.jpg", width = 800, height = 800)
par(mgp=c(3,0.5,0),mar=c(6,6,6,2.1))
graphics::plot(x = biomasses_hippNS_massA_tronque$Time, y = biomasses_hippNS_massA_tronque$Sal,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = cbbPalette[2],pch=16,
               ylim = c(36.9,37.9),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)
graphics::plot(x = front$Time, y = front$Sal,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = cbbPalette[5],pch=16,
               ylim = c(36.9,37.9),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)
graphics::plot(x = front_EDA$Time, y = front_EDA$Sal,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = "grey",pch=3,
               ylim = c(36.9,37.9),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)

graphics::plot(x = biomasses_hippNS_massB_tronque$Time, y = biomasses_hippNS_massB_tronque$Sal,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "Hour",
               type = "p", 
               ylab = expression(paste("Salinity (",g,phantom(0),kg^{-1},")")),
               col = cbbPalette[4],pch=16,
               ylim = c(36.9,37.9),cex=2,cex.axis=1.5,cex.lab=2)
axis.POSIXct(side = 1, at = x_axis, 
             labels = format(x_axis, "%H:%M"),srt=45, cex.axis = 1)
abline(h=c(37.6,37.3))
#dev.off()

## Temperature:

x_axis <- sort(c(biomasses_hippNS_massA_tronque$Time, front$Time, biomasses_hippNS_massB_tronque$Time))
graphics::plot(x = biomasses_hippNS_massA_tronque$Time, y = biomasses_hippNS_massA_tronque$Temp,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = cbbPalette[2],pch=16,
               ylim = c(17.5,19.5),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)
graphics::plot(x = front$Time, y = front$Temp,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = cbbPalette[5],pch=16,
               ylim = c(17.5,19.5),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)

graphics::plot(x = front_EDA$Time, y = front_EDA$Temp,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "",
               type = "p", ylab = "",
               col = "grey",pch=3,
               ylim = c(17.5,19.5),cex=2,cex.axis=1.5,cex.lab=2)
par(new = TRUE)

graphics::plot(x = biomasses_hippNS_massB_tronque$Time, y = biomasses_hippNS_massB_tronque$Temp,
               xlim = c(min(x_axis), max(x_axis)),
               xaxt = "n", xlab = "Date",
               type = "p", ylab = "Temperature",
               col = cbbPalette[3],pch=16,
               ylim = c(17.5,19.5),cex=2,cex.axis=1.5,cex.lab=2)
axis.POSIXct(side = 1, at = x_axis, 
             labels = format(x_axis, "%H:%M"),srt=45, cex.axis = 1)

#We simplify the name of the datasets
biom_A<-biomasses_hippNS_massA_tronque
biom_B<-biomasses_hippNS_massB_tronque
biom_C<-front
biom_T<-front_EDA



##-------------------- Exploratory statistics ---------------------##

############### Mass A
## Calcul of observed proportions
biomasses_tot_hippNS_massA_tronque <- apply(biomasses_hippNS_massA_tronque[ ,2:10], 1, sum)
biomasses_hippNS_massA_tronque$Syne_prop <- biomasses_hippNS_massA_tronque[ ,2] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$Crypto_prop <- biomasses_hippNS_massA_tronque[ ,3] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$Pico1_prop <- biomasses_hippNS_massA_tronque[ ,4] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$Pico2_prop <- biomasses_hippNS_massA_tronque[ ,5] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$Pico3_prop <- biomasses_hippNS_massA_tronque[ ,6] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$SNano_prop <- biomasses_hippNS_massA_tronque[ ,7] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$RNano_prop <- biomasses_hippNS_massA_tronque[ ,8] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$Micro_prop <- biomasses_hippNS_massA_tronque[ ,9] / biomasses_tot_hippNS_massA_tronque
biomasses_hippNS_massA_tronque$PicoHFLR_prop <- biomasses_hippNS_massA_tronque[ ,10] / biomasses_tot_hippNS_massA_tronque

biomasses_hippNS_massA_tronque$Syne <- biomasses_hippNS_massA_tronque$Crypto <- biomasses_hippNS_massA_tronque$Pico1 <- 
  biomasses_hippNS_massA_tronque$Pico2 <- biomasses_hippNS_massA_tronque$Pico3 <- biomasses_hippNS_massA_tronque$SNano <-
  biomasses_hippNS_massA_tronque$RNano <- biomasses_hippNS_massA_tronque$Micro <- biomasses_hippNS_massA_tronque$PicoHFLR <- NULL

############# Masse d'eau B
## Calcul of observed proportions
biomasses_tot_hippNS_massB_tronque <- apply(biomasses_hippNS_massB_tronque[ ,2:10], 1, sum)
biomasses_hippNS_massB_tronque$Syne_prop <- biomasses_hippNS_massB_tronque[ ,2] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$Crypto_prop <- biomasses_hippNS_massB_tronque[ ,3] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$Pico1_prop <- biomasses_hippNS_massB_tronque[ ,4] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$Pico2_prop <- biomasses_hippNS_massB_tronque[ ,5] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$Pico3_prop <- biomasses_hippNS_massB_tronque[ ,6] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$SNano_prop <- biomasses_hippNS_massB_tronque[ ,7] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$RNano_prop <- biomasses_hippNS_massB_tronque[ ,8] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$Micro_prop <- biomasses_hippNS_massB_tronque[ ,9] / biomasses_tot_hippNS_massB_tronque
biomasses_hippNS_massB_tronque$PicoHFLR_prop <- biomasses_hippNS_massB_tronque[ ,10] / biomasses_tot_hippNS_massB_tronque

biomasses_hippNS_massB_tronque$Syne <- biomasses_hippNS_massB_tronque$Crypto <- biomasses_hippNS_massB_tronque$Pico1 <- 
  biomasses_hippNS_massB_tronque$Pico2 <- biomasses_hippNS_massB_tronque$Pico3 <- biomasses_hippNS_massB_tronque$SNano <-
  biomasses_hippNS_massB_tronque$RNano <- biomasses_hippNS_massB_tronque$Micro <- biomasses_hippNS_massB_tronque$PicoHFLR <- NULL


############ Front 
## Calcul of observed proportions
front_tot_hippNS <- apply(front[ ,2:10], 1, sum)
front$Syne_prop <- front[ ,2] / front_tot_hippNS
front$Crypto_prop <- front[ ,3] / front_tot_hippNS
front$Pico1_prop <- front[ ,4] / front_tot_hippNS
front$Pico2_prop <- front[ ,5] / front_tot_hippNS
front$Pico3_prop <- front[ ,6] / front_tot_hippNS
front$SNano_prop <- front[ ,7] / front_tot_hippNS
front$RNano_prop <- front[ ,8] / front_tot_hippNS
front$Micro_prop <- front[ ,9] / front_tot_hippNS
front$PicoHFLR_prop <- front[ ,10] / front_tot_hippNS
front$Syne <- front$Crypto <- front$Pico1 <- front$Pico2 <- front$Pico3 <- front$SNano <- front$RNano <- front$Micro <- front$PicoHFLR <- NULL


############ Transitional waters
## Calcul of observed proportions
front_EDA_tot_hippNS <- apply(front_EDA[ ,2:10], 1, sum)
front_EDA$Syne_prop <- front_EDA[ ,2] / front_EDA_tot_hippNS
front_EDA$Crypto_prop <- front_EDA[ ,3] / front_EDA_tot_hippNS
front_EDA$Pico1_prop <- front_EDA[ ,4] / front_EDA_tot_hippNS
front_EDA$Pico2_prop <- front_EDA[ ,5] / front_EDA_tot_hippNS
front_EDA$Pico3_prop <- front_EDA[ ,6] / front_EDA_tot_hippNS
front_EDA$SNano_prop <- front_EDA[ ,7] / front_EDA_tot_hippNS
front_EDA$RNano_prop <- front_EDA[ ,8] / front_EDA_tot_hippNS
front_EDA$Micro_prop <- front_EDA[ ,9] / front_EDA_tot_hippNS
front_EDA$PicoHFLR_prop <- front_EDA[ ,10] / front_EDA_tot_hippNS
front_EDA$Syne <- front_EDA$Crypto <- front_EDA$Pico1 <- front_EDA$Pico2 <- front_EDA$Pico3 <- front_EDA$SNano <- front_EDA$RNano <- front_EDA$Micro <- front_EDA$PicoHFLR <- NULL


############# Summary 
summary(biomasses_hippNS_massA_tronque)
summary(front)
summary(biomasses_hippNS_massB_tronque)

## Average:
(moyA <- sapply(biomasses_hippNS_massA_tronque[ ,6:14], FUN = mean))
(sdA <- sapply(biomasses_hippNS_massA_tronque[ ,6:14], FUN = sd))
(moyB <- sapply(biomasses_hippNS_massB_tronque[ ,6:14], FUN = mean))
(sdB <- sapply(biomasses_hippNS_massB_tronque[ ,6:14], FUN = sd))
(moy_front <- sapply(front[ ,6:14], FUN = mean))
## Standard-deviation:
sapply(biomasses_hippNS_massA_tronque[ ,6:14], FUN = sd)
sapply(biomasses_hippNS_massB_tronque[ ,6:14], FUN = sd)
sapply(front[ ,6:14], FUN = sd)

## We modify the structure of the dataset before plotting:

data_all_sfront<-rbind(biom_A,biom_B,biom_C)
data_all_sfront$Mass<-c(rep("A",nrow(biom_A)),
                        rep("B",nrow(biom_B)),
                        rep("F",nrow(biom_C)))
data_all_sfront_g<-gather(data_all_sfront,key="OTU",value="biomass", 2:10)
data_all_sfront_g$Mass<-factor(data_all_sfront_g$Mass, levels=c('A', 'F', 'B'))
data_all_sfront_g$OTU<-sub("_.*", "", data_all_sfront_g$OTU)   
data_all_sfront_g$OTU<-factor(data_all_sfront_g$OTU, levels=sub("_.*", "", colnames(data_all_sfront)[2:10]))

###### We do the same but with abundance data
abondance_hippNS_massA <- read.csv(file = "data/cyto_NS_waterA.csv",
                                   header = TRUE,
                                   sep = ",",
                                   quote = "\"",
                                   dec = ".")
abondance_hippNS_massA <- dplyr::mutate(abondance_hippNS_massA, 
                                        Time = as.POSIXct(abondance_hippNS_massA$Time,
                                                          format = "%Y-%m-%d %H:%M",
                                                          tz = "GMT"))
abondance_hippNS_massB <- read.csv(file = "data/cyto_NS_waterB.csv",
                                   header = TRUE,
                                   sep = ",",
                                   quote = "\"",
                                   dec = ".")
abondance_hippNS_massB <- dplyr::mutate(abondance_hippNS_massB, 
                                        Time = as.POSIXct(abondance_hippNS_massB$Time,
                                                          format = "%Y-%m-%d %H:%M",
                                                          tz = "GMT"))
abondance_hippNS_massA$Tot_Pico <- abondance_hippNS_massA$Tot_Nano <- NULL
abondance_hippNS_massA$full_FLR <- abondance_hippNS_massA$full_FWS <- abondance_hippNS_massA$Par <- NULL
abondance_hippNS_massA_tronque <- abondance_hippNS_massA[which(abondance_hippNS_massA$Sal >= 37.6), ]
abondance_hippNS_massB$Tot_Pico <- abondance_hippNS_massB$Tot_Nano <- NULL
abondance_hippNS_massB$full_FLR <- abondance_hippNS_massB$full_FWS <- abondance_hippNS_massB$Par <- NULL
abondance_hippNS_massB_tronque  <- abondance_hippNS_massB[which(abondance_hippNS_massB$Sal <= 37.3), ]

front_ab_massA <- abondance_hippNS_massA[which((abondance_hippNS_massA$Lat > 38.5) & (abondance_hippNS_massA$Lat < 38.6) &
                                                 (abondance_hippNS_massA$Sal <= 37.6) & (abondance_hippNS_massA$Sal >= 37.3)), ]
front_ab_massB <- abondance_hippNS_massB[which((abondance_hippNS_massB$Lat > 38.5) & (abondance_hippNS_massB$Lat < 38.6) &
                                                 (abondance_hippNS_massB$Sal <= 37.6) & (abondance_hippNS_massB$Sal >= 37.3)), ]

front_ab <- rbind(front_ab_massA, front_ab_massB)
front_ab <- front_ab[order(front_ab$Time), ]


ab_all_sfront<-rbind(abondance_hippNS_massA_tronque ,abondance_hippNS_massB_tronque ,front_ab)
ab_all_sfront$Mass<-c(rep("A",nrow(abondance_hippNS_massA_tronque)),
                      rep("B",nrow(abondance_hippNS_massB_tronque)),
                      rep("F",nrow(front_ab)))
ab_all_sfront_g<-gather(ab_all_sfront,key="OTU",value="biomass", 2:10)
ab_all_sfront_g$Mass<-factor(ab_all_sfront_g$Mass, levels=c('A', 'F', 'B'))
ab_all_sfront_g$OTU<-sub("_.*", "", ab_all_sfront_g$OTU)   
ab_all_sfront_g$OTU<-factor(ab_all_sfront_g$OTU, levels=sub("_.*", "", colnames(data_all_sfront)[6:14]))



## Code for figure A1 and A2 


p1<-ggplot(data_all_sfront_g, aes(x=Mass, y=biomass, fill=Mass)) +
  geom_violin()+ylab("Biomass")+xlab("Phytoplankton Functional Types")+ 
  scale_fill_manual(values=c(cbbPalette[2], cbbPalette[5], cbbPalette[3]))+
  facet_wrap(~OTU,scale="free")+ 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1),
        text=element_text(size=20,colour="black"),axis.text=element_text(size=20),
        axis.line=element_line(colour="black"),panel.grid=element_line(colour="darkgrey"),
        axis.ticks=element_line(colour="black"),legend.position="none")



p2<-ggplot(ab_all_sfront_g, aes(x=Mass, y=biomass, fill=Mass)) +
  geom_violin()+ylab("Abundances")+xlab("Phytoplankton Functional Types")+ 
  scale_fill_manual(values=c(cbbPalette[2], cbbPalette[5], cbbPalette[3]))+
  facet_wrap(~OTU,scale="free")+ 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1),
        text=element_text(size=20,colour="black"),axis.text=element_text(size=20),
        axis.line=element_line(colour="black"),panel.grid=element_line(colour="darkgrey"),
        axis.ticks=element_line(colour="black"),legend.position="none")

#print the Figure A1
#jpeg("FigureA1.jpg", width = 800, height = 800)
p2
#dev.off()

#print the Figure A2
#jpeg("FigureA2.jpg", width = 800, height = 800)
p1
#dev.off()

########################################################################
###-------------------- Clustering with MCLUST ---------------------###
########################################################################
library(mclust)# Package = Gaussian mixture estimation with EM algorihtm

### Step 1: We investigate the number of components in water mass A and water mass B directly in the dataset.
mc_A<-mclustICL(biom_A[,2:10],G=1:10) 
mc_B<-mclustICL(biom_B[,2:10],G=1:10)

### Step 2: For component C we use the remainder of the campaign 

date_ABF<-c(biom_C$Time,biom_A$Time, biom_B$Time)

biomasses_world<-biomasses_world[-which(biomasses_world$Time %in% date_ABF),] 
mc_world<-mclustICL(biomasses_world[,2:10],G=1:20)

#Print Figure 2
#jpeg("Figure2.jpg", width = 1600, height = 800)
layout(matrix(c(1:3), ncol = 3,byrow=T))
par(mar=c(10,10,10,10),mgp=c(5,3,0))
plot(apply(as.matrix(mc_A),1,function(x){mean(x,na.rm=T)}),col="purple",lwd=4,type="b", 
     ylab="", xlab="",cex.axis=4,cex.main=5,
     main="(a)                                            ")
abline(v=1,col="darkorange",lwd=4)
mtext("Mean ICL", side=2, line=7, cex=3)
mtext("Number of components", side=1, line=7, cex=3)

plot(apply(as.matrix(mc_B),1,function(x){mean(x,na.rm=T)}),col="purple",lwd=4,type="b", 
     ylab="", xlab="",cex.axis=4,cex.main=5,
     main="(b)                                           ")
abline(v=2,col="darkorange",lwd=4)
mtext("Mean ICL", side=2, line=7, cex=3)
mtext("Number of components", side=1, line=7, cex=3)

plot(apply(as.matrix(mc_world),1,function(x){mean(x,na.rm=T)}),col="purple",lwd=4,type="b", 
     ylab="", xlab="",cex.axis=4,cex.main=5,
     main="(c)                                            ")
abline(v=12,col="darkorange",lwd=4)
mtext("Mean ICL", side=2, line=7, cex=3)
mtext("Number of components", side=1, line=7, cex=3)
#dev.off()

#We select 1 component in A
mc_A<-Mclust(biom_A[,2:10],G=1)

#A visaulization of the parameters of A
par(mfrow=c(2,2))
plot(mc_A$parameters$mean[,1],type="b",ylim=c(0,max(mc_A$parameters$mean)),
     ylab="Mu_A",pch=16,xaxt = "n")
axis(side=1,at=c(1:9),labels= names(mc_A$parameters$mean[,1]))
diagmatA<-cbind(diag(mc_A$parameters$variance$sigma[,,1]))
plot(diagmatA[,1],type="b",ylim=c(0,max(diagmatA)),
     ylab="Diagonal Sigma_A",pch=16,xaxt = "n")
axis(side=1,at=c(1:9),labels= rownames(diagmatA))
max_which<-function(x){which(x==max(x))}
cluster_A<-apply(mc_A$z,1,max_which)
plot(biom_A[,13:14],col=cluster_A,pch=16)
plot(biom_A[,11:12],col=cluster_A,pch=16)

 
library(MASS)
#We use this function to compute densities of component A
A1<-mvrnorm(100000,mc_A$parameters$mean[,1],
            mc_A$parameters$variance$sigma[,,1])
MixA<-mc_A$parameters$pro[1]*A1


#Figure 3A
#jpeg("Figure3A.jpg", width = 1600, height = 1600)
par(mfrow=c(3,3),mar=c(10,10,10,10),mgp=c(5,3,0))
for( i in 1:9){
  histo<-hist(biom_A[,1+i],freq=F,plot=F)
  densA1<-density(MixA[,i],from=min(histo$breaks), to=max(histo$breaks))
  x<-c(density(MixA[,i])$x, density(biom_A[,1+i])$x)
  
  ylimit<-c(0, max(c(densA1$y,histo$density)))
  
  hist(biom_A[,1+i],main=colnames(biom_A[,2:10])[i],xlab="",ylab="",
       xlim=c(min(x),max(x)),ylim=ylimit,freq=F,cex.axis=4,cex.main=4)
  lines(density(MixA[,i]),col=cbbPalette[2],lwd=6)
  mtext("Density", side=2, line=7, cex=3)
  mtext(expression(paste("Biomass (", mmolC, phantom(0), m^{-3},")")), side=1, line=8, cex=3)
}
#dev.off()




#For B we select two components
mc_B<-Mclust(biom_B[,2:10],G=2)

#A visaulization of the parameters of B1 and B2
par(mfrow=c(2,2))
plot(mc_B$parameters$mean[,1],type="b",ylim=c(0,max(mc_B$parameters$mean)),
     ylab="Mu_B",pch=16,xaxt = "n")
axis(side=1,at=c(1:9),labels= names(mc_B$parameters$mean[,1]))
lines(mc_B$parameters$mean[,2],type="b",col=palette()[2],pch=16)
diagmatB<-cbind(diag(mc_B$parameters$variance$sigma[,,1]),
                diag(mc_B$parameters$variance$sigma[,,2]))
plot(diagmatB[,1],type="b",ylim=c(0,max(diagmatB)),
     ylab="Diagonal Sigma_B",pch=16,xaxt = "n")
axis(side=1,at=c(1:9),labels= rownames(diagmatB))
lines(diagmatB[,2],type="b",col=palette()[2],pch=16)
cluster_B<-apply(mc_B$z,1,max_which)
plot(biom_B[,13:14],col=cluster_B,pch=16)
plot(biom_B[,11:12],col=cluster_B,pch=16)


mc_B$parameters$pro #probability if mixture

#We use this function to compute densities of components in B
B1<-mvrnorm(100000,mc_B$parameters$mean[,1],
            mc_B$parameters$variance$sigma[,,1])
B2<-mvrnorm(100000,mc_B$parameters$mean[,2],
            mc_B$parameters$variance$sigma[,,2])
MixB<-(mc_B$parameters$pro[1]*B1)+(mc_B$parameters$pro[2]*B2)



#Figure 3B
jpeg("Figure3B.jpg", width = 1600, height = 1600)
par(mfrow=c(3,3),mar=c(10,10,10,10),mgp=c(5,3,0))
for( i in 1:9){
  histo<-hist(biom_B[,1+i],freq=F,plot=F)
  
  dens1<-density(B1[,i],from=min(0), to=max(histo$breaks))
  dens2<-density(B2[,i],from=min(0), to=max(histo$breaks))
  dens_sum<-(mc_B$parameters$pro[1]*dens1$y) + (mc_B$parameters$pro[2]*dens2$y)
  ylimit<-c(0, max(c(dens_sum,histo$density)))
  xlimit<-c(0, max(histo$breaks))
  
  hist(biom_B[,1+i],main=colnames(biom_B[,2:10])[i],
       xlab="",ylab="",freq=F,ylim=ylimit,cex.axis=4,cex.main=4,xlim=xlimit)
  lines((mc_B$parameters$pro[1]*dens1$y)~
          dens1$x,col=cbbPalette[3],lwd=6)
  lines((mc_B$parameters$pro[2]*dens2$y)
        ~dens2$x,col=cbbPalette[4],lwd=6)
  lines(dens1$x,dens_sum ,
        col="black",lwd=6)
  mtext("Density", side=2, line=7, cex=3)
  mtext(expression(paste("Biomass (", mmolC, phantom(0), m^{-3},")")), side=1, line=8, cex=3)
}
#dev.off()


#Now we select 12 component in the outside dataset
mc_world<-Mclust(biomasses_world[,2:10],G=12)

#Plot of the mu values of 12 the candidates communities
par(mfrow=c(3,4))
for(i in 1:12){
  plot(mc_world$parameters$mean[,i],ylim=c(0,max(mc_world$parameters$mean)),type="b",pch=16,#col=palette()[i],
       ylab="Mu_W",xaxt = "n",main=paste0("C'",i),xlab="")
  axis(side=1,at=c(1:9),labels= names(mc_world$parameters$mean[,1]),cex.axis=0.5,las=2)
  
}

#Plot of the variances of 12 the candidates communities
par(mfrow=c(3,4))
for(i in 1:12){
  plot(diag(mc_world$parameters$variance$sigma[,,i]),ylim=,type="b",pch=16,#col=palette()[i],
       ylab="Diag Sigma_W",xaxt = "n",main=paste0("C'",i),xlab="PFG")
  axis(side=1,at=c(1:9),labels= names(mc_world$parameters$mean[,1]))
  
}


par(mfrow=c(1,2))
cluster_world<-apply(mc_world$z,1,max_which)
cluster_world[-which(cluster_world%in% c(8,10))]<-1
cluster_world<-factor(cluster_world)

colors<-c("grey",cbbPalette[5],cbbPalette[6])

decimal_to_dms <- function(decimal_degrees) {
  degrees <- floor(decimal_degrees)
  minutes <- floor((decimal_degrees - degrees) * 60)
  return(sprintf("%d° %d'", degrees, minutes))
}

#Figure A6
#jpeg("FigureA6.jpg", width = 1000, height = 1000)
par(mfrow=c(1,2),mar=c(10,10,6,2),mgp=c(3,3,0))
plot(biomasses_world[,13:14],
     col=colors[as.numeric(cluster_world)],
     xlab="",ylab="",
     pch=16,cex=2,cex.axis=3,cex.main=3,
     main="(a)                                 ",
     xaxt="n", yaxt="n")
lat_values <- pretty(biomasses_world[,14]) 
lat_labels <- sapply(lat_values, decimal_to_dms)
axis(2, at=lat_values, labels=paste0(lat_labels,"N"), las=0, cex.axis=3)
lon_values <- pretty(biomasses_world[,13])
lon_labels <- sapply(lon_values, decimal_to_dms)
axis(1, at=lon_values, labels=paste0(lon_labels,"E"), las=1, cex.axis=3)

mtext("Latitude", side=2, line=7, cex=3)
mtext("Longitude", side=1, line=7, cex=3)
points(rbind(biom_A,biom_B,biom_C,biom_T)[,13:14],
       col="black",cex=2,pch=16)

plot(biomasses_world[,11:12],
     col=colors[as.numeric(cluster_world)],pch=16,
     xlim=c(36.8,37.9),ylim=c(15.9,19.3),
     ylab="",xlab="",cex=2,cex.axis=3,cex.main=3,
     main="(b)                                  ") 
mtext("Temperature", side=2, line=7, cex=3)
mtext("Salinity", side=1, line=7, cex=3)
points(rbind(biom_A,biom_B,biom_C,biom_T)[,11:12],
       col="black",pch=16,cex=2)
#dev.off()


######## We gather and export the tables of the parameters
mu_table<-data.frame(round(cbind(mc_A$parameters$mean,
                                 mc_B$parameters$mean,
                                 mc_world$parameters$mean),2))

colnames(mu_table)<-c("A","B1","B2",paste0("C",1:12))
mu_table<-mu_table[,c("A","B1","B2","C8","C10")]


write.csv(mu_table,"mu_table.csv", row.names = T)

write.csv(formatC(mc_A$parameters$variance$sigma[,,1], format = "e", digits = 2),
          "covA_table.csv", row.names = T)
write.csv(formatC(mc_B$parameters$variance$sigma[,,1], format = "e", digits = 2),
          "covB1_table.csv", row.names = T)
write.csv(formatC(mc_B$parameters$variance$sigma[,,2], format = "e", digits = 2),
          "covB2_table.csv", row.names = T)
write.csv(formatC(mc_world$parameters$variance$sigma[,,8],format = "e", digits = 2),
          "covC8_table.csv", row.names = T)
write.csv(formatC(mc_world$parameters$variance$sigma[,,10],format = "e", digits = 2),
          "covC10_table.csv", row.names = T)


######################################################
############### Bayesian modelling ###################
######################################################
library(tidyverse)
library(rstan) 
library(bayesplot)
library(loo)
library(LaplacesDemon)
options(mc.cores = parallel::detectCores()) 
color_scheme_set("viridisC")

#Step 3: We load the data as a list for the exploratory model 
stan_data<-list(
  N=dim(biom_C[,2:10])[1],# Number of observations in F
  C=dim(biom_C[,2:10])[2],# Numbre of Phytoplankton groups
  K=3, # Number of components in mixture
  y=biom_C[,2:10], # Observations (table N x C)
  
  K_A=mc_A$G, # Number of components in A    
  alpha_A=mc_A$parameters$pro, #Vector of alpha A (EM)
  mu_A=t(mc_A$parameters$mean),# Mu values of A
  sigma_A=mc_A$parameters$variance$sigma, # Sigma matrix of A
  
  K_B=mc_B$G, # Number of components in B    
  alpha_B=mc_B$parameters$pro, #Vector of alpha B (EM)
  mu_B=t(mc_B$parameters$mean),# Mu values of B
  sigma_B=mc_B$parameters$variance$sigma, # Sigma matrix of B
  
  K_C=mc_world$G, # Number of components in C
  mu_C=matrix(t(mc_world$parameters$mean), nrow = mc_world$G),  #Mu values of C (EM)
  sigma_C=array(mc_world$parameters$variance$sigma,dim=c(9,9,mc_world$G)) # Sigma matrix of C
)

#Run the model in STAN
fit_DMDM<- stan(file = "Discretemixtureofdiscretemixture.stan", 
                data = stan_data,
                iter = 11000, warmup=10000, chains = 4,
                control = list(adapt_delta = 0.95, 
                               max_treedepth = 15)  
) 
print(fit_DMDM)

vec_par<-as.matrix(fit_DMDM)[,2:16]%>%colnames()

#Figure A4
#jpeg("FigureA4.jpg", width = 1000, height = 1000)
fit_DMDM%>%mcmc_trace(pars=vec_par)+ 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1),
        text=element_text(size=30,colour="black"),axis.text=element_text(size=30),
        panel.grid=element_line(colour="darkgrey"),
        axis.ticks=element_line(colour="black"),legend.key.size = unit(2, 'cm'),
        legend.text=element_text(size=30))
#dev.off()

#We see that component C'8 and C'10 have the highest weight
par(mfrow=c(1,2))
boxplot(as.matrix(fit_DMDM)[seq(1,4000,by=20),order(apply(as.matrix(fit_DMDM)[,2:13],2,mean))+1], 
        ylab="Weight", xlab="Alpha_C", 
        names=as.factor(c(1:12)[order(apply(as.matrix(fit_DMDM)[,2:13],2,mean))]))
boxplot(as.matrix(fit_DMDM)[seq(1,4000,by=20),14:16],ylab="Weight", xlab="Lambda", names=c("A","B","C"))


#Step 4 : We load the data as a list for the final model 
stan_data_2<-list(
  N=dim(biom_C[,2:10])[1],# Number of observations in F
  C=dim(biom_C[,2:10])[2],# Numbre of Phytoplankton groups
  K=3, # Number of components in mixture
  y=biom_C[,2:10], # Observations (table N x C)
  
  K_A=mc_A$G, # Number of components in A    
  alpha_A=mc_A$parameters$pro, #Vector of alpha A (EM)
  mu_A=t(mc_A$parameters$mean),# Mu values of A
  sigma_A=mc_A$parameters$variance$sigma, # Sigma matrix of A
  
  K_B=mc_B$G, # Number of components in B    
  alpha_B=mc_B$parameters$pro, #Vector of alpha B (EM)
  mu_B=t(mc_B$parameters$mean),# Mu values of B
  sigma_B=mc_B$parameters$variance$sigma, # Sigma matrix of B

  K_C=2,# Number of components in C    
  mu_C=matrix(t(mc_world$parameters$mean)[c(8,10),], nrow = 2), # Mu values of C
  sigma_C=array(mc_world$parameters$variance$sigma[,,c(8,10)],dim=c(9,9,2))  # Sigma matrix of C
  
)
fit_DMDM2<- stan(file = "Discretemixtureofdiscretemixture.stan", 
                 data = stan_data_2,
                 iter = 11000, warmup=10000, chains = 4,
                 control = list(adapt_delta = 0.95, 
                                max_treedepth = 15)  
)
res_mat_2<-fit_DMDM2%>%as.matrix() 
vec_par_2<-colnames(res_mat_2)[2:6]

#Figure A5
#jpeg("FigureA5.jpg", width = 1000, height = 1000)
fit_DMDM2%>%mcmc_trace(pars=vec_par_2)+ 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1),
        text=element_text(size=30,colour="black"),axis.text=element_text(size=30),
        panel.grid=element_line(colour="darkgrey"),
        axis.ticks=element_line(colour="black"),legend.key.size = unit(2, 'cm'),
        legend.text=element_text(size=30))
#dev.off()

#Figure 4
#jpeg("Figure4.jpg", width = 1600, height = 1600)
par(mfrow=c(2,2),mar=c(10,10,10,10),mgp=c(5,3,0))


id_lambda<-grep("lambda",colnames(as.matrix(fit_DMDM)))[c(1,3,2)]
boxplot(as.matrix(fit_DMDM)[seq(1,4000,by=20),id_lambda],
        ylab="", xlab="", names=c("A","C","B"),cex.axis=4,cex.main=5,
        col=cbbPalette[c(2,5,3)],
        main="(a)                                           ")
mtext("Weight", side=2, line=7, cex=3)
mtext(expression(lambda), side=3, line=2, cex=5, at=mean(par("usr")[1:2]))

id_alpha<-grep("alpha_C",colnames(as.matrix(fit_DMDM)))
boxplot(as.matrix(fit_DMDM)[seq(1,4000,by=20),id_alpha], 
        ylab="", xlab="", 
        names=as.factor(c(1:12)),cex.axis=4,cex.main=5,
        main="(b)                                           ")
mtext("Weight", side=2, line=7, cex=3)
mtext(expression(alpha[C]), side=3, line=2, cex=5, at=mean(par("usr")[1:2]))




id_lambda<-grep("lambda",colnames(res_mat_2))[c(1,3,2)]
boxplot(res_mat_2[seq(1,4000,by=20),id_lambda],
        xlab="",
        ylab="", names=c("A","C","B"),
        ylim=c(0,1),cex.axis=4,cex.main=5,
        col=cbbPalette[c(2,5,3)],
        main="(c)                                           ")
mtext("Weight", side=2, line=7, cex=3)
mtext(expression(lambda), side=3, line=2, cex=5, at=mean(par("usr")[1:2]))

id_alpha<-grep("alpha_C",colnames(res_mat_2))
boxplot(res_mat_2[seq(1,4000,by=20),id_alpha],
        xlab="",names=as.factor(c(1,2)),
        ylab="",
        ylim=c(0,1),cex.axis=4,cex.main=5,
        main="(d)                                           ")
mtext("Weight", side=2, line=7, cex=3)
mtext(expression(alpha[C]), side=3, line=2, cex=5, at=mean(par("usr")[1:2]))

#dev.off()



mean_alphaC<-apply(as.matrix(fit_DMDM2)[,2:3],2,mean)

### Calcul of confidence interval of gaussian mixture 
IC_melange_gaussien <- function(m1, sigma1, m2, sigma2, 
                                lambda1, lambda2, n, alpha = 0.05) {
  
  # Vérification que les poids forment un mélange valide
  if (lambda1 + lambda2 != 1) {
    stop("Les poids lambda1 et lambda2 doivent sommer à 1.")
  }
  
  # Calcul de la moyenne du mélange
  mu <- lambda1 * m1 + lambda2 * m2
  
  # Calcul de la covariance du mélange
  sigma <- lambda1 * (sigma1 + outer(m1 - mu, m1 - mu)) +
    lambda2 * (sigma2 + outer(m2 - mu, m2 - mu))
  
  # Quantile de la loi normale pour le niveau de confiance
  z_alpha <- qnorm(1 - alpha / 2)  # Par défaut 1.96 pour 95%
  
  # Calcul des intervalles de confiance
  ic_lower <- mu - z_alpha * sqrt(diag(sigma) / n)
  ic_upper <- mu + z_alpha * sqrt(diag(sigma) / n)
  
  # Résultat sous forme de tableau
  intervals <- data.frame(
    Mu = mu,
    Lower_Bound = apply(cbind(ic_lower,ic_upper),1,min),
    Upper_Bound = apply(cbind(ic_lower,ic_upper),1,max)
  )
  
  return(intervals)
}

#CI with only one gaussian component 
CI_gaussian<-function(mu,sigma,n,alpha=0.05){
  z_alpha <- qnorm(1 - alpha / 2)  # Par défaut 1.96 pour 95%
  
  # Calcul des intervalles de confiance
  ic_lower <- mu - z_alpha * sqrt(diag(sigma) / n)
  ic_upper <- mu + z_alpha * sqrt(diag(sigma) / n)
  
  # Résultat sous forme de tableau
  intervals <- data.frame(
    Mu = mu,
    Lower_Bound = apply(cbind(ic_lower,ic_upper),1,min),
    Upper_Bound = apply(cbind(ic_lower,ic_upper),1,max)
  )
  
  return(intervals)
  
}

ComA_IC<-CI_gaussian(mc_A$parameters$mean,mc_A$parameters$variance$sigma[,,1],
                     mc_A$n)

ComB_IC<-IC_melange_gaussien(mc_B$parameters$mean[,1],mc_B$parameters$variance$sigma[,,1],
                             mc_B$parameters$mean[,2],mc_B$parameters$variance$sigma[,,2],
                             mc_B$parameters$pro[1],mc_B$parameters$pro[2],44)

ComC_IC<-IC_melange_gaussien(mc_world$parameters$mean[,8],mc_world$parameters$variance$sigma[,,8],
                             mc_world$parameters$mean[,10],mc_world$parameters$variance$sigma[,,10],
                             mean_alphaC[1],mean_alphaC[2],11)


ComB1_IC<-CI_gaussian(mc_B$parameters$mean[,1],
                      mc_B$parameters$variance$sigma[,,1],
                      table(mc_B$classification)[1])

ComB2_IC<-CI_gaussian(mc_B$parameters$mean[,2],
                      mc_B$parameters$variance$sigma[,,2],
                      table(mc_B$classification)[2])

ComC1_IC<-CI_gaussian(mc_world$parameters$mean[,8],
                      mc_world$parameters$variance$sigma[,,8],
                      11*mean_alphaC[1])

ComC2_IC<-CI_gaussian(mc_world$parameters$mean[,10],
                      mc_world$parameters$variance$sigma[,,10],
                      11*mean_alphaC[2])

# We prepare the data for fig6a
abrel<-function(x){
  mu_perc<-x[,1]*100/sum(x[,1])
  lower_perc<-x[,2]*100/sum(x[,1])
  upper_perc<-x[,3]*100/sum(x[,1])
  intervals <- data.frame(
    mu =mu_perc,
    Lower_Bound = lower_perc,
    Upper_Bound = upper_perc
  )
  rownames(intervals)<-rownames(x)
  return(intervals)
}
ComA_IC_rel<-abrel(ComA_IC)
ComB_IC_rel<-abrel(ComB_IC)
ComC_IC_rel<-abrel(ComC_IC)

data_pond_mu<-rbind(ComA_IC_rel[,1],
                    ComC_IC_rel[,1],
                    ComB_IC_rel[,1])

data_inf_pond<-rbind(ComA_IC_rel[,2],
                     ComC_IC_rel[,2],
                     ComB_IC_rel[,2])
data_sup_pond<-rbind(ComA_IC_rel[,3],
                     ComC_IC_rel[,3],
                     ComB_IC_rel[,3])

colnames(data_pond_mu)<-rownames(ComA_IC_rel)
colnames(data_inf_pond)<-rownames(ComA_IC_rel)
colnames(data_sup_pond)<-rownames(ComA_IC_rel)

# We prepare the data for fig 6b
ComA_IC_rel<-abrel(ComA_IC)
ComB1_IC_rel<-abrel(ComB1_IC)
ComB2_IC_rel<-abrel(ComB2_IC)
ComC1_IC_rel<-abrel(ComC1_IC)
ComC2_IC_rel<-abrel(ComC2_IC)

data_pond_mu2<-rbind(ComA_IC_rel[,1],
                     ComC1_IC_rel[,1],
                     ComC2_IC_rel[,1],
                     ComB1_IC_rel[,1],
                     ComB2_IC_rel[,1])

data_inf_pond2<-rbind(ComA_IC_rel[,2],
                      ComC1_IC_rel[,2],
                      ComC2_IC_rel[,2],
                      ComB1_IC_rel[,2],
                      ComB2_IC_rel[,2])

data_sup_pond2<-rbind(ComA_IC_rel[,3],
                      ComC1_IC_rel[,3],
                      ComC2_IC_rel[,3],
                      ComB1_IC_rel[,3],
                      ComB2_IC_rel[,3])

colnames(data_pond_mu2)<-rownames(ComA_IC_rel)
colnames(data_inf_pond2)<-rownames(ComA_IC_rel)
colnames(data_sup_pond2)<-rownames(ComA_IC_rel)


#Figure 6
#jpeg("Figure6.jpg", width = 1400, height = 1000)
par(mfrow=c(2,1),mar=c(7,10,5,1),mgp=c(5,1,0))
bar_positions <- barplot(data_pond_mu,col=rep(cbbPalette[c(2,5,3)],9),
                         ylab="Relative biomass (%)",beside=T,cex.axis =2,cex.names =3,
                         cex.lab=3,cex.main=4,xaxt="n",ylim=c(0,max(data_sup_pond)),
                         main="(a)                                                                                                 ")
abline(h=0)
arrows(
  x0 = bar_positions, 
  y0 = data_inf_pond, 
  x1 = bar_positions, 
  y1 = data_sup_pond, 
  angle = 90, 
  code = 3, 
  length = 0.1
)
text(x=colMeans(bar_positions), y=par("usr")[3] - 1, 
     labels=colnames(data_pond_mu), srt=45, adj=1, xpd=TRUE, cex=2)
par(xpd=TRUE)
legend(0,40, legend=c("A","C","B"), bg="transparent",
  fill=cbbPalette[c(2,5,3)],
  ,cex=2,box.lwd = 0,horiz = T)
par(xpd=FALSE)


#fig6b
bar_positions <- barplot(data_pond_mu2,col=rep(cbbPalette[c(2,5,6,3,4)],9),
                         ylab="Relative biomass (%)",beside=T, cex.axis =2,cex.names =3,
                         cex.main=4, cex.lab=3,xaxt="n",ylim=c(0,max(data_sup_pond2)),
                         main="(b)                                                                                                 ")
abline(h=0)
arrows(
  x0 = bar_positions, 
  y0 = data_inf_pond2, 
  x1 = bar_positions, 
  y1 = data_sup_pond2, 
  angle = 90, 
  code = 3, 
  length = 0.1
)
text(x=colMeans(bar_positions), y=par("usr")[3] - 1, 
     labels=colnames(data_pond_mu2), srt=45, adj=1, xpd=TRUE, cex=2)
par(xpd=TRUE)
legend(0,40, legend=c("A1","C1","C2","B1","B2"), bg="transparent",
       fill=cbbPalette[c(2,5:6,3,4)],
       ,cex=2,horiz = T)
#dev.off()



## We calculate the densities of C'8 and C'10
mean_lambda<-apply(as.matrix(fit_DMDM2)[,4:6],2,mean)
C8<-mvrnorm(100000,mc_world$parameters$mean[,8],
            mc_world$parameters$variance$sigma[,,8])
C10<-mvrnorm(100000,mc_world$parameters$mean[,10],
             mc_world$parameters$variance$sigma[,,10])


#jpeg("Figure5.jpg", width = 1400, height = 1000)
par(mfrow=c(3,3),mar=c(10,10,10,10),mgp=c(5,3,0))
for( i in 1:9){
  histo<-hist(biom_C[,1+i],main=colnames(biom_C[,2:10])[i],
              xlab=expression(paste("Biomass (", mmolC.m^{-3},")")),freq=F,plot=F)
  
  densA1<-density(A1[,i],from=0, to=max(histo$breaks))
  densB1<-density(B1[,i],from=0, to=max(histo$breaks))
  densB2<-density(B2[,i],from=0, to=max(histo$breaks))
  densC8<-density(C8[,i],from=0, to=max(histo$breaks))
  densC10<-density(C10[,i],from=0, to=max(histo$breaks))
  
  dens_sum<-densA1$y*mean_lambda[1]+
    ((mc_B$parameters$pro[1]*densB1$y +
        mc_B$parameters$pro[2]*densB2$y)*mean_lambda[2])+
    ((mean_alphaC[1]*densC8$y +
        mean_alphaC[2]*densC10$y)*mean_lambda[3])
  
  ylimit<-c(0, max(c(dens_sum,histo$density)))
  xlimit<-c(0,max(histo$breaks))
  
  
  hist(biom_C[,1+i],main=colnames(biom_C[,2:10])[i],
       xlab="",ylab="",freq=F,xlim=xlimit,ylim=ylimit,cex.axis=4,cex.main=4)
  mtext("Density", side=2, line=7, cex=3)
  mtext(expression(paste("Biomass (", mmolC, phantom(0), m^{-3},")")), side=1, line=8, cex=3)
  
  lines((mean_lambda[1]*densA1$y)~
          densA1$x,col=cbbPalette[2],lwd=6)  
  
  lines((mc_B$parameters$pro[1]*densB1$y)*mean_lambda[2]~
          densB1$x,col=cbbPalette[3],lwd=6)
  lines((mc_B$parameters$pro[2]*densB2$y)*mean_lambda[2]~
          densB2$x,col=cbbPalette[4],lwd=6)
  
  lines((mean_alphaC[1]*densC8$y)*mean_lambda[3]~
          densC8$x,col=cbbPalette[5],lwd=6)
  lines((mean_alphaC[2]*densC10$y)*mean_lambda[3]~
          densC10$x,col=cbbPalette[6],lwd=6)
  
  lines(densA1$x,dens_sum ,
        col="black",lwd=6)
}
#dev.off()


#### We find the dominant component in each observations
full_data<-rbind(biom_A,biom_B,biom_C,biom_T)
A1_T<-dmvnorm(full_data[,2:10], mean = mc_A$parameters$mean,
              sigma=mc_A$parameters$variance$sigma[,,1])
B1_T<-dmvnorm(full_data[,2:10], mean = mc_B$parameters$mean[,1],
              sigma=mc_B$parameters$variance$sigma[,,1])
B2_T<-dmvnorm(full_data[,2:10], mean = mc_B$parameters$mean[,2],
              sigma=mc_B$parameters$variance$sigma[,,2])
C1_T<-dmvnorm(full_data[,2:10], mean = mc_world$parameters$mean[,8],
              sigma=mc_world$parameters$variance$sigma[,,8])
C2_T<-dmvnorm(full_data[,2:10], mean = mc_world$parameters$mean[,10],
              sigma=mc_world$parameters$variance$sigma[,,10])
dt_dun<-matrix(cbind(A1_T,B1_T,B2_T,
                     C1_T,C2_T),
               nrow=length(A1_T))
apply(dt_dun,1,max_which)
clust<-factor(apply(dt_dun,1,max_which))

summary(biom_T[,13:14])
summary(biom_A[,13:14])
summary(biom_B[,13:14])


col_pal<-cbbPalette[-1]


shp<-c(rep(16,dim(biom_A)[1]),
       rep(16,dim(biom_B)[1]),
       rep(16,dim(biom_C)[1]),
       rep(18,dim(biom_T)[1]))

#Figure 7
#jpeg("Figure7.jpg", width = 1000, height = 1000)
par(mfrow=c(1,1), mar=c(10,10,10,20), mgp=c(5,3,0))

plot(full_data[,13:14], col=col_pal[clust], pch=shp, cex=3,
     ylab="", xlab="", cex.axis=3,
     xaxt="n", yaxt="n") 
text(c(3.05,3.05,3.05), c(38.8,38.55,38.3), c("A", "F", "B"), cex=3, font=2)

lat_values <- seq(38,39,by=0.25)
lat_labels <- sapply(lat_values, decimal_to_dms)
axis(2, at=lat_values, labels=paste0(lat_labels,"N"), las=0, cex.axis=3)

lon_values <- seq(3.05,3.35, by=0.1) 
lon_labels <- sapply(lon_values, decimal_to_dms)
axis(1, at=lon_values, labels=paste0(lon_labels,"E"), las=1, cex.axis=3)

abline(h=c(38.5,38.6))
mtext("Latitude", side=2, line=7, cex=3)
mtext("Longitude", side=1, line=7, cex=3)

par(xpd=TRUE)  
legend_x <- 3.3  
legend_y <- 39  

text(legend_x+0.05, legend_y, "Components", cex=3, font=2)
legend(legend_x, legend_y - 0.02, legend=c("A1","B1","B2","C1","C2"), 
       col=1:nlevels(clust), fill=col_pal, cex=3, bty="n")
#dev.off()
