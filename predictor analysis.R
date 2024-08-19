# how predictors affect presence/absence


#Aspect - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$Aspect) #166.7047

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$Aspect[which(toto>=1)]
tata0 <- data.ssom$occ.covs$Aspect[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="Aspect values")
hist(tata1,main="Presence",xlab="Aspect values")

#0 North, 90 east, 180 South, 270 West
mean(tata0) #163.5113 - south-east
mean(tata1) #172.7967 - south east
median(tata0) # 149.2913
median(tata1) # 160.5008
plot(Aspect.1)





#slope - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$slope) #21.91908

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$slope[which(toto>=1)]
tata0 <- data.ssom$occ.covs$slope[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="Aspect values")
hist(tata1,main="Presence",xlab="Aspect values")

#prefer steep slopes
mean(tata0) #21.82952 - south-east
mean(tata1) #22.08993 - south east
median(tata0) # 21.15443
median(tata1) # 20.9753




#elevation - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$elevation) #1728.117

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$elevation[which(toto>=1)]
tata0 <- data.ssom$occ.covs$elevation[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="elevation values")
hist(tata1,main="Presence",xlab="elevation values")

#prefer lower elevations 
mean(tata0) #1749.648
mean(tata1) #1687.041 
median(tata0) # 21.15443
median(tata1) # 20.9753



#C.height - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$Canopy_height) #16.05237

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$Canopy_height[which(toto>=1)]
tata0 <- data.ssom$occ.covs$Canopy_height[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="Canopy_height values")
hist(tata1,main="Presence",xlab="Canopy_height values")

#prefer higher canopies 
mean(tata0) #14.89257 
mean(tata1) #18.26492
median(tata0) # 15.91543
median(tata1) # 19.1239




#EVImax - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$EVIJan_july_max) #0.5944692

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$EVIJan_july_max[which(toto>=1)]
tata0 <- data.ssom$occ.covs$EVIJan_july_max[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="EVIJan_july_max values")
hist(tata1,main="Presence",xlab="EVIJan_july_max values")

#prefer higher canopies 
mean(tata0) #0.603565 
mean(tata1) #0.5771171
median(tata0) # 0.6096148
median(tata1) # 0.5723813



#EVI july-december max - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$EVIjul_dec_max) #0.6370618
#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$EVIjul_dec_max[which(toto>=1)]
tata0 <- data.ssom$occ.covs$EVIjul_dec_max[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="EVIjul_dec_max values")
hist(tata1,main="Presence",xlab="EVIjul_dec_max values")

#prefer higher canopies 
mean(tata0) #0.6581598 
mean(tata1) #0.5968133
median(tata0) # 0.6602599
median(tata1) # 0.5959378


#Precipit wettest quarter - neagtiveffect on occurence of T.Thrush

#tot mean
mean(data.ssom$occ.covs$Precipit_wettest) #166.7047

#mean presence nd absence

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$Precipit_wettest[which(toto>=1)]
tata0 <- data.ssom$occ.covs$Precipit_wettest[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="Precipitation values")
hist(tata1,main="Presence",xlab="Precipitation values")

#90 North, 90 east, 180 South, 270 West
mean(tata0) #542.1188 - south-east
mean(tata1) #529.4425 - south east
median(tata0) # 556.2668
median(tata1) # 517.8613



#mean whole area

#EVI
mean(raster.max_jan_jul_background.df$index_1) #0.30
median(raster.max_jan_jul_background.df$index_1)
hist(raster.max_jan_jul_background.df$index_1)
mean(occ.covs$EVIJan_july_max)
#2 peaks - one in 0 one in 0.3 (south east)
hist(occ.covs$EVIJan_july_max)
#Aspect
mean(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37) #148.82
hist(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37) #2 peaks - one innorth one in 150 (south east)
hist(occ.covs$Aspect)
plot(Aspect.1)

#CH

mean(C.height.df$GLAD_SAFR_30m_UTM37S) #3.19
hist(C.height.df$GLAD_SAFR_30m_UTM37S) #squed towatrd lowe values with mean 3


#Precipit wettest
mean(precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m) #351.4621
hist(precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m) 
hist(occ.covs$Precipit_wettest)
mean(Taita_Data_Sp$Precipit_wettest) #531.19



