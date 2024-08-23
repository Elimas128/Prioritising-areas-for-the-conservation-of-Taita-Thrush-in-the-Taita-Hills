# TRANSFORM the DATA in the right FORMAT for SpOccupancy 
install.packages("spOccupancy")
library(spOccupancy)
library(lubridate)
library(dplyr)
library(sf)
library(raster)
library(sp)


Taita_Data_Sp <- Taita_Data_env
# Remove rows with NA in all covariates and geometry points 
Taita_Data_Sp <- Taita_Data_Sp %>%
  filter(!is.na(geometry) & !is.na(avg_temp) & !is.na(Euclidean_dist_occupied_forest)& !is.na(Precipit_wettest) & !is.na(Precipit_driest) & !is.na(slope) & !is.na(Canopy_height))



#FORMAT for model runnig 
names(Taita_Data_Sp)[1] <- "Plot"

#create replicate column
Taita_Data_Sp$Replicate <- 1
for(i in unique(Taita_Data_Sp$Plot)){
  toto <- Taita_Data_Sp[which(Taita_Data_Sp$Plot==i),]
  j <- 0
  for(jj in 1:nrow(unique(toto[,c(2,5)]))){
    j <- j+1
    toto[which(toto[,2]==unique(toto[,c(2,5)])[j,1] & toto[,5]==unique(toto[,c(2,5)])[j,2]),]$Replicate <- j
  }
  Taita_Data_Sp[which(Taita_Data_Sp$Plot==i),] <- toto
}

#head(Taita_Data_Sp) - changing column names for right format - taken from https://www.jeffdoser.com/files/spoccupancy-web/articles/dataformatting#:~:text=All%20multi%2Dspecies%20occupancy%20model,species%2C%20site%2C%20and%20replicate.

names(Taita_Data_Sp)[5] <- "Time"
names(Taita_Data_Sp)[11] <- "Observer"
names(Taita_Data_Sp)[8] <- "Species"
class(Taita_Data_Sp$Date)
Taita_Data_Sp$Year <- year(Taita_Data_Sp$Date)

str(Taita_Data_Sp)


# Converting srtring "Date" into R object

class(Taita_Data_Sp$Date)

#doenst work - NA's produced 
Taita_Data_Sp$Date <- as.Date(Taita_Data_Sp$Date, "%d/%m/%Y") 
str(Taita_Data_Sp)
class(Taita_Data_Sp$Date)


# Extract the year from the data column
Taita_Data_Sp$Year <- year(Taita_Data_Sp$Date)
str(Taita_Data_Sp)




#EXTRACT DETECTION-NONDETECTION data

y.long <- Taita_Data_Sp %>%
  group_by(Plot, Date, Replicate, Species) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::ungroup() %>%
  pillar::glimpse()
# maybe have to change Replicate from dbl to character 

# convert y.long to 3 dimensional array that contains the site/replicate matrices for each of our 𝑁 species

# Species codes.
sp.codes <- sort(unique(y.long$Species))
# Plot (site) codes.
plot.codes <- sort(unique(y.long$Plot))
# Number of species
N <- length(sp.codes)

#see ho many unique replicates there are 
unique(Taita_Data_Sp$Replicate) #there are 11

# Maximum number of replicates at a site
K <- 11
# Number of sites
J <- length(unique(y.long$Plot))
# Array for detection-nondetection data. 
y <- array(NA, dim = c(N, J, K))
# Label the dimensions of y (not necessary, but helpful)
dimnames(y)[[1]] <- sp.codes
dimnames(y)[[2]] <- plot.codes
# Look at the structure of our array y
str(y)
#contains data for 62 species across 831 sites acrss a possible 5 replicates at each site 

#fill y.long array with data form each species by using nested for loops - 
# assume that if a site is sampled for a given replicate then the observer would have detected at leas one bird, if no bird is detected then the site was not sampled 

for (j in 1:J) { # Loop through sites.
  for (k in 1:K) { # Loop through replicates at each site.
    # Extract data for current site/replicate combination.
    curr.df <- y.long %>%
      filter(Plot == plot.codes[j], Replicate == k)
    # Check if more than one date for a given replicate
    if (n_distinct(curr.df$Date) > 1) {
      # If there is more than 1 date, only use the data
      # from the first date.
      curr.dates <- unique(sort(curr.df$Date))
      curr.df <- curr.df %>% 
        filter(Date == curr.dates[1])
    }
    # If plot j was sampled during replicate k, 
    # curr.df will have at least 1 row (i.e., at least 
    # one species will be observed). If not, assume it 
    # was not sampled for that replicate.
    if (nrow(curr.df) > 0) {
      # Extract the species that were observed during
      # this site/replicate.
      curr.sp <- which(sp.codes %in% curr.df$Species)
      # Set value to 1 for species that were observed.
      y[curr.sp, j, k] <- 1
      # Set value to 0 for all other species.
      y[-curr.sp, j, k] <- 0
    }
  } # k (replicates)
} # j (sites)
str(y)

# Total number of observations for each species
apply(y, 1, sum, na.rm = TRUE)




# FORMAT two-observation DETECTION COVARIATES - to account for variation in detection probability across all sites and different surveys at each sites
#extract the unique date and time of day (tod) for each plot and replicate combination Taita_Data_Sp

library(dplyr)
#MAKE sure all Time is in minutes: sec - CHAT GPT 
day.time.2022_23 <- Taita_Data_Sp %>%
  group_by(Plot, Replicate) %>%
  dplyr::summarize(Date = unique(Date), 
                   tod = unique(Time)) %>%
  ungroup() %>%
  glimpse()

#extract Date as Julian date 
#extract the time of day as the number of minutes since midnight
library(dplyr)

# Initialize matrices
hb.day <- matrix(NA, nrow = J, ncol = K)
hb.tod <- matrix(NA, nrow = J, ncol = K)
for (j in 1:J) { # Loop through sites
  for (k in 1:K) { # Loop through replicate surveys
    # Get current date and time for each survey 
    curr.vals <- day.time.2022_23 %>%
      filter(Plot == plot.codes[j], Replicate == k) %>%
      dplyr::mutate(Date = yday(Date), 
                    tod = period_to_seconds(hm(tod)) / 60 ) %>%
      dplyr::select(Date, tod) %>%
      arrange(Date)
    # If the site was surveyed for the given replicate, 
    # extract the first date and time value. 
    if (nrow(curr.vals) > 0) {
      hb.day[j, k] <- curr.vals$Date[1]
      hb.tod[j, k] <- curr.vals$tod[1] 
    }
  } # k (replicates)
} # j (sites) 


# Check out the structure of our covariates. 
str(hb.day) #there are 757 uniaue Plots
str(hb.tod) 

# Detection covariates 
det.covs <- list(day = hb.day, 
                 tod = hb.tod)
str(det.covs)

#NO RUN
#average EVI
#EVIaverage <- Taita_Data_Sp %>%
#dplyr::select(EVI09, EVI10, EVI11, EVI12, EVI13, EVI14, EVI15, EVI16, EVI17, EVI18, EVI19, EVI20, EVI21) %>%
#Adding EVI to data
#average winter 2009-2021

#EVIjan_jul <- Taita_Data_Sp %>%
#dplyr::select(EVI09, EVI10, EVI11, EVI12, EVI13, EVI14, EVI15, EVI16, EVI17, EVI18, EVI19, EVI20, EVI21) %>%
#mutate(avg.jan_jul = (EVI09 + EVI10 + EVI11 + EVI12 + EVI13 + EVI14 + EVI15 + EVI16 + EVI17 + EVI18 + EVI19 + EVI20 + EVI21)/13)

#EVIjan_jul <- EVIjan_jul[, !names(EVIjan_jul) %in% c("EVI09","EVI10", "EVI11", "EVI12", "EVI13", "EVI14","EVI15", "EVI16", "EVI17", "EVI18", "EVI19","EVI20","EVI21")]
#EVIjan_jul


# average summer 2009-2021

#EVIjul_dec <- Taita_Data_Sp %>%
#dplyr::select(EVI09_jul,EVI10_jul, EVI11_jul, EVI12_jul, EVI13_jul, EVI14_jul, EVI15_jul, EVI16_jul, EVI17_jul, EVI18_jul, EVI19_jul, EVI20_jul, EVI21_jul) %>% 
#mutate(avg.jul_dec = (EVI09_jul + EVI10_jul + EVI11_jul + EVI12_jul + EVI13_jul + EVI14_jul + EVI15_jul + EVI16_jul + EVI17_jul + EVI18_jul + EVI19_jul + EVI20_jul + EVI21_jul)/13)
#EVIjul_dec <- EVIjul_dec[, !names(EVIjul_dec) %in% c("EVI09_jul","EVI10_jul", "EVI11_jul", "EVI12_jul", "EVI13_jul", "EVI14_jul","EVI15_jul", "EVI16_jul", "EVI17_jul", "EVI18_jul", "EVI19_jul","EVI20_jul","EVI21_jul")]

#str(EVIjul_dec)
#EVIjul_dec
#plot(EVIjul_dec)

#replacing EVI values for avg EVI winter vs summmer

#Taita_Data_Sp$EVIjan_jul <- EVIjan_jul
#Taita_Data_Sp$EVIjul_dec <- EVIjul_dec

Taita_Data_Sp <- Taita_Data_Sp[, !names(Taita_Data_Sp) %in% c("EVI09", "EVI09_jul", "EVI10", "EVI10_jul", "EVI11", "EVI11_jul", "EVI12", "EVI12_jul", "EVI13", "EVI13_jul", "EVI14", "EVI14_jul", "EVI15", "EVI15_jul", "EVI16", "EVI16_jul", "EVI17", "EVI17_jul", "EVI18", "EVI18_jul", "EVI19", "EVI19_jul", "EVI20", "EVI20_jul", "EVI21", "EVI21_jul")]
write.csv(Taita_Data_Sp, "Taita_Data_SpOcc_final.csv", row.names = FALSE)



# Format OCCURRENCE COVARIATES
#occurrence covariates formatted a smatrix or data frame 
# use slope as a site level covariate on occurence 

#create data frame for occurence covariates
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Euclidean_dist_occupied_forest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)

#extract covariates at each of the 823 sites from Taita_Data_Sp - as a matrix ) set avg.temp as main occ.cov
avg.temp <- occ.covs[, 3]
str(avg.temp)

canopy.height <- occ.covs[, 4]
str(canopy.height)

Precipit.driest <- occ.covs[, 6]
str(Precipit.driest)

Precipit.wettest <- occ.covs[, 5]
str(Precipit.wettest)

slope <- occ.covs[, 2]
str(slope)

#no add
#Eucl.dist.occ.forest <- occ.covs[, 7]
#str(slope)

Aspect <- occ.covs[, 8]
str(Aspect)

Elevation <- occ.covs[, 9]
str(Elevation)

EVImax_jan_jul <- occ.covs[,11]

EVImax_jul_dec <- occ.covs[,12]

#explore rage of slope
hist(occ.covs$slope)
#explore range of elevation 
hist(occ.covs$elevation)
hist(occ.covs$Precipit_wettest)



# RUNNING single spceis occupancy model (WITHOUT COORDs)

curr.bird <- 'Taita Thrush'
y.ssom <- y[which(sp.codes == curr.bird), , ]
str(y.ssom)



# Format with explicit specification of inits for alpha and beta
# with four detection parameters and three occurrence parameters 
# (including the intercept).
oven.inits <- list(alpha = c(0, 0, 0, 0), 
                   beta = c(0, 0, 0), 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))
# Format with abbreviated specification of inits for alpha and beta.
oven.inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))

#will be using the default priors of spOccupancy - specifies for clarity down below
oven.priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

#specify the number of samples for the MCMC algorithm
n.samples <- nrow(Taita_Data_Sp) 
n.burn <- round(.10 * n.samples)
n.thin <- 2
n.chains <- 3

#data
data.ssom <- list(y = y.ssom, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs,
                  coords = coords)


# out.ssom with model (model A.)
out.ssom <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIJan_july_max) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                  det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                  data = data.ssom,
                  inits = oven.inits,
                  n.samples = n.samples,
                  priors= oven.priors,
                  n.omp.threads = 1,
                  verbose = FALSE,
                  n.report = 1000, 
                  n.burn = 3000,
                  n.thin = n.thin,
                  n.chains = n.chains
) 

# out.ssom with model (model B.) - no slope
out.ssom0 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                   det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                   data = data.ssom,
                   inits = oven.inits,
                   n.samples = n.samples,
                   priors= oven.priors,
                   n.omp.threads = 1,
                   verbose = FALSE,
                   n.report = 1000, 
                   n.burn = 3000,
                   n.thin = n.thin,
                   n.chains = n.chains
) 

# with Jan-july_max EVI (modelC.)

out.ssom1 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIJan_july_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                   det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                   data = data.ssom,
                   inits = oven.inits,
                   n.samples = n.samples,
                   priors= oven.priors,
                   n.omp.threads = 1,
                   verbose = FALSE,
                   n.report = 1000, 
                   n.burn = 3000,
                   n.thin = n.thin,
                   n.chains = n.chains
) 

#with july_dec_max EVI (model D)
out.ssom2 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                   det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                   data = data.ssom,
                   inits = oven.inits,
                   n.samples = n.samples,
                   priors= oven.priors,
                   n.omp.threads = 1,
                   verbose = FALSE,
                   n.report = 1000, 
                   n.burn = 3000,
                   n.thin = n.thin,
                   n.chains = n.chains
) 


#with Jan_jul_max EVI no slope no EVI july dec (model E)
out.ssom3 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                   det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                   data = data.ssom,
                   inits = oven.inits,
                   n.samples = n.samples,
                   priors= oven.priors,
                   n.omp.threads = 1,
                   verbose = FALSE,
                   n.report = 1000, 
                   n.burn = 3000,
                   n.thin = n.thin,
                   n.chains = n.chains
) 


names(out.ssom3) # Look at the contents of the resulting object.
summary(out.ssom) #all coefficients are printed on the logit scale
summary(out.ssom0)
summary(out.ssom1)
summary(out.ssom2)
summary(out.ssom3)



#estimating probability of occurrence
plogis(1.66) #0.84
plogis(-1.70) #0.15
plogis(-0.39) #0.4

#detection probability

plogis(-0.31)
#plotting estimates

hist(Aspect)
min(Aspect)
max(Aspect)

x<- 46:304
x<-1:1000

asp <- -0.63*x#-0.04*x^2
plot(x,asp)
mean(Aspect)

min(Canopy_height)
max(Canopy_height)

x <- 3.4:23.3

can.heig <- 1.56*x
plot(x,can.heig)

min(Precipit_wettest)
max(Precipit_wettest)
x <- 351:531
x <- 1:1000
p <- -0.53*x-2.27*x^2
plot(x,p)


min(EVImax_jan_jul)
max(EVImax_jan_jul)
x <- 0.34:0.77
x <- 1:1000000
EVI <- -1.27*x
plot(x,EVI)

#Assess convergence 
plot(out.ssom3, 'beta', density = FALSE) # Occupancy parameters.
plot(out.ssom3, 'alpha', density = FALSE) # Detection parameters.

#occupancy covariate effect
install.packages("mcmcplots")
install.packages("MCMCvis")
library(MCMCvis)

quartz()
MCMCplot(out.ssom0$alpha.samples,ref_ovl= TRUE, ci = c(50,95)) #no of the covariates overlaps 0 - pretty important occupancy covariates
MCMCplot(out.ssom0$beta.samples,ref_ovl= TRUE, ci = c(50,95)) #detection covariates - same a part form slope and aspect


# POSTERIOR PREDICTIVE checks
ppc.out <- ppcOcc(out.ssom, fit.stat = 'freeman-tukey', group = 1) #0.068     binned values between sites: p-value is significsnt meaning that there is not an adequate fit of the non-spacial model- the nullhypothesis of data matching the expected data is false 
ppc.out <- ppcOcc(out.ssom, fit.stat = 'freeman-tukey', group = 2) #0.0043    binned values between replicates : p-value is significsnt meaning that there is not an adequate fit of the non-spacial model- the nullhypothesis of data matching the expected data is false 
ppc.out0 <- ppcOcc(out.ssom0, fit.stat = 'freeman-tukey', group = 1) # 0.0722  p-value is significsnt meaning that there is not an adequate fit of the non-spacial model- the nullhypothesis of data matching the expected data is false 
ppc.out1 <- ppcOcc(out.ssom1, fit.stat = 'freeman-tukey', group = 1)# 0.0671  -  the model fails to adequately represent variation in detection probability across the different replicate surveys
ppc.out2 <- ppcOcc(out.ssom2, fit.stat = 'freeman-tukey', group = 1)# 0.0753  - the model fails to adequately represent variation in occurrence and detection probability across space
ppc.out3 <- ppcOcc(out.ssom3, fit.stat = 'freeman-tukey', group = 1)# 0.0723 - the model fails to adequately represent variation in occurrence and detection probability across space

#the cathegories are not evenly distributed in the population bewteen either sites and replicates (but for replicates its due to rep. 9)

summary(ppc.out) 
summary(ppc.out0)
summary(ppc.out1)
summary(ppc.out2)
summary(ppc.out3)

ts <- theta.samples


#Visualisation throughout sites
ppc.df <- data.frame(fit = ppc.out3$fit.y, 
                     fit.rep = ppc.out3$fit.y.rep, 
                     color = 'lightskyblue1')
ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21, 
     ylab = 'Fit', xlab = 'True')
lines(ppc.df$fit, ppc.df$fit, col = 'black')
#no lack of fit + most of the fit statistics are smaller for the replicate data than the actual data set


#Visualisation throught replicates
ppc.df <- data.frame(fit = ppc.out$fit.y, 
                     fit.rep = ppc.out$fit.y.rep, 
                     color = 'lightskyblue1')
ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21, 
     ylab = 'Fit', xlab = 'True')
lines(ppc.df$fit, ppc.df$fit, col = 'black')
#yes lack of fit + most of the fit statistics are smaller for the replicate data than the actual data set





#viualising discrepancies across sites - multiple points have importnat contributions to the overall GoF measure
diff.fit <- ppc.out1$fit.y.rep.group.quants[3, ] - ppc.out1$fit.y.group.quants[3, ]
plot(diff.fit, pch = 19, xlab = 'Site ID', ylab = 'Replicate - True Discrepancy')
# might be due to extraordinary local habitat that is not adequately described by the occurrence covariates?


#viualising discrepancies across replicates
diff.fit <- ppc.out2$fit.y.rep.group.quants[3, ] - ppc.out2$fit.y.group.quants[3, ]
plot(diff.fit, pch = 19, xlab = 'Replicates', ylab = 'Replicate - True Discrepancy')
# one point has importnat contributions to the overall GoF measure - replicate 9

# plot Taita Plot and Rplictes 

ggplot(Taita_Data_Sp, aes(x = Replicate, y = Plot)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("0" = "Absence", "1" = "Presence")) +
  labs(title = "EVI by Month and Presence/Absence of Taita Thrush",
       x = "Replicate",
       y = "Pot"
       +
         theme_minimal()
  )




#   MODEL SELECTION using WAIC 
# WAIC values: smaller the better performance of the model - however WAIC is not always reliable for occupancy modeling althought it is faster than k-fold cross validation

#model A
summary(out.ssom)
waicOcc(out.ssom)  #elpd: -943.78732        pD: 14.36552      WAIC: 1916.30569 
waicOcc(out.ssom0) #elpd: -943.90465        pD: 12.46389      WAIC: 1912.73709  
summary(out.ssom1)
waicOcc(out.ssom1) #elpd: -943.8002        pD:13.3865       WAIC: 1914.3733 
summary(out.ssom2)
waicOcc(out.ssom2) #elpd: -966.08447        pD:13.32332       WAIC: 1958.81559
summary(out.ssom3)
waicOcc(out.ssom3) #elpd: -943.89611        pD:11.47025       WAIC: 1910.73272 - best model 

#.  ANOTHER METHOD L k-fold cross-validation (CV) 


#MODEL A + slope + canopy height + aspect + both EVIs + Precipit.wettest

out.k.fold.final <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIJan_july_max) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                          det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                          data = data.ssom,
                          inits = oven.inits,
                          n.samples = n.samples,
                          priors= oven.priors,
                          n.omp.threads = 1,
                          verbose = FALSE,
                          n.report = 1000, 
                          n.burn = 3000,
                          n.thin = n.thin,
                          n.chains = n.chains,
                          k.fold = 11, 
                          k.fold.threads = 11) 

summary(out.k.fold.final)

#MODEL B without slope
out.k.fold.final1 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                           det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                           data = data.ssom,
                           inits = oven.inits,
                           n.samples = n.samples,
                           priors= oven.priors,
                           n.omp.threads = 1,
                           verbose = FALSE,
                           n.report = 1000, 
                           n.burn = 3000,
                           n.thin = n.thin,
                           n.chains = n.chains,
                           k.fold = 4, 
                           k.fold.threads = 4) 

#MODEL C. with only EVIjan_july_max
out.k.fold.final_jan_jul <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIJan_july_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                                  det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                                  data = data.ssom,
                                  inits = oven.inits,
                                  n.samples = n.samples,
                                  priors= oven.priors,
                                  n.omp.threads = 1,
                                  verbose = FALSE,
                                  n.report = 1000, 
                                  n.burn = 3000,
                                  n.thin = n.thin,
                                  n.chains = n.chains,
                                  k.fold = 4, 
                                  k.fold.threads = 4) 

#MODEL D. with only EVIjul_dec_max
out.k.fold.final_jul_dec <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(slope) + I(scale(slope)^2) + scale(EVIjul_dec_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                                  det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                                  data = data.ssom,
                                  inits = oven.inits,
                                  n.samples = n.samples,
                                  priors= oven.priors,
                                  n.omp.threads = 1,
                                  verbose = FALSE,
                                  n.report = 1000, 
                                  n.burn = 3000,
                                  n.thin = n.thin,
                                  n.chains = n.chains,
                                  k.fold = 4, 
                                  k.fold.threads = 4) 

#MODEL E. withno slope and EVI jul dec
out.k.fold.final_3 <- PGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                            det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                            data = data.ssom,
                            inits = oven.inits,
                            n.samples = n.samples,
                            priors= oven.priors,
                            n.omp.threads = 1,
                            verbose = FALSE,
                            n.report = 1000, 
                            n.burn = 3000,
                            n.thin = n.thin,
                            n.chains = n.chains,
                            k.fold = 4, 
                            k.fold.threads = 4) 



summary(out.k.fold.final) # all Rhat <1.01
summary(out.k.fold.final1) # all Rhat <1.01
summary(out.k.fold.final_jan_jul) # all Rhat <1.01
summary(out.k.fold.final_jul_dec) # all Rhat <1.01
summary(out.k.fold.final_3) # all Rhat <1.01

out.k.fold.final$k.fold.deviance 
out.k.fold.final1$k.fold.deviance 
out.k.fold.final_jan_jul$k.fold.deviance
out.k.fold.final_jul_dec$k.fold.deviance
out.k.fold.final_3$k.fold.deviance



#model A. with slope, canopy height, aspect, precip. wet and EVIjan_jul_max + EVIjul_dec_max - 1969.411
#model B. with no slope, with canopy height, aspect, precip. wet and EVIjan_jul_max + EVIjul_dec_max - 1953.558
#model C. with slope, canopy height, aspect, precip. wet and EVIjan_jul_max  - 1956.165
#model D. with slope, canopy height, aspect, precip. wet and EVIjul_dec_max - 2021.33
#model E. with no slope, canopy height, aspect, precip. wet and EVIjan_jul_max - 1951.412 - best model prediction
#I've used the same rule of thumb as suggested by Spiegelhalter for DIC: within 2 units is considered about the same, within 7 units similar model fit, and >7 different. I haven't seen anyone use different guidelines. github.




#  PREDICTION
#plotting predictions of the model
#converted raster to lower resolution to nnot make laptop crash - from 30x30 to 298.8298 x 298.8298 to reduce computing time by facor of 10
#TEMP CRASHES LAPTOP! NO run 

#canopy height

C.height.df <- raster::as.data.frame(CanopyHeight.1,xy=TRUE)
str(C.height.df)
C.height.df$GLAD_SAFR_30m_UTM37S[is.na(C.height.df$GLAD_SAFR_30m_UTM37S)] <- 0
str(C.height.df)

mean(C.height.df$GLAD_SAFR_30m_UTM37S) #3.19
hist(C.height.df$GLAD_SAFR_30m_UTM37S) #squed towatrd lowe values with mean 3

#precipit driest

Precdry.df <- raster::as.data.frame(DryQuart.1,xy=TRUE)
str(Precdry.df)
Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m[is.na(Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m)] <- 0
str(Precwet.df)

#background precipitation wettest

precipit.df <- raster::as.data.frame(WetQuart.1,xy=TRUE)
str(precipit.df)
precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m[is.na(precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m)] <- 0
str(precipit.df)


#Aspect

Aspect.df <- raster::as.data.frame(Aspect.1,xy=TRUE)
str(Aspect.df)
Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S[is.na(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S)] <- 0
str(Aspect.df)


#ELEVATION

# Create a new raster layer with NA values for elevations below 1200 and above 2200 meters
install.packages("raster")
library("raster")

new_elev_raster <- calc(Elevation.1, fun = function(x) {
  x[x < 1200 | x > 2200] <- NA
  return(x)
})

plot(new_elev_raster)

Elevation.df <- raster::as.data.frame(new_elev_raster,xy=TRUE)
str(Elevation.df)

Elevation.df$layer[is.na(Elevation.df$layer)] <- 0
str(Elevation.df)


#Crops

Crops.df <- raster::as.data.frame(Crops.1,xy=TRUE)
str(Crops.df)
Crops.df$Band_1[is.na(Crops.df$Band_1)] <- 0
str(Crops.df)

#Pastures 

Pastures.df <- raster::as.data.frame(Pastures.1,xy=TRUE)
str(Pastures.df)
Pastures.df$Band_1[is.na(Pastures.df$Band_1)] <- 0
str(Pastures.df)

#slope

Slope.df <- raster::as.data.frame(Slope.1,xy=TRUE)
str(Slope.df)
Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S[is.na(Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S)] <- 0
str(Slope.df)


#background max EVIjan_jul_max 

raster.max_jan_jul_background.df <- raster::as.data.frame(raster.max_jan_jul.1,xy=TRUE)
str(raster.max_jan_jul_background.df)
raster.max_jan_jul_background.df$index_1[is.na(raster.max_jan_jul_background.df$index_1)] <- 0
str(raster.max_jan_jul_background.df)




#background max EVIjuly_dec_max 

raster.max_jul_dec_background.df <- raster::as.data.frame(raster.max_jul_dec.1,xy=TRUE)
str(raster.max_jul_dec_background.df)
raster.max_jul_dec_background.df$index_1[is.na(raster.max_jul_dec_background.df$index_1)] <- 0
str(raster.max_jul_dec_background.df)




#Given that we standardized the elevation values when we fit the model, we need to standardize the average temperature values for prediction using the exact same values of the mean and standard deviation of the average temp values used to fit the data.
canopy.height.pred <- (C.height.df$GLAD_SAFR_30m_UTM37S - mean(occ.covs[, 4])) / sd(occ.covs[, 4])
precipit.dry.pred <- (Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m - mean(occ.covs[, 6])) / sd(occ.covs[, 6])
#Dist.pred <- (Dist.df$Euclidean_Distance_OccupiedForest_30m_UTM37S - mean(occ.covs[, 7])) / sd(occ.covs[, 7])
Aspect.pred <- (Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S - mean(occ.covs[, 8])) / sd(occ.covs[, 8])
EVIjan_jul.pred <- (raster.max_jan_jul_background.df$index_1 - mean(occ.covs[, 11])) / sd(occ.covs[, 11])
EVIjul_dec.pred <- (raster.max_jul_dec_background.df$index_1 - mean(occ.covs[, 12])) / sd(occ.covs[, 12])
Elevation.pred <- (Elevation.df$layer - mean(occ.covs[, 9])) / sd(occ.covs[, 9])
slope.pred <- (Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S - mean(occ.covs[, 2])) / sd(occ.covs[, 2])
precipit.pred <- (precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m - mean(occ.covs[, 5])) / sd(occ.covs[, 5])



#multicollinearity - no add
#Crops.pred <- (Crops.df$Band_1 - mean(occ.covs[, 10])) / sd(occ.covs[, 10])
#avg.temp.pred <- (avg.temp.df$avg_temp - mean(occ.covs[, 3])) / sd(occ.covs[, 3])
#precipit.wet.pred <- (Precwet.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m - mean(occ.covs[, 5])) / sd(occ.covs[, 5])


#creating a data frame for all standardised values 
occ.covs.pred <- as.data.frame(canopy.height.pred)
#add other columns 
#occ.covs.pred$precipit.dry.pred <- precipit.dry.pred  #remove
#occ.covs.pred$Pastures.pred <- Pastures.pred #remove
#occ.covs.pred$Aspect.pred <- Aspect.pred #remove
occ.covs.pred$EVIjan_jul <- EVIjan_jul.pred
#occ.covs.pred$EVIjul_dec <- EVIjul_dec.pred 

#occ.pred.quadrat
occ.covs.pred.quad <- as.data.frame(Aspect.pred)
#?occ.covs.pred.quad$Elevation.pred <- Elevation.pred 
#occ.covs.pred.quad$slope.pred <- slope.pred 
occ.covs.pred.quad$precipit.pred <- precipit.pred 



#NO add due to multicollinearity
#occ.covs.pred$Dist.pred <- Dist.pred
#occ.covs.pred$avg.temp.pred <- avg.temp.pred
#occ.covs.pred$precipit.wet.pred <- precipit.wet.pred
#occ.covs.pred$Crops.pred  <- Crops.pred 
#occ.covs.pred.quad <- as.data.frame(precipit.dry.pred)



occ.covs.pred <- as.matrix.data.frame(occ.covs.pred)
occ.covs.pred.quad <- as.matrix.data.frame(occ.covs.pred.quad)
str(occ.covs.pred)
str(occ.covs.pred.quad)

# These are the new intercept and covariate data.

X.0 <- cbind(1, occ.covs.pred, occ.covs.pred.quad, occ.covs.pred.quad^2)  
out.pred <- predict(out.ssom3, X.0)


#PLOT 

library(ggplot2)
install.packages("plm")
library(plm)
install.packages("stars")
library(stars)

plot.dat <- data.frame(x = C.height.df$x, #coordinates are the same for all environmental values so we can use whichever as reference coords
                       y = C.height.df$y, 
                       mean.psi = apply(out.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.pred$psi.0.samples, 2, sd), 
                       stringsAsFactors = FALSE)

st_as_raster <- function(rstars){
  rext <- st_bbox(rstars)
  raster(t(rstars[[1]]), xmn = rext[1], xmx = rext[3],
         ymn = rext[2], ymx=rext[4],
         crs = st_crs(rstars)$proj4string)
}


# Make a species distribution map showing the point estimates,
# or predictions (posterior means)
dat.stars <- st_as_stars(plot.dat, dims = c('x', 'y'))
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = mean.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Probability', 
       title = 'Mean Taita Thrush occurrence probability') +
  theme_bw()


dat.stars.rast <- st_as_raster(dat.stars)


plot(dat.stars.rast)
contour(dat.stars.rast, add=TRUE)
plot(dat.stars.rast,col=terrain.colors(100))



# Map of the associated uncertainty of these predictions
# (i.e., posterior sds)
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = sd.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Uncertainity', 
       title = 'SD Taita Thrush occurrence probability uncertainty') +
  theme_bw()


#Adding isoclines 

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
plot(r)
contour(dat.stars, add=TRUE)
plot(dat.stars,col=terrain.colors(100))





#PREDICTION if Forest was everywhere 
#rerun model with EVI as optimal value and canopy height

#create a raster simulating a NEW ENVIRONMENT C.height:

#get a reference raster
plot(CanopyHeight.1)

#create a new raster with the same dimensions as C.height
r1 <- raster(CanopyHeight.1)

#find best value for EVI_max in Jan_jul - histogram

toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$Canopy_height[which(toto>=1)]
tata0 <- data.ssom$occ.covs$Canopy_height[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="EVI values")
hist(tata1,main="Presence",xlab="EVI values")

mean(tata0) #14.89257 
mean(tata1) #18.26492 - higher canoy height in presence 
median(tata0) # 15.91543
median(tata1) # 19.1239



#set all values to 18.26 (you need to select a value that makes sense for EVI or canopy height)
values(r1) <- runif(ncell(r1),16.434,20.086) #±10%
plot(r1)




#EVI
#get a reference raster
plot(raster.max_jan_jul.1)
#create a new raster with the same dimensions as raster.max_jan_jul.1
r2 <- raster(raster.max_jan_jul.1)

#find best value for EVI_max in Jan_jul - histogram
toto <- rowSums(data.ssom$y,na.rm=TRUE)
table(toto)

toto

tata1 <- data.ssom$occ.covs$EVIJan_july_max[which(toto>=1)]
tata0 <- data.ssom$occ.covs$EVIJan_july_max[which(toto==0)]
par(mfrow=c(1,2))
hist(tata0,main="Absence",xlab="EVI values")
hist(tata1,main="Presence",xlab="EVI values")

mean(tata0) #0.603565 - higher EVI in absence 
mean(tata1) #0.5771171
median(tata0) # 0.6096148
median(tata1) # 0.5723813


#set all values to a random value between 0.513 and 0.627 (if you want to see if that will have an effect)
values(r2) <- runif(ncell(r2),0.513,0.627) #±10%
plot(r2)


#EVI july december
#get a reference raster
plot(raster.max_jul_dec.1)
#create a new raster with the same dimensions as raster.max_jan_jul.1
#r5 <- raster(raster.max_jul_dec.1)

#find best value for EVI_max in Jan_jul - histogram
#toto <- rowSums(data.ssom$y,na.rm=TRUE)
#table(toto)

#toto

#tata1 <- data.ssom$occ.covs$EVIjul_dec_max[which(toto>=1)]
#tata0 <- data.ssom$occ.covs$EVIjul_dec_max[which(toto==0)]
#par(mfrow=c(1,2))
#hist(tata0,main="Absence",xlab="EVI values")
#hist(tata1,main="Presence",xlab="EVI values")

#mean(tata0) #0.6581598 - higher EVI in absence 
#mean(tata1) #0.5968133
#median(tata0) # 0.6602599
#median(tata1) # 0.5959378


#set all values to a random value between 0.513 and 0.627 (if you want to see if that will have an effect)
#values(r5) <- runif(ncell(r5),0.531,0.649) #±10%
#plot(r5)








#extract values for coordinates C. height
C.height_best <- raster::extract(r1, coordinates_sf)

#background C.height_best
r1


#extract values for coordinates EVI jan-july
EVIJan_july_max_best <- raster::extract(r2, coordinates_sf)

#background EVIjul_dec_max_best
r2

#extract values for coordinates EVI july-dec
#EVIJuly_dec_max_best <- raster::extract(r5, coordinates_sf)





#add to Sp data
extracted_values_df <- data.frame(
  Aspect = Aspect,
  Euclidean_dist_occupied_forest = Euclidean_dist_occupied_forest,
  Precipit_driest = Precipit_driest,
  Precipit_wettest = Precipit_wettest,
  Max_temp = Max_temp,
  Min_temp = Min_temp,
  slope = slope,
  VH = VH,
  VV = VV,
  EVI09 = EVI09,
  EVI10 = EVI10,
  EVI11 = EVI11,
  EVI12 = EVI12,
  EVI13 = EVI13,
  EVI14 = EVI14,
  EVI15 = EVI15,
  EVI16 = EVI16,
  EVI17 = EVI17,
  EVI18 = EVI18,
  EVI19 = EVI19,
  EVI20 = EVI20,
  EVI21 = EVI21,
  EVI09_jul = EVI09_jul,
  EVI10_jul = EVI10_jul,
  EVI11_jul = EVI11_jul,
  EVI12_jul = EVI12_jul,
  EVI13_jul = EVI13_jul,
  EVI14_jul = EVI14_jul,
  EVI15_jul = EVI15_jul,
  EVI16_jul = EVI16_jul,
  EVI17_jul = EVI17_jul,
  EVI18_jul = EVI18_jul,
  EVI19_jul = EVI19_jul,
  EVI20_jul = EVI20_jul,
  EVI21_jul = EVI21_jul,
  EVIJan_july_max = EVIJan_july_max,
  EVIjul_dec_max = EVIjul_dec_max,
  #Crops = Crops, no add
  #Pastures = Pastures, no add
  elevation = elevation,
  C.height_best = C.height_best,
  EVIJan_july_max_best = EVIJan_july_max_best
)

# Combine the coordinates with the extracted values
survey_points_with_environmental_values <- cbind(coordinates_sf, extracted_values_df)
# Print the first few rows of the combined data frame
print(head(survey_points_with_environmental_values))

#Match survey_points_with_environmental_values with Taita Thrush data and merge into unique dataset 
Taita_Data_env <- merge(Taita_Data, survey_points_with_environmental_values, by = "Point.ID", all.x = TRUE)




#RERUN initial code to create Taita_data_Sp untill occ.covs





#add r1 for new values of canopy height in the occ.cov and in the model 
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, Aspect, Precipit_wettest, geometry, EVIJan_july_max_best, C.height_best) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)


#extract covariates at each of the 823 sites from Taita_Data_Sp - as a matrix ) set avg.temp as main occ.cov
C.height_best <- occ.covs[, 7]
str(C.height_best)

slope <- occ.covs[, 2]
str(slope)

Aspect <- occ.covs[, 3]
str(Aspect)

Precipit.wett <- occ.covs[, 4]
str(Elevation)

EVImax_jan_jul_best <- occ.covs[,6]



#Aspect

Aspect.df <- raster::as.data.frame(Aspect.1,xy=TRUE)
str(Aspect.df)
Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S[is.na(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S)] <- 0
str(Aspect.df)

#Elevation
# Create a new raster layer with NA values for elevations below 1200 and above 2200 meters

new_elev_raster <- calc(Elevation.1, fun = function(x) {
  x[x < 1200 | x > 2200] <- NA
  return(x)
})

plot(new_elev_raster)

Elevation.df <- raster::as.data.frame(new_elev_raster,xy=TRUE)
str(Elevation.df)

Elevation.df$layer[is.na(Elevation.df$layer)] <- 0
str(Elevation.df)


#slope

Slope.df <- raster::as.data.frame(Slope.1,xy=TRUE)
str(Slope.df)
Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S[is.na(Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S)] <- 0
str(Slope.df)


#background max EVIjan_jul_max

raster.max_jan_jul_background.df <- raster::as.data.frame(r2,xy = TRUE)
str(raster.max_jan_jul_background.df)
raster.max_jan_jul_background.df$layer[is.na(raster.max_jan_jul_background.df$layer)] <- 0
str(raster.max_jan_jul_background.df)


#background max EVIjuly_dec_max

#raster.max_july_dec_background.df <- raster::as.data.frame(r5,xy = TRUE)
#str(raster.max_july_dec_background.df)
#raster.max_july_dec_background.df$layer[is.na(raster.max_july_dec_background.df$layer)] <- 0
#str(raster.max_july_dec_background.df)


#background C.height_best

C.height_best.df <- raster::as.data.frame(r1,xy=TRUE)
str(C.height_best.df)
C.height_best.df$layer[is.na(C.height_best.df$layer)] <- 0
str(C.height_best.df)

#background precipitation wettest

precipit.df <- raster::as.data.frame(WetQuart.1,xy=TRUE)
str(precipit.df)
precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m[is.na(precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m)] <- 0
str(precipit.df)



#Given that we standardized the elevation values when we fit the model, we need to standardize the average temperature values for prediction using the exact same values of the mean and standard deviation of the average temp values used to fit the data.
Aspect.pred <- (Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S - mean(occ.covs[, 3])) / sd(occ.covs[, 3])
EVIJan_july_max_best.pred <- (raster.max_jan_jul_background.df$layer - mean(occ.covs[, 6])) / sd(occ.covs[, 6])
#slope.pred <- (Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S - mean(occ.covs[, 2])) / sd(occ.covs[, 2])
C.height_best.pred <- (C.height_best.df$layer -mean(occ.covs[, 7])) / sd(occ.covs[, 7])
precipit.pred <- (precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m - mean(occ.covs[, 4])) / sd(occ.covs[, 4])


#creating a data frame for all standardised values 
occ.covs.pred <- as.data.frame(C.height_best.pred)
#add other columns 
occ.covs.pred$EVIJan_july_max_best <- EVIJan_july_max_best.pred


#occ.pred.quadrat
occ.covs.pred.quad <- as.data.frame(Aspect.pred)
occ.covs.pred.quad$precipit.pred <- precipit.pred 


occ.covs.pred <- as.matrix.data.frame(occ.covs.pred)
occ.covs.pred.quad <- as.matrix.data.frame(occ.covs.pred.quad)
str(occ.covs.pred)
str(occ.covs.pred.quad)

# These are the new intercept and covariate data.
curr.bird <- 'Taita Thrush'
y.ssom <- y[which(sp.codes == curr.bird), , ]
str(y.ssom)

# Format with explicit specification of inits for alpha and beta
# with four detection parameters and three occurrence parameters 
# (including the intercept).
oven.inits <- list(alpha = c(0, 0, 0, 0), 
                   beta = c(0, 0, 0), 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))
# Format with abbreviated specification of inits for alpha and beta.
oven.inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))

#will be using the default priors of spOccupancy - specifies for clarity down below
oven.priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

#specify the number of samples for the MCMC algorithm
n.samples <- nrow(Taita_Data_Sp) 
n.burn <- round(.10 * n.samples)
n.thin <- 2
n.chains <- 3

#data
data.ssom <- list(y = y.ssom, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs,
                  coords = coords)


#MODEL A. with both EVI's best and C.heihgt best - Predicting: if perfect conditions were found where would T.Thrush be more likely found 

out.ssom.1 <- PGOcc(occ.formula = ~ scale(C.height_best) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max_best) + scale(Precipit.wett) + I(scale(Precipit.wett)^2), 
                    det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                    data = data.ssom,
                    inits = oven.inits,
                    n.samples = n.samples,
                    priors= oven.priors,
                    n.omp.threads = 1,
                    verbose = FALSE,
                    n.report = 1000, 
                    n.burn = 3000,
                    n.thin = n.thin,
                    n.chains = n.chains
) 



X.0 <- cbind(1, occ.covs.pred, occ.covs.pred.quad, occ.covs.pred.quad^2)  #X.0 must have 10 columns 
out.pred <- predict(out.ssom.1, X.0)



plot.dat <- data.frame(x = Slope.df$x, #coordinates are the same for all environmental values so we can use whichever as reference coords
                       y = Slope.df$y, 
                       mean.psi = apply(out.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.pred$psi.0.samples, 2, sd), 
                       stringsAsFactors = FALSE)
# Make a species distribution map showing the point estimates,
# or predictions (posterior means)
dat.stars <- st_as_stars(plot.dat, dims = c('x', 'y'))
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = mean.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Probability', 
       title = 'Mean Taita Thrush occurrence probability for best EVI and Canopy height values') +
  theme_bw()


# Map of the associated uncertainty of these predictions
# (i.e., posterior sds)
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = sd.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Uncertainity', 
       title = 'SD Taita Thrush occurrence probability uncertainty in best case scenario') +
  theme_bw()










#PLot EVI_jan_jul_max with C.height to get the relationship between values of EVI and C.height 

quartz()
plot(Taita_Data_Sp$EVIJan_july_max,Taita_Data_Sp$Canopy_height)


#how do other types of forest affect the presenc eof T.Thrush and how to best choose forest type for reforestation 


#Selecting values base on ranks

Taita_Data_Sp$EVI1_R <- rank(Taita_Data_Sp$EVIJan_july_max)
#Taita_Data_Sp$EVI2_R <- rank(Taita_Data_Sp$EVIjul_dec_max)
Taita_Data_Sp$CH_R <- rank(Taita_Data_Sp$Canopy_height)
Taita_Data_Sp$EVI1_Rinv <- max(Taita_Data_Sp$EVI1_R)-Taita_Data_Sp$EVI1_R
#Taita_Data_Sp$EVI2_Rinv <- max(Taita_Data_Sp$EVI2_R)-Taita_Data_Sp$EVI2_R


quartz()
plot(Taita_Data_Sp$EVIJan_july_max,Taita_Data_Sp$Canopy_height)
points(Taita_Data_Sp$EVIJan_july_max[which.max(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)],Taita_Data_Sp$Canopy_height[which.max(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)],col="red",pch=16)
points(Taita_Data_Sp$EVIJan_july_max[which.min(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)],Taita_Data_Sp$Canopy_height[which.min(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)],col="red",pch=16)
points(Taita_Data_Sp$EVIJan_july_max[which.max(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)],Taita_Data_Sp$Canopy_height[which.max(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)],col="red",pch=16)
points(Taita_Data_Sp$EVIJan_july_max[which.min(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)],Taita_Data_Sp$Canopy_height[which.min(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)],col="red",pch=16)
points(mean(Taita_Data_Sp$EVIJan_july_max),mean(Taita_Data_Sp$Canopy_height),col="red",pch=16)

#MaxEVI, MAx C.height = 0.70 , 20.93
Taita_Data_Sp$EVIJan_july_max[which.max(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)]
Taita_Data_Sp$Canopy_height[which.max(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)]
#MinEVI, Min C.height = 0.38 , 3.46
Taita_Data_Sp$EVIJan_july_max[which.min(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)]
Taita_Data_Sp$Canopy_height[which.min(Taita_Data_Sp$EVI1_R+Taita_Data_Sp$CH_R)]
#MinEVI, MAx C.height = 0.49, 21.78
Taita_Data_Sp$EVIJan_july_max[which.max(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)]
Taita_Data_Sp$Canopy_height[which.max(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)]
#MaxEVI, Min C.height = 0.76 , 7.60
Taita_Data_Sp$EVIJan_july_max[which.min(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)]
Taita_Data_Sp$Canopy_height[which.min(Taita_Data_Sp$EVI1_Rinv+Taita_Data_Sp$CH_R)]
#avgEVI, avg C.height = 0.60 , 16.22
mean(Taita_Data_Sp$EVIJan_july_max)
mean(Taita_Data_Sp$Canopy_height)


#value ±10% as you need a range of values to run the model 
#.1.choose min(EVI) and min(canopy_height) - values: EVI:  0.38 - 0.342, 0.41 , C.heihgt: 3.46 - 3.11, 3.80 - lower probabilty of occurence but lower uncertainity compared to best case scenario - 
#.2.choose min(EVI) and max(canopy_height) - values: EVI: 0.49 - 0.441, 0.539 , C.heihgt:  21.78 - 19.602, 23.958 - lower probbailty of occurence than best case scenario, lower uncertainity
#.3.choose max(EVI) and min(canopy_height) - values: EVI: 0.76 - 0.684, 0.836, C.heihgt: 7.60 - 6.84, 8.36 - lower probbailty of occurence than best case scenario, lower uncertainity
#.4.choose max(EVI) and max(canopy_height) - values: EVI: 0.7 - 0.63, 0.77 , C.heihgt: 20.93 - 18.83, 23.023 - similar prediction (slightly lower suitability) and lower uncertinity
#.5.choose avg(EVI) and avg(canopy_heihgt) for the most common forest type (could be easier to implement if it doesnt significantly affect the distribution of T.Thrush) - values: EVI: 0.60 - 0.54, 0.66 , C.heihgt: 16.22- 14.598, 17.842 - slight lower probability of occurence, lower uncertainity than best case scenario



#choose X(EVI) and X(canopy_height) and plot - r3 and r4 will be inputted the values mentioned above to save time 


#create a new raster with the same dimensions as CanopyHeight.1
r3 <- raster(CanopyHeight.1)
#set all values 
values(r3) <- runif(ncell(r2),14.598,17.842) #±10%
plot(r3)


#create a new raster with the same dimensions as raster.max_jan_jul.1
r4 <- raster(raster.max_jan_jul.1)
#set all values to a random value between 0.342 and 0.418 (if you want to see if that will have an effect)

values(r4) <- runif(ncell(r2),0.54,0.66) #±10%
plot(r4)


#extract values for coordinates C. height
C.height <- raster::extract(r3, coordinates_sf)

#background C.height_best
r3

#extract values for coordinates EVI
EVI <- raster::extract(r4, coordinates_sf)

#background C.height_best
r3



#add to Sp data
extracted_values_df <- data.frame(
  Aspect = Aspect,
  Euclidean_dist_occupied_forest = Euclidean_dist_occupied_forest,
  Precipit_driest = Precipit_driest,
  Precipit_wettest = Precipit_wettest,
  Max_temp = Max_temp,
  Min_temp = Min_temp,
  slope = slope,
  VH = VH,
  VV = VV,
  EVI09 = EVI09,
  EVI10 = EVI10,
  EVI11 = EVI11,
  EVI12 = EVI12,
  EVI13 = EVI13,
  EVI14 = EVI14,
  EVI15 = EVI15,
  EVI16 = EVI16,
  EVI17 = EVI17,
  EVI18 = EVI18,
  EVI19 = EVI19,
  EVI20 = EVI20,
  EVI21 = EVI21,
  EVI09_jul = EVI09_jul,
  EVI10_jul = EVI10_jul,
  EVI11_jul = EVI11_jul,
  EVI12_jul = EVI12_jul,
  EVI13_jul = EVI13_jul,
  EVI14_jul = EVI14_jul,
  EVI15_jul = EVI15_jul,
  EVI16_jul = EVI16_jul,
  EVI17_jul = EVI17_jul,
  EVI18_jul = EVI18_jul,
  EVI19_jul = EVI19_jul,
  EVI20_jul = EVI20_jul,
  EVI21_jul = EVI21_jul,
  EVIJan_july_max = EVIJan_july_max,
  EVIjul_dec_max = EVIjul_dec_max,
  #Crops = Crops, no add
  #Pastures = Pastures, no add
  elevation = elevation,
  C.height = C.height,
  EVI = EVI
)

# Combine the coordinates with the extracted values
survey_points_with_environmental_values <- cbind(coordinates_sf, extracted_values_df)
# Print the first few rows of the combined data frame
print(head(survey_points_with_environmental_values))

#Match survey_points_with_environmental_values with Taita Thrush data and merge into unique dataset 
Taita_Data_env <- merge(Taita_Data, survey_points_with_environmental_values, by = "Point.ID", all.x = TRUE)




#RERUN initial code to create Taita_data_Sp untill occ.covs

Taita_Data_Sp <- Taita_Data_env

#FORMAT for model runnig 
names(Taita_Data_Sp)[1] <- "Plot"

#create replicate column
Taita_Data_Sp$Replicate <- 1
for(i in unique(Taita_Data_Sp$Plot)){
  toto <- Taita_Data_Sp[which(Taita_Data_Sp$Plot==i),]
  j <- 0
  for(jj in 1:nrow(unique(toto[,c(2,5)]))){
    j <- j+1
    toto[which(toto[,2]==unique(toto[,c(2,5)])[j,1] & toto[,5]==unique(toto[,c(2,5)])[j,2]),]$Replicate <- j
  }
  Taita_Data_Sp[which(Taita_Data_Sp$Plot==i),] <- toto
}

#head(Taita_Data_Sp) - changing column names for right format - taken from https://www.jeffdoser.com/files/spoccupancy-web/articles/dataformatting#:~:text=All%20multi%2Dspecies%20occupancy%20model,species%2C%20site%2C%20and%20replicate.

names(Taita_Data_Sp)[5] <- "Time"
names(Taita_Data_Sp)[11] <- "Observer"
names(Taita_Data_Sp)[8] <- "Species"
class(Taita_Data_Sp$Date)
Taita_Data_Sp$Year <- year(Taita_Data_Sp$Date)

str(Taita_Data_Sp)


# Converting srtring "Date" into R object

class(Taita_Data_Sp$Date)

#doenst work - NA's produced 
Taita_Data_Sp$Date <- as.Date(Taita_Data_Sp$Date, "%d/%m/%Y") 
str(Taita_Data_Sp)
class(Taita_Data_Sp$Date)


# Extract the year from the data column
Taita_Data_Sp$Year <- year(Taita_Data_Sp$Date)
str(Taita_Data_Sp)




#EXTRACT DETECTION-NONDETECTION data

y.long <- Taita_Data_Sp %>%
  group_by(Plot, Date, Replicate, Species) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::ungroup() %>%
  pillar::glimpse()
# maybe have to change Replicate from dbl to character 

# convert y.long to 3 dimensional array that contains the site/replicate matrices for each of our 𝑁 species

# Species codes.
sp.codes <- sort(unique(y.long$Species))
# Plot (site) codes.
plot.codes <- sort(unique(y.long$Plot))
# Number of species
N <- length(sp.codes)

#see ho many unique replicates there are 
unique(Taita_Data_Sp$Replicate) #there are 11

# Maximum number of replicates at a site
K <- 11
# Number of sites
J <- length(unique(y.long$Plot))
# Array for detection-nondetection data. 
y <- array(NA, dim = c(N, J, K))
# Label the dimensions of y (not necessary, but helpful)
dimnames(y)[[1]] <- sp.codes
dimnames(y)[[2]] <- plot.codes
# Look at the structure of our array y
str(y)
#contains data for 62 species across 831 sites acrss a possible 5 replicates at each site 

#fill y.long array with data form each species by using nested for loops - 
# assume that if a site is sampled for a given replicate then the observer would have detected at leas one bird, if no bird is detected then the site was not sampled 

for (j in 1:J) { # Loop through sites.
  for (k in 1:K) { # Loop through replicates at each site.
    # Extract data for current site/replicate combination.
    curr.df <- y.long %>%
      filter(Plot == plot.codes[j], Replicate == k)
    # Check if more than one date for a given replicate
    if (n_distinct(curr.df$Date) > 1) {
      # If there is more than 1 date, only use the data
      # from the first date.
      curr.dates <- unique(sort(curr.df$Date))
      curr.df <- curr.df %>% 
        filter(Date == curr.dates[1])
    }
    # If plot j was sampled during replicate k, 
    # curr.df will have at least 1 row (i.e., at least 
    # one species will be observed). If not, assume it 
    # was not sampled for that replicate.
    if (nrow(curr.df) > 0) {
      # Extract the species that were observed during
      # this site/replicate.
      curr.sp <- which(sp.codes %in% curr.df$Species)
      # Set value to 1 for species that were observed.
      y[curr.sp, j, k] <- 1
      # Set value to 0 for all other species.
      y[-curr.sp, j, k] <- 0
    }
  } # k (replicates)
} # j (sites)
str(y)

# Total number of observations for each species
apply(y, 1, sum, na.rm = TRUE)




# FORMAT two-observation DETECTION COVARIATES - to account for variation in detection probability across all sites and different surveys at each sites
#extract the unique date and time of day (tod) for each plot and replicate combination Taita_Data_Sp

library(dplyr)
#MAKE sure all Time is in minutes: sec - CHAT GPT 
day.time.2022_23 <- Taita_Data_Sp %>%
  group_by(Plot, Replicate) %>%
  dplyr::summarize(Date = unique(Date), 
                   tod = unique(Time)) %>%
  ungroup() %>%
  glimpse()

#extract Date as Julian date 
#extract the time of day as the number of minutes since midnight
library(dplyr)

# Initialize matrices
hb.day <- matrix(NA, nrow = J, ncol = K)
hb.tod <- matrix(NA, nrow = J, ncol = K)
for (j in 1:J) { # Loop through sites
  for (k in 1:K) { # Loop through replicate surveys
    # Get current date and time for each survey 
    curr.vals <- day.time.2022_23 %>%
      filter(Plot == plot.codes[j], Replicate == k) %>%
      dplyr::mutate(Date = yday(Date), 
                    tod = period_to_seconds(hm(tod)) / 60 ) %>%
      dplyr::select(Date, tod) %>%
      arrange(Date)
    # If the site was surveyed for the given replicate, 
    # extract the first date and time value. 
    if (nrow(curr.vals) > 0) {
      hb.day[j, k] <- curr.vals$Date[1]
      hb.tod[j, k] <- curr.vals$tod[1] 
    }
  } # k (replicates)
} # j (sites) 


# Check out the structure of our covariates. 
str(hb.day) #there are 757 uniaue Plots
str(hb.tod) 

# Detection covariates 
det.covs <- list(day = hb.day, 
                 tod = hb.tod)
str(det.covs)

Taita_Data_Sp <- Taita_Data_Sp[, !names(Taita_Data_Sp) %in% c("EVI09", "EVI09_jul", "EVI10", "EVI10_jul", "EVI11", "EVI11_jul", "EVI12", "EVI12_jul", "EVI13", "EVI13_jul", "EVI14", "EVI14_jul", "EVI15", "EVI15_jul", "EVI16", "EVI16_jul", "EVI17", "EVI17_jul", "EVI18", "EVI18_jul", "EVI19", "EVI19_jul", "EVI20", "EVI20_jul", "EVI21", "EVI21_jul")]




#add r1 for new values of canopy height in the occ.cov and in the model 
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, Aspect, Precipit_wettest, geometry, EVI, C.height) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)


#extract covariates at each of the 823 sites from Taita_Data_Sp - as a matrix ) set avg.temp as main occ.cov
C.height <- occ.covs[, 6]
str(C.height)

Aspect <- occ.covs[, 2]
str(Aspect)

Precipit.wett <- occ.covs[, 3]
str(Precipit.wett)

EVI <- occ.covs[,5]



#Aspect

Aspect.df <- raster::as.data.frame(Aspect.1,xy=TRUE)
str(Aspect.df)
Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S[is.na(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S)] <- 0
str(Aspect.df)

#Elevation
# Create a new raster layer with NA values for elevations below 1200 and above 2200 meters

new_elev_raster <- calc(Elevation.1, fun = function(x) {
  x[x < 1200 | x > 2200] <- NA
  return(x)
})

plot(new_elev_raster)

Elevation.df <- raster::as.data.frame(new_elev_raster,xy=TRUE)
str(Elevation.df)

Elevation.df$layer[is.na(Elevation.df$layer)] <- 0
str(Elevation.df)


#background max EVIjan_jul_max

EVI.df <- raster::as.data.frame(r4,xy = TRUE)
str(EVI.df)
EVI.df$layer[is.na(EVI.df$layer)] <- 0
str(EVI.df)


#background C.height

C.height.df <- raster::as.data.frame(r3,xy=TRUE)
str(C.height.df)
C.height.df$layer[is.na(C.height.df$layer)] <- 0
str(C.height.df)

#background precipitation wettest

precipit.df <- raster::as.data.frame(WetQuart.1,xy=TRUE)
str(precipit.df)
precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m[is.na(precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m)] <- 0
str(precipit.df)


#Given that we standardized the elevation values when we fit the model, we need to standardize the average temperature values for prediction using the exact same values of the mean and standard deviation of the average temp values used to fit the data.
Aspect.pred <- (Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S - mean(occ.covs[, 2])) / sd(occ.covs[, 2])
EVI.pred <- (EVI.df$layer - mean(occ.covs[, 5])) / sd(occ.covs[, 5])
Precipit.pred <- (precipit.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m - mean(occ.covs[, 3])) / sd(occ.covs[, 3])
C.height.pred <- (C.height.df$layer -mean(occ.covs[, 6])) / sd(occ.covs[, 6])

#creating a data frame for all standardised values 
occ.covs.pred <- as.data.frame(C.height.pred)
#add other columns 
occ.covs.pred$EVI <- EVI.pred

#occ.pred.quadrat
occ.covs.pred.quad <- as.data.frame(Aspect.pred)
occ.covs.pred.quad$Precipit.pred <- Precipit.pred 



occ.covs.pred <- as.matrix.data.frame(occ.covs.pred)
occ.covs.pred.quad <- as.matrix.data.frame(occ.covs.pred.quad)
str(occ.covs.pred)
str(occ.covs.pred.quad)

# These are the new intercept and covariate data.
curr.bird <- 'Taita Thrush'
y.ssom <- y[which(sp.codes == curr.bird), , ]
str(y.ssom)

# Format with explicit specification of inits for alpha and beta
# with four detection parameters and three occurrence parameters 
# (including the intercept).
oven.inits <- list(alpha = c(0, 0, 0, 0), 
                   beta = c(0, 0, 0), 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))
# Format with abbreviated specification of inits for alpha and beta.
oven.inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(y.ssom, 1, max, na.rm = TRUE))

#will be using the default priors of spOccupancy - specifies for clarity down below
oven.priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

#specify the number of samples for the MCMC algorithm
n.samples <- nrow(Taita_Data_Sp) 
n.burn <- round(.10 * n.samples)
n.thin <- 2
n.chains <- 3

#data
data.ssom <- list(y = y.ssom, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs,
                  coords = coords)


#MODEL C. with only EVIjan_july_ min max and C.heihgt min max - 1min to run

out.ssom1.1 <- PGOcc(occ.formula = ~ scale(C.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVI) + scale(Precipit.wett) + I(scale(Precipit.wett)^2), 
                     det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
                     data = data.ssom,
                     inits = oven.inits,
                     n.samples = n.samples,
                     priors= oven.priors,
                     n.omp.threads = 1,
                     verbose = FALSE,
                     n.report = 1000, 
                     n.burn = 3000,
                     n.thin = n.thin,
                     n.chains = n.chains
) 
plogis(-0.53)
summary(out.ssom1.1)
X.0 <- cbind(1, occ.covs.pred, occ.covs.pred.quad, occ.covs.pred.quad^2)  #X.0 must have 10 columns 
out.pred <- predict(out.ssom1.1, X.0) #run time : 3min 40



#run time : 5min 50
plot.dat <- data.frame(x = Slope.df$x, #coordinates are the same for all environmental values so we can use whichever as reference coords
                       y = Slope.df$y, 
                       mean.psi = apply(out.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.pred$psi.0.samples, 2, sd), 
                       stringsAsFactors = FALSE)

# Make a species distribution map showing the point estimates,
# or predictions (posterior means)
dat.stars <- st_as_stars(plot.dat, dims = c('x', 'y'))
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = mean.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Probability', 
       title = 'Mean Taita Thrush occurrence probability with average EVI and averge C.height scenario') +
  theme_bw()


# Map of the associated uncertainty of these predictions
# (i.e., posterior sds)
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = sd.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Uncertainity', 
       title = 'SD Taita Thrush occurrence probability with average EVI and average C.height scenario') +
  theme_bw()






# Fitting a Spacial occupancy model 
#create matrix with J rows (number of sites) and 2 columns with horizontal component (easting) in the first column and the vertical component (northing) in the second column

library(sf)
library(raster)
library(dplyr)

occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Euclidean_dist_occupied_forest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)

#coordinates needed for the model
occ.covs <- occ.covs %>%
  mutate(x= st_coordinates(occ.covs$geometry)[,"X"], y=  st_coordinates(occ.covs$geometry)[,"Y"])

coords <- occ.covs %>%
  dplyr::select(x,y)

coords<- as.matrix(coords)
str(coords)




curr.bird <- 'Taita Thrush'
y.ssom <- y[which(sp.codes == curr.bird), , ]
str(y.ssom)



#RUN SpOccupancy with coordinates

#package all data into list object 
data.ssom <- list(y = y.ssom, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs, 
                  coords = coords
) 
str(data.ssom)


#establishing phi
# Pair-wise distances between all sites
dist.occ.covs <- dist(data.ssom$coords)



# Exponential covariance model
cov.model <- "exponential"
# Specify list of inits
Thrush.inits <- list(alpha = 0, 
                     beta = 0, 
                     z = apply(data.ssom$y, 1, max, na.rm = TRUE), 
                     sigma.sq = 8, 
                     phi = 3 / mean(dist.occ.covs), 
                     w = rep(0, nrow(data.ssom$y)))


#As with all other parameters, it is recommended to use the default initial values for an initial model run,
#and if the model is taking a very long time to converge you can rerun the model with initial values based on the posterior 
#means of estimated parameters from the initial model fit


#establishing batch lenght 
#set batch.length = 25 and then play around with n.batch until convergence of all model parameters is reached.


batch.length <- 25
n.batch <- 400
n.burn <- 2000
n.thin <- 20
n.chains <- 3

#establish an acceptance rate and a tuning parameter for the spatial range parameter (and spatial smoothness parameter if cov.model = 'matern'), which are both features of the adaptive algorithm we use to sample these parameters
#In this algorithm, we propose new values for phi (and nu), compare them to our previous values, and use a statistical algorithm to determine if we should accept the new proposed value or keep the old one. The accept.rate argument specifies the ideal proportion of times we will accept the newly proposed values for these parameters
#Roberts and Rosenthal (2009) show that if we accept new values around 43% of the time, this will lead to optimal mixing and convergence of the MCMC chains
#set accept rate = 0.43 (default value)

#The values specified in the tuning argument helps control the initial values we will propose for both phi and nu
#This initial tuning value is closely related to the ideal (or target) acceptance rate we specified in accept.rate
#set initial vaue to 0.5 - this is an adaptive algorithm it will change the tuning values after each batch of the MCMC to yield acceptance rates that are close to our target acceptance rate that we specified in the accept.rate argument
#after a few runs the tuning values should get closer to the target acceptance rate 
#this leads to much shorter run times due to the adaptive algorithm, compared to the other more simple approaches


Thrush.tuning <- list(phi = 1)
# accept.rate = 0.43 by default, so we do not specify it.


#Establishing priors (use default)
#inverse-Gamma prior on the spatial variance, we typically set the shape parameter to 2 and the scale parameter equal to our best guess of the spatial variance

min.dist <- min(dist.occ.covs)
max.dist <- max(dist.occ.covs)
Thrush.priors <- list(beta.normal = list(mean = 0, var = 2.72), 
                      alpha.normal = list(mean = 0, var = 2.72), 
                      sigma.sq.ig = c(2, 1), 
                      phi.unif = c(3/max.dist, 3/min.dist))

n.omp.threads <- 1
verbose <- TRUE
n.report <- 100 # Report progress at every 100th batch.

nrow(data.ssom$coords) != nrow(data.ssom$y)


#fit a spacially explicit single species occupancy model - for the best model identified in the single species model - Model E

out.sp <- spPGOcc(occ.formula = ~ scale(canopy.height) + scale(Aspect) + I(scale(Aspect)^2) + scale(EVIJan_july_max) + scale(Precipit.wettest) + I(scale(Precipit.wettest)^2), 
                  det.formula =  ~ scale(day) + I(scale(day)^2) + scale(tod),
                  data = data.ssom, 
                  inits = Thrush.inits, 
                  n.batch = n.batch, 
                  batch.length = batch.length, 
                  priors = Thrush.priors, 
                  cov.model = cov.model, 
                  NNGP = TRUE, 
                  n.neighbors = 5,
                  tuning = Thrush.tuning, 
                  n.report = n.report, 
                  n.burn = n.burn, 
                  n.thin = n.thin, 
                  n.chains = n.chains
)




class(out.sp) # Look at what spOccupancy produced.
names(out.sp)
summary(out.sp)  #Rhat values = greater than 1.1 # run with n.batch= 600 (with 600 sigma square is low but not Pastures), are Rhats are low a part from pastures
#summary(out.sp.jan_jul)  #Rhat values high - lower Rhat values 
#summary(out.sp.jul_dec)  #Rhat values high - highest Rhat values sigma square : 4.99


#most RHat values are more than 1.1 which means that 
#the effective sample size of Spacial covaraince is high which indicates limited mixing of the parameters 


#Posterior predictive check using 
ppc.sp.out <- ppcOcc(out.sp, fit.stat = 'freeman-tukey', group = 2)  #the model fails to adequately represent variation in detection probability across the different replicate surveys
summary(ppc.sp.out) 

#Posterior predictive check using 
ppc.sp.out1 <- ppcOcc(out.sp, fit.stat = 'freeman-tukey', group = 1)
summary(out.sp)




#The Bayesian p-value is very low (0.0042): p-value is low, reject null hypothesis, meaning the test finds evidence that the quadratic term might not be 0, meaning there is strong evidence of non-linearity in the model and it needs to be re-specified before we can trust the results of the model.
#for sigma sq. = 2, p-value: 0.0051
#for sigma sq. = 6, p-value: 0.005
#for sigma sq. = 4, p-value: 0.0051
#for sigma sq. = 7, p-value: 0.0046 
#for sigma sq. = 8, p-value: 0.0021
#for sigma sq. = 12, p-value: 0.0056 

#posterior check for non spcail model


#Model selection using WAIC
waicOcc(out.sp)         #elpd: -782.89467 , pD : 92.89882, WAIC : 1751.58698   

#batch no = 600 and sigma squared = 2, elpd: -769.79216 , pD : 92.93463, WAIC : 1712.62304
#batch no = 600 and sigma squared = 4, elpd: -764.40691 , pD : 92.04078, WAIC : 1712.89538 
#batch no = 600 and sigma squared = 6, elpd: -761.8410 , pD : 92.0166, WAIC : 1707.7152 - best model 
#batch no = 600 and sigma squared = 7, elpd: -762.26096 , pD : 92.83657, WAIC : 1710.19506 
#batch no = 600 and sigma squared = 8, elpd: -765.87157 , pD : 93.45589, WAIC : 1718.65492 
#batch no = 600 and sigma squared = 12, elpd: -766.60634 , pD : 93.36875, WAIC : 1719.95017 



#Visualisation throughout sites
ppc.df <- data.frame(fit = ppc.sp.out1$fit.y, 
                     fit.rep = ppc.sp.out1$fit.y.rep, 
                     color = 'lightskyblue1')
ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21, 
     ylab = 'Fit', xlab = 'True')
lines(ppc.df$fit, ppc.df$fit, col = 'black')
#yes lack of fit + most of the fit statistics are smaller for the replicate data than the actual data set


#Visualisation throught replicates
ppc.df <- data.frame(fit = ppc.sp.out1$fit.y, 
                     fit.rep = ppc.sp.out1$fit.y.rep, 
                     color = 'lightskyblue1')
ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21, 
     ylab = 'Fit', xlab = 'True')
lines(ppc.df$fit, ppc.df$fit, col = 'black')
#yes lack of fit + most of the fit statistics are smaller for the replicate data than the actual data set



#Assess convergence 
plot(out.sp, 'beta', density = FALSE) # Occupancy parameters.
plot(out.sp, 'alpha', density = FALSE) # Detection parameters.

#occupancy covariate effect
install.packages("mcmcplots")
install.packages("MCMCvis")
library(MCMCvis)

quartz()
MCMCplot(out.sp$alpha.samples,ref_ovl= TRUE, ci = c(50,95)) #None of the covariates overlaps 0 - pretty accurate occupancy covariates
MCMCplot(out.sp$beta.samples,ref_ovl= TRUE, ci = c(50,95)) #all detection covariates oveerlap 0 - inaccurate covariates 



#viualising discrepancies across sites - multiple points have importnat contributions to the overall GoF measure
diff.fit <- out.sp.jan_jul$fit.y.rep.group.quants[3, ] - out.sp.jan_jul$fit.y.group.quants[3, ]
plot(diff.fit, pch = 19, xlab = 'Site ID', ylab = 'Replicate - True Discrepancy')
# might be due to extraordinary local habitat that is not adequately described by the occurrence covariates?


#viualising discrepancies across replicates
diff.fit <- out.sp.jan_jul$fit.y.rep.group.quants[3, ] - out.sp.jan_jul$fit.y.group.quants[3, ]
plot(diff.fit, pch = 19, xlab = 'Replicates', ylab = 'Replicate - True Discrepancy')
# one point has importnat contributions to the overall GoF measure - replicate 9




#PREDICTION


#the following code is not polished for the right predictors as this model was dropped from the analysis due to lack of MCMC chin cnovergence and highly significant freeman-tuckey p-value

#canopy height

C.height.df <- raster::as.data.frame(CanopyHeight.1,xy=TRUE)
str(C.height.df)
C.height.df$GLAD_SAFR_30m_UTM37S[is.na(C.height.df$GLAD_SAFR_30m_UTM37S)] <- 0
str(C.height.df)

#precipit wettest

Precwet.df <- raster::as.data.frame(WetQuart.1,xy=TRUE)
str(Precwet.df)
Precwet.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m[is.na(Precwet.df$WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m)] <- 0
str(Precwet.df)

#precipit driest

Precdry.df <- raster::as.data.frame(DryQuart.1,xy=TRUE)
str(Precdry.df)
Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m[is.na(Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m)] <- 0
str(Precwet.df)

#Aspect

Aspect.df <- raster::as.data.frame(Aspect.1,xy=TRUE)
str(Aspect.df)
Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S[is.na(Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S)] <- 0
str(Aspect.df)

#ELEVATION

Elevation.df <- raster::as.data.frame(Elevation.1,xy=TRUE)
str(Elevation.df)
Elevation.df$filea180a29cbb7[is.na(Elevation.df$filea180a29cbb7)] <- 0
str(Elevation.df)

#slope

Slope.df <- raster::as.data.frame(Slope.1,xy=TRUE)
str(Slope.df)
Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S[is.na(Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S)] <- 0
str(Slope.df)


#background max EVIjan_jul_max 

raster.max_jan_jul_background.df <- raster::as.data.frame(raster.max_jan_jul.1,xy=TRUE)
str(raster.max_jan_jul_background.df)
raster.max_jan_jul_background.df$index_1[is.na(raster.max_jan_jul_background.df$index_1)] <- 0
str(raster.max_jan_jul_background.df)



#background max EVIjuly_dec_max 

raster.max_jul_dec_background.df <- raster::as.data.frame(raster.max_jul_dec.1,xy=TRUE)
str(raster.max_jul_dec_background.df)
raster.max_jul_dec_background.df$index_1[is.na(raster.max_jul_dec_background.df$index_1)] <- 0
str(raster.max_jul_dec_background.df)


#Given that we standardized the elevation values when we fit the model, we need to standardize the average temperature values for prediction using the exact same values of the mean and standard deviation of the average temp values used to fit the data.
canopy.height.pred <- (C.height.df$GLAD_SAFR_30m_UTM37S - mean(occ.covs[, 4])) / sd(occ.covs[, 4])
precipit.dry.pred <- (Precdry.df$WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m - mean(occ.covs[, 6])) / sd(occ.covs[, 6])
Aspect.pred <- (Aspect.df$NASA_DEM_HGT001_Aspect_30m_UTM37S - mean(occ.covs[, 8])) / sd(occ.covs[, 8])
EVIjan_jul.pred <- (raster.max_jan_jul_background.df$index_1 - mean(occ.covs[, 11])) / sd(occ.covs[, 11])
EVIjul_dec.pred <- (raster.max_jul_dec_background.df$index_1 - mean(occ.covs[, 12])) / sd(occ.covs[, 12])
Elevation.pred <- (Elevation.df$filea180a29cbb7 - mean(occ.covs[, 9])) / sd(occ.covs[, 9])
slope.pred <- (Slope.df$NASA_DEM_HGT_Slope_30m_UTM37S - mean(occ.covs[, 2])) / sd(occ.covs[, 2])

#creating a data frame for all standardised values 
occ.covs.pred <- as.data.frame(canopy.height.pred)
occ.covs.pred$EVIjan_jul <- EVIjan_jul.pred

#occ.pred.quadrat
occ.covs.pred.quad <- as.data.frame(Aspect.pred)
occ.covs.pred.quad$Elevation.pred <- Elevation.pred 
occ.covs.pred.quad$slope.pred <- slope.pred 

occ.covs.pred <- as.matrix.data.frame(occ.covs.pred)
occ.covs.pred.quad <- as.matrix.data.frame(occ.covs.pred.quad)
str(occ.covs.pred)
str(occ.covs.pred.quad)


# These are the new intercept and covariate data.

X.0 <- cbind(1, occ.covs.pred, occ.covs.pred.quad, occ.covs.pred.quad^2)  #X.0 must have 10 columns 

# Do prediction. 
coords.0 <- as.matrix(C.height.df [, c('x', 'y')])
# Approx. run time: 6 min
out.sp.pred <- predict(out.sp.jan_jul, X.0, coords.0, verbose = FALSE)
str(out.sp.pred)
# Produce a species distribution map (posterior predictive means of occupancy)
plot.dat <- data.frame(x = C.height.df$x, 
                       y = C.height.df$y, 
                       mean.psi = apply(out.sp.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.sp.pred$psi.0.samples, 2, sd))
library(stars)
library(ggplot2)
dat.stars <- st_as_stars(plot.dat, dims = c('x', 'y'))
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = mean.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') + 
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Mean Taita Thrush occurrence probability') +
  theme_bw()


# Map of the associated uncertainty of these predictions
# (i.e., posterior sds)
ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, fill = sd.psi)) +
  scale_fill_viridis_c(na.value = 'transparent') +
  labs(x = 'Easting', y = 'Northing', fill = 'Uncertainity', 
       title = 'SD Taita Thrush occurrence probability') +
  theme_bw()





