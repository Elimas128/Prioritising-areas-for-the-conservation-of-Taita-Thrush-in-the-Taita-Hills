# Predictor analysis 
# tetsing for multiccolinarity between predictors 

# pairs() cor() (overdispersion watch out)

install.packages("GGally")
library(GGally)

#create new column with presence of Taita Thrush as 1 and Absence as 0
Taita_Data_Sp <- Taita_Data_Sp %>%
  mutate(Presence_Absence_T.Thrush = ifelse(Species == "Taita Thrush", 1, 0))


T.Thrush_presence_absence <- Taita_Data_Sp %>%
  dplyr::select(Presence_Absence_T.Thrush) %>%
  distinct(Taita_Data_Sp$Plot, .keep_all = TRUE)
  

T.Thrush_presence_absence <- T.Thrush_presence_absence[, !names(T.Thrush_presence_absence) %in% ("Taita_Data_Sp$Plot")]

# original occ covs
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Euclidean_dist_occupied_forest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)

occ.covs$Presence_Absence_T.Thrush <- T.Thrush_presence_absence

#from here
occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","x","y","Euclidean_dist_occupied_forest")]
str(occ.covs)


#need both ggplot2 and GGally packages loaded to use ggpairs()
quartz()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")

#or
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(occ.covs, histogram = TRUE, method = "pearson")
plot(chart.Correlation)


# measures bethween 0.5 and 0.7 means variables are moderately correlated
# measures between 0.7 and 1 means variables are highly correlated
# 0.9 to 1.0 very highly correalted 

# in this analysis we consider multicllinearity if r> 0.7
#slope: correaltion under 0.5 = no multicollinearity
#avg_temp : high correlation with 3 predictors (3/10) - highly correlated to: precipit wettest, driest, elevation  (very low scatter, no over-dispersion)
#Canopy height: high correlation with none - no multicollinearity 
#Precipi_wettest : 3/10 highly correlated- avg temp, elevation and prcipit.driest (very low scatter, strong correlation)
#Precipit_driest: 3/10 highly correlated - avg temp, elevation, precipit wettest (very low scatter, strong correlation)
#aspect: no multicollinearity with any variables - ;oght correlation woth EVIjan_jul_max
#EVIjan_jul_max :  no multicollinearity with any variables, slight multicollinearity woth aspect
#EVIjul_dec_max :  no multicollinearity with any variables, slightmulticollinearity with EVIjan_jul_max
#elevation: high corr with avg temp, precipit wettest and driest

#there is one weird point in dist occ.forest - maybe this is causing the rectangular anomality in the graph? 


# original occ covs
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest,Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)

# DELETE: avg_temp


occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","avg_temp")]
str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")
#high correlation in precipit wettest and driest and elevation and aspect 



# DELETE: avg_temp + precipit wettest or driest depending on WAIC of model  - based on a better model with precipit driest: delete precipit wettest
# original occ covs
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest,Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)

occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","avg_temp","Precipit_driest","EVIjul_dec_max")]
str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")
#high correlation in Elevation and precipit driest 




# DELETE: avg_temp + precipit driest 
occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","avg_temp","Precipit_wettest")]
str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")

#elevation - higly correlated with precipitaation driest



# DELETE: avg_temp + precipit driest + elevation
# original occ covs
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)

occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","avg_temp","Precipit_driest", "Crops","elevation")]

str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
quartz()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")




# DELETE: avg_temp + prec wettest + driest
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)

occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","avg_temp","Precipit_wettest","Precipit_driest")]
str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")
#slight correlation between EVIJan_july_max and EVIjul_dec_max and them both with and aspect (decide which one to remove base on model accuracy k fold validation + WAIC)



# DELETE:  prec wettest + driest
occ.covs <- Taita_Data_Sp %>%
  dplyr::select(Plot, slope, avg_temp, Canopy_height, Precipit_wettest, Precipit_driest, Aspect, elevation, geometry, EVIJan_july_max, EVIjul_dec_max) %>%
  distinct(Plot, .keep_all = TRUE)
str(occ.covs)

occ.covs <- occ.covs[, !names(occ.covs) %in% c("Plot", "geometry","Presence_Absence_T.Thrush","Precipit_wettest","Precipit_driest")]
str(occ.covs)

# You need both ggplot2 and GGally packages loaded to use ggpairs()
ggpairs(occ.covs,
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of predictors")
