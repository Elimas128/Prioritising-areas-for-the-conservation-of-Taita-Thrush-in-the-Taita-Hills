# Pull in Spatial data and stack for Taita Hills service call log 3525
# 01st Feb 2024
# Penny Gardner

# call in libraries
library(raster)
library(terra)

# create directory where all rasters are stored
localDir = file.path("/Users/elisa/Library/Mobile Documents/com~apple~CloudDocs/Edinburgh University/Taita Hills bird conservation") # Change this to your directory
Rasters.directory = file.path(localDir, "Remote sensing data")

Slope <- raster(file.path(Rasters.directory, "Slope_NASA_DEM_30m/NASA_DEM_HGT_Slope_30m_UTM37S.tif"))
Aspect <- raster(file.path(Rasters.directory, "Aspect_NASA_DEM_30m/NASA_DEM_HGT001_Aspect_30m_UTM37S.tif"))
CanopyHeight <- raster(file.path(Rasters.directory, "GLAD_SAFR_30m/GLAD_SAFR_30m_UTM37S.tif"))
MaxTemp <- raster(file.path(Rasters.directory, "WorldClimBio_MaxTemp/WorldClimBio_Max_temp_warmest_mnth_UTM37S_30m.tif"))
MinTemp <- raster(file.path(Rasters.directory, "WorldClimBio_MinTemp/WorldClimBio_927.67m_Min_temp_coldest_mnth_UTM37S_Rescaled_30m.tif"))
DryQuart <- raster(file.path(Rasters.directory, "WorldClimBio_DriestQu/WorldClimBio_927_67m_Precip_Driest_Quarter_UTM37S_30m.tif"))
WetQuart <- raster(file.path(Rasters.directory, "WorldClimBio_WettestQ/WorldClimBio_927_67m_Precip_Wettest_Quarter_UTM37S_30m.tif"))
VV <- raster(file.path(Rasters.directory, "S1_SAR_VV_30m/VV_AscDesc_Mean_30m_UTM37S.tif"))
VH <- raster(file.path(Rasters.directory, "S1_SAR_VH_30m/VH_AscDesc_Mean_30m_UTM37S.tif"))
Dist <- raster(file.path(Rasters.directory, "Dist_Occupied_Forest_30m/Euclidean_Distance_OccupiedForest_30m_UTM37S.tif"))

# EVI rasters -- two per year (6 months) -- Jan 01st to Jul 2nd & Jul 3rd to Dec 31st for each year 2009 to 2022
# Note no data available for Jul-Dec 22. Cloud cover in some rasters which will influence EVI
Jan_Jul09_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2009_L5_EVI_30m.tif"))
Jul_Dec09_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2009_L5_EVI_30m.tif"))
Jan_Jul10_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2010_L5_EVI_30m.tif"))
Jul_Dec10_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2010_L5_EVI_30m.tif"))
Jan_Jul11_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2011_L5_EVI_30m.tif"))
Jul_Dec11_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2011_L5_EVI_30m.tif"))
Jan_Jul12_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2012_L7_EVI_30m.tif"))
Jul_Dec12_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2012_L7_EVI_30m.tif"))
Jan_Jul13_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2013_L7_EVI_30m.tif"))
Jul_Dec13_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2013_L7_EVI_30m.tif"))
Jan_Jul14_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2014_L7_EVI_30m.tif"))
Jul_Dec14_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2014_L7_EVI_30m.tif"))
Jan_Jul15_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2015_L7_EVI_30m.tif"))
Jul_Dec15_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2015_L7_EVI_30m.tif"))
Jan_Jul16_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2016_L7_EVI_30m.tif"))
Jul_Dec16_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2016_L7_EVI_30m.tif"))
Jan_Jul17_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2017_L7_EVI_30m.tif"))
Jul_Dec17_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2017_L7_EVI_30m.tif"))
Jan_Jul18_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2018_L7_EVI_30m.tif"))
Jul_Dec18_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2018_L7_EVI_30m.tif"))
Jan_Jul19_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2019_L7_EVI_30m.tif"))
Jul_Dec19_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2019_L7_EVI_30m.tif"))
Jan_Jul20_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2020_L7_EVI_30m.tif"))
Jul_Dec20_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2020_L7_EVI_30m.tif"))
Jan_Jul21_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2021_L7_EVI_30m.tif"))
Jul_Dec21_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jul_Dec_2021_L7_EVI_30m.tif"))
Jan_Jul22_EVI <- raster(file.path(Rasters.directory, "EVI_Landsat5-7-9_30m_UTM37S/3525_Jan_Jul_2022_L9_EVI_30m.tif"))

# Resample to match the same raster grid of Slope using Bilinear method

Slope.1 <- aggregate(Slope, fact=10)

Aspect.1 = resample(Aspect, Slope, method = "bilinear")
Aspect.1 <- aggregate(Aspect.1, fact=10)

CanopyHeight.1 = resample(CanopyHeight, Slope, method = "bilinear") # Resampling using Bilinear causes slight change in the min & max values ~aprox 1m, changing the min from 0 to -1.
CanopyHeight.1 <- aggregate(CanopyHeight.1, fact=10)
res(CanopyHeight.1) #298.8298 x 298.8298
plot(CanopyHeight.1)

MaxTemp.1 = resample(MaxTemp, Slope, method = "bilinear") #no use
MaxTemp.1 <- aggregate(MaxTemp.1, fact=10)

MinTemp.1 = resample(MinTemp, Slope, method = "bilinear") #no use
MinTemp.1 <- aggregate(MinTemp.1, fact=10)

DryQuart.1 = resample(DryQuart, Slope, method = "bilinear")
DryQuart.1 <- aggregate(DryQuart.1, fact=10)

WetQuart.1 = resample(WetQuart, Slope, method = "bilinear")
WetQuart.1 <- aggregate(WetQuart.1, fact=10)

VV.1 = resample(VV, Slope, method = "bilinear") # no use
VH.1 = resample(VH, Slope, method = "bilinear") # no use 
Dist.1 = resample(Dist, Slope, method = "bilinear")

Jan_Jul09_EVI.1 = resample(Jan_Jul09_EVI, Slope, method = "bilinear")
Jan_Jul09_EVI.1 <- aggregate(Jan_Jul09_EVI.1, fact=10)
res(Jan_Jul09_EVI.1) #298.8298 x 298.8298
plot(Jan_Jul09_EVI.1)

Jul_Dec09_EVI.1 = resample(Jul_Dec09_EVI, Slope, method = "bilinear")
Jul_Dec09_EVI.1 <- aggregate(Jul_Dec09_EVI.1, fact=10)
plot(Jul_Dec09_EVI.1)

Jan_Jul10_EVI.1 = resample(Jan_Jul10_EVI, Slope, method = "bilinear")
Jan_Jul10_EVI.1 <- aggregate(Jan_Jul10_EVI.1, fact=10)


Jul_Dec10_EVI.1 = resample(Jul_Dec10_EVI, Slope, method = "bilinear")
Jul_Dec10_EVI.1 <- aggregate(Jul_Dec10_EVI.1, fact=10)


Jan_Jul11_EVI.1 = resample(Jan_Jul11_EVI, Slope, method = "bilinear")
Jan_Jul11_EVI.1 <- aggregate(Jan_Jul11_EVI.1, fact=10)


Jul_Dec11_EVI.1 = resample(Jul_Dec11_EVI, Slope, method = "bilinear")
Jul_Dec11_EVI.1 <- aggregate(Jul_Dec11_EVI.1, fact=10)


Jan_Jul12_EVI.1 = resample(Jan_Jul12_EVI, Slope, method = "bilinear")
Jan_Jul12_EVI.1 <- aggregate(Jan_Jul12_EVI.1, fact=10)


Jul_Dec12_EVI.1 = resample(Jul_Dec12_EVI, Slope, method = "bilinear")
Jul_Dec12_EVI.1 <- aggregate(Jul_Dec12_EVI.1, fact=10)


Jul_Dec13_EVI.1 = resample(Jul_Dec13_EVI, Slope, method = "bilinear")
Jul_Dec13_EVI.1 <- aggregate(Jul_Dec13_EVI.1, fact=10)


Jan_Jul13_EVI.1 = resample(Jan_Jul13_EVI, Slope, method = "bilinear")
Jan_Jul13_EVI.1 <- aggregate(Jan_Jul13_EVI.1, fact=10)


Jan_Jul14_EVI.1 = resample(Jan_Jul14_EVI, Slope, method = "bilinear")
Jan_Jul14_EVI.1 <- aggregate(Jan_Jul14_EVI.1, fact=10)


Jul_Dec14_EVI.1 = resample(Jul_Dec14_EVI, Slope, method = "bilinear")
Jul_Dec14_EVI.1 <- aggregate(Jul_Dec14_EVI.1, fact=10)


Jan_Jul15_EVI.1 = resample(Jan_Jul15_EVI, Slope, method = "bilinear")
Jan_Jul15_EVI.1 <- aggregate(Jan_Jul15_EVI.1, fact=10)


Jul_Dec15_EVI.1 = resample(Jul_Dec15_EVI, Slope, method = "bilinear")
Jul_Dec15_EVI.1 <- aggregate(Jul_Dec15_EVI.1, fact=10)


Jan_Jul16_EVI.1 = resample(Jan_Jul16_EVI, Slope, method = "bilinear")
Jan_Jul16_EVI.1 <- aggregate(Jan_Jul16_EVI.1, fact=10)

Jul_Dec16_EVI.1 = resample(Jul_Dec16_EVI, Slope, method = "bilinear")
Jul_Dec16_EVI.1 <- aggregate(Jul_Dec16_EVI.1, fact=10)

Jan_Jul17_EVI.1 = resample(Jan_Jul17_EVI, Slope, method = "bilinear")
Jan_Jul17_EVI.1 <- aggregate(Jan_Jul17_EVI.1, fact=10)

Jul_Dec17_EVI.1 = resample(Jul_Dec17_EVI, Slope, method = "bilinear")
Jul_Dec17_EVI.1 <- aggregate(Jul_Dec17_EVI.1, fact=10)

Jan_Jul18_EVI.1 = resample(Jan_Jul18_EVI, Slope, method = "bilinear")
Jan_Jul18_EVI.1 <- aggregate(Jan_Jul18_EVI.1, fact=10)

Jul_Dec18_EVI.1 = resample(Jul_Dec18_EVI, Slope, method = "bilinear")
Jul_Dec18_EVI.1 <- aggregate(Jul_Dec18_EVI.1, fact=10)

Jan_Jul19_EVI.1 = resample(Jan_Jul19_EVI, Slope, method = "bilinear")
Jan_Jul19_EVI.1 <- aggregate(Jan_Jul19_EVI.1, fact=10)

Jul_Dec19_EVI.1 = resample(Jul_Dec19_EVI, Slope, method = "bilinear")
Jul_Dec19_EVI.1 <- aggregate(Jul_Dec19_EVI.1, fact=10)

Jan_Jul20_EVI.1 = resample(Jan_Jul20_EVI, Slope, method = "bilinear")
Jan_Jul20_EVI.1 <- aggregate(Jan_Jul20_EVI.1, fact=10)

Jul_Dec20_EVI.1 = resample(Jul_Dec20_EVI, Slope, method = "bilinear")
Jul_Dec20_EVI.1 <- aggregate(Jul_Dec20_EVI.1, fact=10)

Jan_Jul21_EVI.1 = resample(Jan_Jul21_EVI, Slope, method = "bilinear")
Jan_Jul21_EVI.1 <- aggregate(Jan_Jul21_EVI.1, fact=10)

Jul_Dec21_EVI.1 = resample(Jul_Dec21_EVI, Slope, method = "bilinear")
Jul_Dec21_EVI.1 <- aggregate(Jul_Dec21_EVI.1, fact=10)

Jan_Jul22_EVI.1 = resample(Jan_Jul22_EVI, Slope, method = "bilinear") #no use 
Jan_Jul22_EVI.1 <- aggregate(Jan_Jul22_EVI.1, fact=10)


# Stack rasters
Stack <- stack(Slope.1, Aspect.1, CanopyHeight.1, MaxTemp.1, MinTemp.1, DryQuart.1, WetQuart.1, VV.1, VH.1, Dist.1, Jan_Jul09_EVI.1, Jul_Dec09_EVI.1,
               Jan_Jul10_EVI.1, Jul_Dec10_EVI.1, Jan_Jul11_EVI.1, Jul_Dec11_EVI.1, Jan_Jul12_EVI.1, Jul_Dec12_EVI.1, Jan_Jul13_EVI.1, Jul_Dec13_EVI.1,
               Jan_Jul14_EVI.1, Jul_Dec14_EVI.1, Jan_Jul15_EVI.1, Jul_Dec15_EVI.1, Jan_Jul16_EVI.1, Jul_Dec16_EVI.1, Jan_Jul17_EVI.1, Jul_Dec17_EVI.1,
               Jan_Jul18_EVI.1, Jul_Dec18_EVI.1, Jan_Jul19_EVI.1, Jul_Dec19_EVI.1, Jan_Jul20_EVI.1, Jul_Dec20_EVI.1, Jan_Jul21_EVI.1, Jul_Dec21_EVI.1,
               Jan_Jul22_EVI.1)
#Assign names to stack layers
names(Stack) <- c("Slope", "Aspect", "CanopyHeight", "MaxTemp", "MinTemp", "DryQuart", "WetQuart", "VV", "VH", "Dist", "Jan_Jul09_EVI", "Jul_Dec09_EVI",
                  "Jan_Jul10_EVI", "Jul_Dec10_EVI", "Jan_Jul11_EVI", "Jul_Dec11_EVI", "Jan_Jul12_EVI", "Jul_Dec12_EVI", "Jan_Jul13_EVI", "Jul_Dec13_EVI",
                  "Jan_Jul14_EVI", "Jul_Dec14_EVI", "Jan_Jul15_EVI", "Jul_Dec15_EVI", "Jan_Jul16_EVI", "Jul_Dec16_EVI", "Jan_Jul17_EVI", "Jul_Dec17_EVI",
                  "Jan_Jul18_EVI", "Jul_Dec18_EVI", "Jan_Jul19_EVI", "Jul_Dec19_EVI", "Jan_Jul20_EVI", "Jul_Dec20_EVI", "Jan_Jul21_EVI", "Jul_Dec21_EVI",
                  "Jan_Jul22_EVI")

#taking the max values for EVIs
Max.jan_jul <- stack(Jan_Jul09_EVI.1,
               Jan_Jul10_EVI.1, Jan_Jul11_EVI.1, Jan_Jul12_EVI.1, Jan_Jul13_EVI.1,
               Jan_Jul14_EVI.1, Jan_Jul15_EVI.1, Jan_Jul16_EVI.1, Jan_Jul17_EVI.1,
               Jan_Jul18_EVI.1, Jan_Jul19_EVI.1, Jan_Jul20_EVI.1, Jan_Jul21_EVI.1)

Max.jul_dec <- stack(Jul_Dec09_EVI.1,
            Jul_Dec10_EVI.1, Jul_Dec11_EVI.1, Jul_Dec12_EVI.1, Jul_Dec13_EVI.1,
               Jul_Dec14_EVI.1, Jul_Dec15_EVI.1, Jul_Dec16_EVI.1, Jul_Dec17_EVI.1,
               Jul_Dec18_EVI.1, Jul_Dec19_EVI.1, Jul_Dec20_EVI.1, Jul_Dec21_EVI.1
                )

#selecting only the maximum value between all the years for jan_jul and Jul-dec froming one raster with max values (highet likelyhood that it is the most truthful value) - assuing EVI doent significanlty get affected by cloud cover change thoughout the years 
raster.max_jan_jul.1 <- stackApply(Max.jan_jul, indices = rep(1, nlayers(Max.jan_jul)), fun = max)
raster.max_jul_dec.1 <- stackApply(Max.jul_dec, indices = rep(1, nlayers(Max.jul_dec)), fun = max)

plot(raster.max_jan_jul.1)
plot(raster.max_jul_dec.1)
plot(CanopyHeight.1)
plot(Aspect.1)
plot(Slope.1)
plot(new_elev_raster)

#same thing
#no run calc(b, function(x){max(x)})

#Check names
names(Stack)

# write raster stack
terra::writeRaster(Stack, "/Users/elisa/Library/Mobile Documents/com~apple~CloudDocs/Edinburgh University/Taita Hills bird conservation/Remote sensing data/CombinedStack_30m/CombinedStack_30m.tif", overwrite=TRUE)

# Read it in
Stack <- raster::brick("/Users/elisa/Library/Mobile Documents/com~apple~CloudDocs/Edinburgh University/Taita Hills bird conservation/Remote sensing data/CombinedStack_30m/CombinedStack_30m.tif")

