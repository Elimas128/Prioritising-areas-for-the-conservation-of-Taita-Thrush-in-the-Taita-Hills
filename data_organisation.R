library(dplyr)
library(stringr)
library(raster)
library(terra)


# Correct spelling mistakes and making data omogeneous
#correcting data "opotunistic" to "opportunistic"
Taita_Data <- read.csv("data/Taita_Hills_Data.csv")
Taita_Data$Point.ID <- gsub("opotunistic", "opportunistic", Taita_Data$Point.ID)
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)

#making sure that all Poin.ID are the same in the format XX_XXX- replace any character that is not a letter or a digit with an underscore.
Taita_Data$Point.ID <- gsub("[^A-Za-z0-9]", "_", Taita_Data$Point.ID)

#Counting opportunistic sightings
#count occurrences of each value in the "SightingType" column
sighting_counts <- table(Taita_Data$Point.ID)
#Display the count of opportunistic observations
opportunistic_count <- sighting_counts["opportunistic"]
print(opportunistic_count) #92 opportunistic counts 

# Viewing what type of birds were opportunistically sighted 
#Filter the dataframe to include only opportunistic sightings
opportunistic_sightings <- subset(Taita_Data, Point.ID == "opportunistic")
#View the names of bird species for opportunistic sightings
opportunistic_species <- opportunistic_sightings$Species.Name
print(opportunistic_species)



#Excluding opportunistic sightings

# Exclude rows where "opportunistic" is already written
Taita_Data <- subset(Taita_Data, Point.ID != "opportunistic")
# Save the modified dataframe back to a CSV file
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)



#  Correct spelling mistakes of bird species 
# Calculate the count of each bird species
species_counts <- table(Taita_Data$Species.Name)
# Convert the counts to a dataframe - visulaise misspelled words 
species_counts_df <- as.data.frame(species_counts)
# Correct misspelled bird species names
Taita_Data$Species.Name <- gsub("African dusky Flycatcher", "African Dusky Flycatcher", Taita_Data$Species.Name)
Taita_Data$Species.Name <- gsub("Black -headed Apalis", "Black-headed Apalis", Taita_Data$Species.Name)
Taita_Data$Species.Name <- gsub("Blue-mantled crested flycatcher", "Blue-mantled Crested Flycatcher", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Common fiscal", "Common Fiscal", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Pale flycatcher", "Pale Flycatcher", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Rock martin", "Rock Martin", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Ruppell's robin-chat", "Ruppell's Robin-chat", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Yellow bishop", "Yellow Bishop", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("yellow-bellied Greenbul", "Yellow-bellied Greenbul", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Yellow-rumped seedeater", "Yellow-rumped Seedeater", Taita_Data$Species.Name, ignore.case = TRUE)
Taita_Data$Species.Name <- gsub("Yellow-throated Woodland warbler", "Yellow-throated Woodland Warbler", Taita_Data$Species.Name, ignore.case = TRUE)
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)

# NO RUN - double check
species_counts <- table(Taita_Data$Species.Name)
# Convert the counts to a dataframe - visulaise misspelled words 
species_counts_df <- as.data.frame(species_counts)

# remove species that have not been recorded as "None"
Taita_Data <- subset(Taita_Data, Species.Name != "None")
# Save the modified data frame back to a CSV file
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)
#remove "blank" species names
Taita_Data <- subset(Taita_Data, Species.Name != "")
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)


# NO RUN - double check
species_counts <- table(Taita_Data$Species.Name)
# Convert the counts to a dataframe - visulaise misspelled words 
species_counts_df <- as.data.frame(species_counts) #excluded "None"




# destacking observations - making 5 individuls in one observation as 5 individual observations 
# Install from CRAN
install.packages("tidyverse")
library(tidyr)
library(dplyr)


# Expand the rows based on the 'No.' column
Taita_Data <- Taita_Data %>%
  # Repeat each row 'No.' times
  uncount(No.) %>%
  # Replace 'No.' column values with 1
  mutate(No. = 1)
# write the expanded data frame to a new CSV file
write.csv(Taita_Data, "Taita_Data.csv", row.names = FALSE)




# Correcting the POINT IDs - MS_ into VU_

# Replace all "MS" with "VU" in the Point.ID column 
Taita_Data <- Taita_Data %>%
  mutate(Point.ID = gsub("MS", "VU", Point.ID))
#Make point.ID on same format
# Load the required libraries
library(dplyr)
library(stringr)

# Define a function to reformat Point.ID to the desired format
format_point_id <- function(id) {
  # Extract letters and numbers from the ID
  letters_part <- str_extract(id, "^[A-Za-z]+")
  numbers_part <- str_extract(id, "\\d+$")
  # Ensure letters part has exactly 2 characters
  if (nchar(letters_part) != 2) {
    letters_part <- str_pad(letters_part, 2, pad = "X")
  }
  # Ensure numbers part has exactly 3 digits
  if (nchar(numbers_part) != 3) {
    numbers_part <- str_pad(numbers_part, 3, pad = "0")
  }
  # Combine parts into the desired format
  formatted_id <- paste(letters_part, numbers_part, sep = "_")
  
  return(formatted_id)
}
# Apply the function to the Point.ID column
Taita_Data <- Taita_Data %>%
  mutate(Point.ID = sapply(Point.ID, format_point_id))




# Adding Coordinates of point counts

# Read the second CSV file conttaining coordinates
coordinates_data <- read.csv("data/Coordinates.csv")
# Adjust the format of GPS IDs in the second CSV file to match the first CSV file
coordinates_data$Point.ID <- gsub("[^A-Za-z0-9]", "_", coordinates_data$Point.ID)
#exclude the useless columns 
coordinates_data <- coordinates_data[, !names(coordinates_data) %in% c("time","sym","type","gpxx_WaypointExtension","wptx1_WaypointExtension","ctx_CreationTimeExtension","wkt_geom","ele")]
# Write the modified data frame to a new CSV file
write.csv(coordinates_data, "coordinates_data.csv", row.names = FALSE)
# Merge the two dataframes based on GPS IDs
Taita_Data <- merge(Taita_Data, coordinates_data, by = "Point.ID", all.x = TRUE)
# Write the merged dataframe to a new CSV file
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)
# Subset merged_data to exclude the "Waypoint extension" and more column
Taita_Data <- Taita_Data[, !names(Taita_Data) %in% c("GPS.ID","Notes","time","sym","type","gpxx_WaypointExtension","wptx1_WaypointExtension","ctx_CreationTimeExtension","wkt_geom")]
# Write the modified data frame to a new CSV file
write.csv(Taita_Data, "Taita_Data", row.names = FALSE)



#. NO RUN -See the distribution of Taita Thrush observations to determine raster square size 

# Filter the data frame to keep only the rows where the species column contains "Taita Thrush"
taita_thrush_occurrences <- Taita_Data[Taita_Data$Species.Name == "Taita Thrush", ]
# View the filtered data frame
print(taita_thrush_occurrences)
# Export filtered data back to CSV
write.csv(taita_thrush_occurrences, "Taita_Thrush_occurences.csv", row.names = FALSE) #there are 344 Taita Thrush occurences (discounting opportunistic oberveations)






# Adding dataset 2001-2017

Data2001_07 <- read.csv("data_2001_2017/Points_Counts_2001_2017.csv")

#Counting opportunistic sightings
#count occurrences of each value in the "SightingType" column
sighting_counts <- table(Taita_Data$Point.ID)
#Display the count of opportunistic observations
opportunistic_count <- sighting_counts["opportunistic"]
print(opportunistic_count) #NA opportunistic counts  - itsin point counts as OP_XX


#  Correct spelling mistakes of bird species 
# Calculate the count of each bird species
species_counts <- table(Data2001_07$Species)
# Convert the counts to a dataframe - visulaise misspelled words 
species_counts_df <- as.data.frame(species_counts)
# Correct misspelled bird species names - Taita thrush to Taita Thrush
Data2001_07$Species <- gsub("Taita thrush", "Taita Thrush", Data2001_07$Species)

# remove species that have not been recorded as "None"
Data2001_07 <- subset(Data2001_07, Species != "None")
#remove "blank" species names
Data2001_07 <- subset(Data2001_07, Species != "")
write.csv(Data2001_07, "Taita_Data_2001_2017.csv", row.names = FALSE)

# destacking observations - making 5 individuls in one observation as 5 individual observations 
# Install from CRAN
library(tidyr)
library(dplyr)

#remove all NAs in individula counts 
Data2001_07 <- Data2001_07 %>%
  filter(!is.na(N_Inds))

# Expand the rows based on the 'No.' column
Data2001_07 <- Data2001_07 %>%
  # Repeat each row 'No.' times
  uncount(N_Inds) %>%
  # Replace 'No.' column values with 1
  dplyr::mutate(N_Inds = 1)
# write the expanded data frame to a new CSV file
write.csv(Data2001_07, "Taita_Data_2001_2017.csv", row.names = FALSE)

# Removing opportunistic counts 
Data2001_07 <- subset(Data2001_07, Opportunistic != "TRUE")

#Match data form suvery point habitat to point counts 
#Match : Point_ID with GPS_ID (in survey point habitat counts)

habitat2001_2017 <- read.csv("data_2001_2017/Survey_point_habitat_data_2001_2017.csv")
habitat2001_2017 <- habitat2001_2017[, !names(habitat2001_2017) %in% c("Hab_recorded_by","TAPres","TTPres","BHAPres","Nearest_gap_distance","Nearest_gap_diameter","CSC_N","CSC_SE","CSC_SW","CSC_SE","CHCK_N","CHCK_SE","CHCK_SW","AVG_CSC","AVG_CHK","RINGED","COLOR","Rings.combination","Height","Forest","Bush","TreePlant","Agric","Rocks","Seen_well")]

Data2001_07 <- merge(Data2001_07, habitat2001_2017, by = "Point_ID", all.x = TRUE)
Data2001_07 <- Data2001_07[, !names(Data2001_07) %in% c("Point_ID","Opportunistic","Species_ID")] 

Data2001_07 <- Data2001_07 %>% 
  mutate(Point.ID = Data2001_07$GPS_ID)
#remove unecessary columns
Data2001_07 <- Data2001_07[, !names(Data2001_07) %in% c("Species_ID","TA_Playback","TA_Playback.y","N_mins","Sex","Age","Cue_S.H","Distance","Direction","Nearest_gap_distance","Nearest_gap_diameter","CSC_N","CSC_SE","CSC_SW","CSC_SE","CHECK_N","CHECK_SE","CHECK_SW","AVG_CSC","AVG_CHK","RINGED","COLOR","Rings.combination","Height","Forest","Bush","TreePlant","Agric","Rocks","Seen_well")]

# Adding Coordinates of point counts

# Read the second CSV file conttaining coordinates
coordinates_data <- read.csv("data/Coordinates.csv")
# Adjust the format of GPS IDs in the second CSV file to match the first CSV file
coordinates_data$Point.ID <- gsub("[^A-Za-z0-9]", "_", coordinates_data$Point.ID)
#exclude the useless columns 
coordinates_data <- coordinates_data[, !names(coordinates_data) %in% c("time","sym","type","gpxx_WaypointExtension","wptx1_WaypointExtension","ctx_CreationTimeExtension","wkt_geom","ele")]

# Merge the two dataframes based on GPS ID
Data2001_07 <- merge(Data2001_07, coordinates_data, by = "Point.ID", all.x = TRUE)
# Subset merged_data to exclude the "Waypoint extension" and more column # Aslo only keep start_time.x

#Merging Data 2022-23 with 2001-2017
Data2001_07 <- Data2001_07 %>%
  dplyr::mutate(Species.Name = Data2001_07$Species, Start.Time = Data2001_07$Start_time.x, Detection.Time= Data2001_07$Detection_time, No. = Data2001_07$N_Inds, Observers =  Data2001_07$Team)

Data2001_07 <- Data2001_07[, !names(Data2001_07) %in% c("GPS_ID","Opport","Species","Start_time.x","Opport","Opportunistic","Start_time","Observations","Detection_time","N_Inds","Team","Observations.x","Observations.y","Start_time.y","Species_ID","TA_Playback.x", "TA_Playback","Point_ID","N_mins","Sex","Age","Cue_S.H","Distance","Direction","Nearest_gap_distance","Nearest_gap_diameter","CSC_N","CSC_SE","CSC_SW","CSC_SE","CHECK_N","CHECK_SE","CHECK_SW","AVG_CSC","AVG_CHK","RINGED","COLOR","Rings.combination","Height","Forest","Bush","TreePlant","Agric","Rocks","Seen_well","ele")]

install.packages("plyr")
library(plyr)
Taita_Data <- rbind.fill(Taita_Data,Data2001_07)

#Remove data from unknow plots

Taita_Data <- Taita_Data %>%
  filter(!is.na(latitude) & !is.na(longitude))







#. Adding environmental variables to Taita_Data
# Go to StackingRasterPredictors_30m to get the overall stack - run StackingRasterPredctors_30m.R

install.packages("sf")
install.packages("raster")
# Load required libraries
library(sf)
library(raster)

# Read surveying points from CSV into an sf object
survey_points <- st_read("coordinates_data.csv")
# Create spatial points from latitude and longitude columns
coordinates_sf <- st_as_sf(coordinates_data, coords = c("longitude", "latitude"), crs = 4326)
#Transform CRS of spatial points to match raster layer
coordinates_sf <- st_transform(coordinates_sf, crs(Stack))


# NO RUN Finding raster values corresponding to survey point coordinates for each environmental layer - run StackingRasterPredictors_30m.R  

# Visualise CRS
# Get the CRS of the raster layer
raster_crs <- crs(CanopyHeight.1)
# Print the CRS information ??? which one to pick?
print(raster_crs) #  in geospatial distance


#Canopy height
class(CanopyHeight.1)
Canopy_height <- raster::extract(CanopyHeight.1, coordinates_sf)
#Aspect
Aspect <- raster::extract(Aspect.1, coordinates_sf)
#Euclidean distance to occupied forest fragment
Euclidean_dist_occupied_forest <- raster::extract(Dist.1, coordinates_sf)
#Precipitation of driest quarter 
Precipit_driest <- raster::extract(DryQuart.1, coordinates_sf)
#Precipitation of wettest quarter 
Precipit_wettest <- raster::extract(WetQuart.1, coordinates_sf)
#Max temp
Max_temp <- raster::extract(MaxTemp.1, coordinates_sf)
#Min temp 
Min_temp <- raster::extract(MinTemp.1, coordinates_sf)
#slope
slope <- raster::extract(Slope, coordinates_sf)
#VH Mean ascending & descending backscatter
VH <- raster::extract(VH.1, coordinates_sf)
#VV Mean ascending & descending backscatter
VV <- raster::extract(VV.1, coordinates_sf)
#EVI Jan-July 2009 
EVI09 <- raster::extract(Jan_Jul09_EVI.1, coordinates_sf)
#EVI Jan-July 2010 
EVI10 <- raster::extract(Jan_Jul10_EVI.1, coordinates_sf)
#EVI Jan-July 2011
EVI11 <- raster::extract(Jan_Jul11_EVI.1, coordinates_sf)
#EVI Jan-July 2012 
EVI12 <- raster::extract(Jan_Jul12_EVI.1, coordinates_sf)
#EVI Jan-July 2013 
EVI13 <- raster::extract(Jan_Jul13_EVI.1, coordinates_sf)
#EVI Jan-July 2014 
EVI14 <- raster::extract(Jan_Jul14_EVI.1, coordinates_sf)
#EVI Jan-July 2015 
EVI15 <- raster::extract(Jan_Jul15_EVI.1, coordinates_sf)
#EVI Jan-July 2016 
EVI16 <- raster::extract(Jan_Jul16_EVI.1, coordinates_sf)
#EVI Jan-July 2017 
EVI17 <- raster::extract(Jan_Jul17_EVI.1, coordinates_sf)
#EVI Jan-July 2018 
EVI18 <- raster::extract(Jan_Jul18_EVI.1, coordinates_sf)
#EVI Jan-July 2019 
EVI19 <- raster::extract(Jan_Jul19_EVI.1, coordinates_sf)
#EVI Jan-July 2020 
EVI20 <- raster::extract(Jan_Jul20_EVI.1, coordinates_sf)
#EVI Jan-July 2021 
EVI21 <- raster::extract(Jan_Jul21_EVI.1, coordinates_sf)
#EVI Jul-Dec 2009 
EVI09_jul <- raster::extract(Jul_Dec09_EVI.1, coordinates_sf)
#EVI Jul-Dec 2010
EVI10_jul <- raster::extract(Jul_Dec10_EVI.1, coordinates_sf)
#EVI Jul-Dec 2011 
EVI11_jul <- raster::extract(Jul_Dec11_EVI.1, coordinates_sf)
#EVI Jul-Dec 2012 
EVI12_jul <- raster::extract(Jul_Dec12_EVI.1, coordinates_sf)
#EVI Jul-Dec 2013 
EVI13_jul <- raster::extract(Jul_Dec13_EVI.1, coordinates_sf)
#EVI Jul-Dec 2014 
EVI14_jul <- raster::extract(Jul_Dec14_EVI.1, coordinates_sf)
#EVI Jul-Dec 2015 
EVI15_jul <- raster::extract(Jul_Dec15_EVI.1, coordinates_sf)
#EVI Jul-Dec 2016 
EVI16_jul <- raster::extract(Jul_Dec16_EVI.1, coordinates_sf)
#EVI Jul-Dec 2017 
EVI17_jul <- raster::extract(Jul_Dec17_EVI.1, coordinates_sf)
#EVI Jul-Dec 2018 
EVI18_jul <- raster::extract(Jul_Dec18_EVI.1, coordinates_sf)
#EVI Jul-Dec 2019 
EVI19_jul <- raster::extract(Jul_Dec19_EVI.1, coordinates_sf)
#EVI Jul-Dec 2020 
EVI20_jul <- raster::extract(Jul_Dec20_EVI.1, coordinates_sf)
#EVI Jul-Dec 2021 
EVI21_jul <- raster::extract(Jul_Dec21_EVI.1, coordinates_sf)

#Exrtact Max value EVI 

EVIJan_july_max <- raster::extract(raster.max_jan_jul.1, coordinates_sf)
EVIjul_dec_max <- raster::extract(raster.max_jul_dec.1, coordinates_sf)
hist(EVIJan_july_max)
hist(det.covs$day)
hist(det.covs$tod)
mean(det.covs$day)

#Aspect 
aspect <- raster::extract(Aspect.1, coordinates_sf)
#Elevation
#need to extract from web
install.packages("elevatr")
library(elevatr)

#use Stack or Slope as teplate raster layer  #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
elevation <- get_elev_raster(Slope, z = 9)
plot(elevation)
#resample to match the same raster grid of Slope using Bilinear method
Elevation.1 = resample(elevation, Slope, method = "bilinear")
plot(Elevation.1)
Elevation.1 <- aggregate(Elevation.1, fact=10)
plot(Elevation.1)

elevation <- raster::extract(Elevation.1, coordinates_sf)


#Adding Cropland and Pastures from Google Earth Engine + extract coordinate values 
#ATTENTIO! very low resolution
Crops <- raster(file.path(Rasters.directory, "af-croplands-geotif/af_cropland.tif"))
Pastures <- raster(file.path(Rasters.directory, "af-pastures-geotif/af_pasture.tif"))

#setting CRS to slope CRS
crs(Slope)

Crops <- projectRaster(Crops, 
                       crs = Slope)
Pastures <- projectRaster(Pastures, 
                          crs = Slope)

# Resample to match the same raster grid of Slope using Bilinear method
Crops.1 = resample(Crops, Slope, method = "bilinear")
Crops.1 <- aggregate(Crops.1, fact=10)

Pastures.1 = resample(Pastures, Slope, method = "bilinear")
Pastures.1 <- aggregate(Pastures.1, fact=10)


#extract coordinates
Crops <- raster::extract(Crops.1, coordinates_sf)
Pastures <- raster::extract(Pastures.1, coordinates_sf)





# Combine all the extracted values into a single data frame
extracted_values_df <- data.frame(
  Canopy_height = Canopy_height,
  Aspect = Aspect,
  Precipit_driest = Precipit_driest,
  Precipit_wettest = Precipit_wettest,
  Euclidean_dist_occupied_forest = Euclidean_dist_occupied_forest,
  Max_temp = Max_temp,
  Min_temp = Min_temp,
  VV = VV,
  VH = VH,
  slope = slope,
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
  elevation = elevation
)

# Combine the coordinates with the extracted values
survey_points_with_environmental_values <- cbind(coordinates_sf, extracted_values_df)
# Print the first few rows of the combined data frame
print(head(survey_points_with_environmental_values))

#Match survey_points_with_environmental_values with Taita Thrush data and merge into unique dataset 
Taita_Data_env <- merge(Taita_Data, survey_points_with_environmental_values, by = "Point.ID", all.x = TRUE)



# Merging max temp with min temperature to get a yearly average 
library(dplyr)
Taita_Data_env <- Taita_Data_env %>%
  mutate(avg_temp = (Max_temp + Min_temp) / 2)
#remove Max_temp and Min_temp 
Taita_Data_env <- Taita_Data_env[, !names(Taita_Data_env) %in% c("Max_temp","Min_temp")]

Taita_Data_env<- Taita_Data_env %>%
  mutate(Fragment = gsub("Yale", "Iyale", Fragment))

#Precipit_driest and Precipit_wettest - can be limiting fctors - to keep

write.csv(Taita_Data_env, "Taita_Data_env.csv", row.names = FALSE)


