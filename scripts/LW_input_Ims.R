####################################################################################################################
# Code to calculate input from mass movements
# By Carina Helm, August 20, 2019
# Work-in-progress
####################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required pacakges ---------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("sf")
library("here")
library("dplyr")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and wrangle data --------------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slides <- read_sf(dsn = "data/raw/shapefiles", layer = "landslides")
basin_area <- read_sf(dsn = "data/raw/shapefiles", layer = "basin_area")
streams <- read_sf(dsn = "data/raw/shapefiles", layer = "study_reaches")

# include only relevent columns from VRI dataframe 
vri_col = c("FULL_LABEL","BASAL_AREA", "LBL_TEND","Q_DIAM_125", "Q_DIAM_175", "Q_DIAM_225", "PROJ_AGE_1","PROJ_HT_1","PROJ_HT_1", "LVLSP1_125",
               "LVLSP2_125", "N_LOG_DATE","REF_YR_ID", "ATRIB_DATE", "PROJ_DATE", "N_LOG_DIST","HRVSTDT" ,"SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2",
               "SPEC_PCT_2", "SPEC_CD_3", "SPEC_CD_3", "SPEC_PCT_3", "LIVE_STEMS","DEAD_STEMS", "BCLCS_LV_2", "BCLCS_LV_5", "LVLSP3_125", 
               "LVLTOT_125", "SPEC_PCT_1","SPEC_PCT_2", "SPEC_PCT_3", "SPEC_PCT_4", "P_HT_CAS_1", "P_HT_CAS_2")
VRI <- read_sf(dsn = "data/raw/shapefiles", layer = "VRI_nad83")
VRI <- VRI[, vri_col]
VRI <-  st_intersection(VRI, basin_area) # crop VRI data by study area

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate LW inputted from mass movements-------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select only the slides adjacent to the study reach---------------------------------------------------------------
slides <- subset(slides, Name == 2| Name ==3| Name ==4 | Name ==16)  

# Attribute volume/ha data to slides based on nearby polygons-----------------------------------------------------
Vs_16 <- mean(c(77.9, 86.176, 173.254, 47.983, 206.909))  
Vs_2 <- 80.149 # slides 2-4 were present in 1971, so perhaps input from them is actually negligable?
Vs_3 <- mean(c(80.149, 131.702))
Vs_4 <- 80.149
Vs_slides <- c(Vs_2, Vs_3, Vs_4, Vs_16)
slides$Vs <- Vs_slides # add volume/ha estimates to dataframe

# Calculate area of each slide------------------------------------------------------------------------------------
slides <- slides %>%  # in m2
  mutate(Area = st_area(.) %>% as.numeric())
slides$Area <- slides$Area/10000 # convert from m2 to ha

# Plot of study reach and landslide polygons in the study area----------------------------------------------------
plot(basin_area$geometry, axes = TRUE, col = "lightblue") 
plot(slides$geometry, add = TRUE, col = 'red')
plot(streams$geometry, add = TRUE, col = 'darkblue')

# Calculate I_ms = A_sf(V_sd + V_CWD) -----------------------------------------------------------------------------
V_cwd = 5 # DUMMY VALUE --> an appropriate estimate may come from running the tipsy model for the area 

slides$I_ms <- slides$Area*(slides$Vs + V_cwd) 
slides.df <- as.data.frame(slides) 
I_ms <- aggregate(slides.df['I_ms'], by=slides.df['Year'], sum) # sum input from mass movements by year