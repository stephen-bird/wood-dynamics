# Script with fuction that calcualtes the volume of logs transfered from the
# landslide areas to the channel. 

# I_ms returns the volume of wood inputted to the channel by year 

# by Carina Helm
# 2019-09-14

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required pacakges for the function-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("sf")
library("here")
library("dplyr")


# landslide_input - a function that returns the volume of wood inputted to the channel by year (I_ms)
landslide_input <- function(slides, VRI, V_cwd){
  
  # Inputs include: slides = slides shapefile; VRI =  VRI shapefile; V_cwd = volume of CWD in forest (tipsy estimate)
  
  slides <- slides %>%  # in m2
  mutate(Area = st_area(.) %>% as.numeric())
  slides$Area <- slides$Area/10000 # convert from m2 to ha
  slides$I_ms <- slides$Area*(slides$Ds + V_cwd) 
  slides.df <- as.data.frame(slides) 
  I_ms <- aggregate(slides.df['I_ms'], by=slides.df['Year'], sum) # sum input from mass movements by year
  return(I_ms)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required data and run the function to determine I_ms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slides <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "slides")
basin_area <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "basin_area")
streams <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "study_reaches")
VRI <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "VRI_nad83")
VRI <-  st_intersection(VRI, basin_area) # crop VRI data by study area
V_cwd = 0.54425  # estimate from TIPSY for 100 year old forest stand of LP and WS

I_ms = landslide_input(slides, VRI, V_cwd)
