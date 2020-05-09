####################################################################################################################
# Code to calculate LW input from bank erosion
# By Carina Helm, Sept 11, 2019
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

# Include only relevent columns from VRI dataframe 
vri_col = c("FULL_LABEL","BASAL_AREA", "LBL_TEND","Q_DIAM_125", "Q_DIAM_175", "Q_DIAM_225", "PROJ_AGE_1","PROJ_HT_1","PROJ_HT_1", "LVLSP1_125",
            "LVLSP2_125", "N_LOG_DATE","REF_YR_ID", "ATRIB_DATE", "PROJ_DATE", "N_LOG_DIST","HRVSTDT" ,"SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2",
            "SPEC_PCT_2", "SPEC_CD_3", "SPEC_CD_3", "SPEC_PCT_3", "LIVE_STEMS","DEAD_STEMS", "BCLCS_LV_2", "BCLCS_LV_5", "LVLSP3_125", 
            "LVLTOT_125", "SPEC_PCT_1","SPEC_PCT_2", "SPEC_PCT_3", "SPEC_PCT_4", "P_HT_CAS_1", "P_HT_CAS_2")
VRI <- read_sf(dsn = "data/raw/shapefiles", layer = "VRI_nad83")
VRI <- VRI[, vri_col]
VRI <-  st_intersection(VRI, basin_area) # crop VRI data by study area

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate LW inputted from bank erosion---------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot VRI polygons that are adjacent to the stream----------------------------------------------------------------
VRI_rip <- VRI[lengths(st_intersects(VRI, streams)) > 0,] 
plot(basin_area$geometry, axes = TRUE, col = "lightblue")
plot(VRI_rip$geometry, add = TRUE, col = "orange")
plot(streams$geometry, add = TRUE, col = 'darkblue', lty = 1, lwd = 2)

# Calculate length (x) of stream reach in each VRI polygon---------------------------------------------------------
VRI_int <- st_intersection(streams, VRI)
VRI_int <- VRI_int %>% 
        mutate(x = st_length(.) %>% as.numeric()) # length in m
VRI_int.df <- as.data.frame(VRI_int) # convert VRI_int to dataframe for data wrangling

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate input from bank erosion for all polygons WITHOUT harvesting-------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the constants-----------------------------------------------------------------------------------------------
E_Vb <- 0.005 # dummy value to be replaced
V_cwd <- 1 # dummy value to be replaced from tipsy
B <- 0.01 # m/yr dummy value -- will be calculated from airphotos later
Pf_be <- 1 # set probability of fall from BE to 1
t1 = 1971
t2 = 2007

# Select all polygons that haven't been harvested-----------------------------------------------------------------
ints_nh.df <- subset(VRI_int.df, is.na(HRVSTDT))  

# Calculate Ibe = summation[Ds*B*dx*Pf*E(Vb)]
I_bet <- sum(ints_nh.df$LVLTOT_125*ints_nh.df$x*Pf_be*E_Vb*B, na.rm = TRUE) + sum(V_cwd*ints_nh.df$x*Pf_be*E_Vb*B, na.rm = TRUE) # annual input from bank erosion from all polys
I_bet <- rep(I_bet , t2-t1+1) # repeat annual input from t1 to t2

# Dataframe to store yearly inputs--------------------------------------------------------------------------------
I_bet.df <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) 
headers <- c("Year", "I_bet")
colnames(I_bet.df) <- headers
I_bet.df$Year <- seq(t1, t2, by=1)
I_bet.df$I_bet <- I_bet

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate input from bank erosion for all polygons WITH harvesting------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select polygons that have been harvested--------------------------------------------------------------------------
ints_h.df <- subset(VRI_int.df, HRVSTDT > 0)  
x <- aggregate(x ~ FULL_LABEL, data=ints_h.df, FUN=sum) # sum length of stream in each cutblock

tipsy <- read.csv("data/raw/tipsy.csv") # dummy dataframe of tipsy values for harvested areas
tipsy$x <- x$x[match(tipsy$ID, x$FULL_LABEL)] # add stream lengths/cutblock to dataframe

# Calculate Ibe = summation[Ds*B*dx*Pf*E(Vb)]
tipsy$I_bet_h <- (tipsy$x*tipsy$Vs*Pf_be*tipsy$E_vb*B) + (tipsy$x*V_cwd*Pf_be*tipsy$E_vb*B)

# Sum values by year and add to dataframe--------------------------------------------------------------------------
I_bet_h <- aggregate(I_bet_h ~ Year, data=tipsy, FUN=sum) 
I_bet.df$I_bet = I_bet.df$I_bet + I_bet_h$I_bet_h





