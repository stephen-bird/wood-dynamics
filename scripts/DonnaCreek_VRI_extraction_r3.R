####################################################################################################################
# Donna Creek Wood Budget Analysis
#
# Code to extract VRI data from slides
#
# By Carina Helm, August 20, 2019
#
####################################################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required pacakges
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(tidyverse)
library(rgeos)
library(sp)
library(plyr)
library(here)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the basedata
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slides <- read_sf(dsn = here::here("basedata"), layer = "landslides")
basin_area <- read_sf(dsn = here::here("basedata"), layer = "basin_area")
basin_area <- basin_area[, -c(1,2,3,4)] # remove extra columns in dataframe

streams <- read_sf(dsn = here::here("basedata"), layer = "study_reaches")
streams <- streams[, -c(1:27)] 

VRI <- read_sf(dsn = here::here("basedata"), layer = "VRI_nad83")

# include only relevent columns from VRI dataframe - see word doc for key to codes
VRI <- VRI[, c("FULL_LABEL","BASAL_AREA", "LBL_TEND",
              "Q_DIAM_125", "Q_DIAM_175", "Q_DIAM_225",
              "PROJ_AGE_1","PROJ_HT_1","PROJ_HT_1", "LVLSP1_125",
              "LVLSP2_125", "N_LOG_DATE","REF_YR_ID",
              "ATRIB_DATE", "PROJ_DATE", "N_LOG_DIST",
              "HRVSTDT" ,"SPEC_CD_1", "SPEC_PCT_1", 
              "SPEC_CD_2", "SPEC_PCT_2", "SPEC_CD_3", 
              "SPEC_CD_3", "SPEC_PCT_3", "LIVE_STEMS",
              "DEAD_STEMS", "BCLCS_LV_2", "BCLCS_LV_5",
              "LVLSP3_125", "LVLTOT_125")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Crop VRI data by study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VRI <-  st_intersection(VRI, basin_area)
plot(VRI$geometry, axes = TRUE, col = "lightblue")


####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PART 1: CALCULATE INPUTTED LW (I)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an empty dataframe to store the results of the LW inputs for each year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t1 <- 1971 # start date to be determined...but 1971 is the first year we have airphotos for the landslides. 
t2 <- 2007 # end date - last year we have field data?

LW_input <- data.frame(matrix(ncol = 4))
headers <- c("Year", "I_ms", "I_m", "I_bet")
colnames(LW_input) <- headers


####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 CALCULATE I_ms --> INPUT FROM MASS MOVEMENTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

# 1.1 Intersect VRI polygons with the slides
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slides <- subset(slides, Name == 2| Name ==3| Name ==4 | Name ==16)  # select only the slides adjacent to the study reach

# Attribute volume/ha data to slides based on nearby polygons
Vs_16 <- mean(c(77.9, 86.176, 173.254, 47.983, 206.909))  
Vs_2 <- 80.149 # slides 2-4 were present in 1971, so perhaps input from them is actually negligable?
Vs_3 <- mean(c(80.149, 131.702))
Vs_4 <- 80.149
Vs_slides <- c(Vs_2, Vs_3, Vs_4, Vs_16)

slides$Vs <- Vs_slides # add volume/ha estimates to dataframe
slides <- slides %>%  # calculate area of each slide polygon (in m2)
  mutate(Area = st_area(.) %>% as.numeric())
slides$Area <- slides$Area/10000 # convert slide area from m2 to ha

# Plot of study reach and landslide polygons in the drainage basin
plot(basin_area$geometry, axes = TRUE, col = "lightblue") 
plot(slides$geometry, add = TRUE, col = 'red')
plot(streams$geometry, add = TRUE, col = 'darkblue')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.2 Calculate I_ms  --> (I_ms = A_sf(V_sd + V_CWD) + I_g)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
V_cwd = 5 # DUMMY VALUE --> an appropriate estimate may come from running the tipsy model for the area. 

slides$I_ms <- slides$Area*(slides$Vs + V_cwd) # (I_ms = A_sf(V_sd + V_CWD) + I_g)
slides.df <- as.data.frame(slides) # convert to data frame for data wrangling
I_ms <- aggregate(slides.df['I_ms'], by=slides.df['Year'], sum) # sum input from mass movements by year

LW_input <- merge(I_ms,LW_input, by=names(I_ms), all = TRUE)

####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 CALCULATE I_bet --> INPUT FROM BANK EROSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

# 2.1 Plot the VRI polygons that the stream intersects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BE_polys <- VRI[lengths(st_intersects(VRI, streams)) > 0,]
plot(basin_area$geometry, axes = TRUE, col = "lightblue")
plot(BE_polys$geometry, add = TRUE, col = "orange")
plot(streams$geometry, add = TRUE, col = 'darkblue', lty = 1, lwd = 2)

# 2.2 get the length (x) of each stream segment that intersects the VRI polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ints <- st_intersection(streams, VRI)
ints <- ints %>% 
  mutate(x = st_length(.) %>% as.numeric()) # length in m

ints.df <- as.data.frame(ints) # convert ints to dataframe for data wrangling

# 2.3  make new columns for each species type and repalce with inverse of species' lifespan for Pf
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ints.df$Pf_SP1 <- ints.df$SPEC_CD_1
ints.df$Pf_SP2 <- ints.df$SPEC_CD_2
ints.df$Pf_SP3 <- ints.df$SPEC_CD_3

ints.df[ , 34:36][ints.df[ , 34:36]=="PL"] <- 1/250 # Lodgepole pine
ints.df[ , 34:36][ints.df[ , 34:36]=="SW"]<-1/250 # White spruce
ints.df[ , 34:36][ints.df[ , 34:36]=="AC"]<-1/150 # Balsam poplar
ints.df[ , 34:36][ints.df[ , 34:36]=="BL"]<- 1/150 # Subalpine fir
ints.df[ , 34:36][ints.df[ , 34:36]=="SX"]<- 1/250 # Spruce hybrid

# 2.4 Calculate E(V_b) DUMMY VALUES for now...while figuring out integral of for f(a)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
z <- 1     # distance of tree from banks - not sure how to choose this
ints.df$Ps <- (acos(z/ints.df$PROJ_HT_1))/180
h <- 4
r <- 0.1
Vb <- (1/3)*pi*h*r^2 

ints.df$E_Vb <- ints.df$Ps*Vb

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.4.1 Calculate input from bank erosion for all polygons WITHOUT harvesting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set the constants
B <- 1 # m/yr DUMMY VALUE -- will be calculated from airphotos later
hv_date <- 1984 # harvest date of only polygon along stream that was logged
Pf_be <- 1 # set probability of fall from BE to 1

# estimate pre-harvest V_s (m3/ha) in harvested polygon from adjacent polygons
# v_h <- mean(c(413.427, 216.59, 384.992, 255,03, 208.023, 251.656, 201.468)) 
# ints.df$LVLTOT_125[which(ints.df$HRVSTDT > 0)] <- v_h # replace V_s cutblock values in dataframe with estimate
# 
# ints.df$LVLSP1_125[which(ints.df$HRVSTDT > 0)] <- v_h # replace V_s cutblock values for leading species with estimate
# ints.df$LVLSP2_125[which(ints.df$HRVSTDT > 0)] <- 0 
# ints.df$LVLSP3_125[which(ints.df$HRVSTDT > 0)] <- 0

ints.df_nh <- subset(ints.df, is.na(HRVSTDT))  # select all polygons that haven't been harvested

# sum input from standing trees and CWD
I_bet <- sum(ints.df_nh$LVLTOT_125*ints.df_nh$x*Pf_be*ints.df_nh$E_Vb*B, na.rm = TRUE) + sum(V_cwd*ints.df_nh$x*Pf_be*ints.df_nh$E_Vb*B, na.rm = TRUE) # annual input from bank erosion from all polys
I_bet <- rep(I_bet , t2-t1+1) # repeat annual input from t1 to t2

I_bet.df = data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) # dataframe to store yearly inputs
headers <- c("Year", "I_bet")
colnames(I_bet.df) <- headers
I_bet.df$Year = seq(t1, t2, by=1)
I_bet.df$I_bet = I_bet

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.4.2 Calculate input from bank erosion for polygons WITH HARVESTING 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ints_h.df <- subset(ints.df, HRVSTDT > 0)  # select polygons that have been harvested
ints_h.df[2, 2] <- "test" # for testing purposes
x <- aggregate(x ~ FULL_LABEL, data=ints_h.df, FUN=sum) # sum distance of stream in each poly

tipsy <- read.csv(here::here("basedata", "tipsy.csv")) # dummy dataframe of tipsy values for harvested areas
tipsy$x <- x$x[match(tipsy$ID, x$FULL_LABEL)] # add stream lengths/poly to dataframe

# sum inputs from standing wood and cwd
tipsy$I_bet_h <- (tipsy$x*tipsy$Vs*Pf_be*tipsy$E_vb*B) + (tipsy$x*V_cwd*Pf_be*tipsy$E_vb*B)

I_bet_h <- aggregate(I_bet_h ~ Year, data=tipsy, FUN=sum) # sum values by year

I_bet_all <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) # dataframe to store yearly inputs
headers <- c("Year", "I_bet")
colnames(I_bet_all) <- headers
I_bet_all$Year = seq(t1, t2, by=1)
I_bet_all$I_bet <- I_bet.df$I_bet+I_bet_h$I_bet_h # sum bank erosion inputs for harvested and unharvested polygons

LW_input <- merge(I_bet_all,LW_input, by=names(I_bet_all), all = TRUE)
####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 CALCULATE I_m --> INPUT FROM MORTALITY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

# 3.1 again DUMMY VALUES FOR E_Vb while figuring out f(a) integral
E_V_m <- 0.003

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.2.1 Calculate input from mortality for poly with NO HARVESTING 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get volume estimates for EACH species in the polygons
LVLSP <- data.frame(cbind(ints.df_nh$LVLSP1_125, ints.df_nh$LVLSP2_125, ints.df_nh$LVLSP3_125))
colnames(LVLSP) <- c("LVLSP1", "LVLSP2", "LVLSP3")

# each species has a different fall probability
Pf <- data.frame(ints.df_nh$Pf_SP1 , ints.df_nh$Pf_SP2, ints.df_nh$Pf_SP3, stringsAsFactors=FALSE)
Pf <- as.data.frame(sapply(Pf, as.numeric))
colnames(Pf) <- c("Pf_SP1", "Pf_SP2", "Pf_SP3")

I_m <- sum(LVLSP * ints.df_nh$x * Pf * E_V_m, na.rm = TRUE) # calculate annual input from mortality
I_m <- rep(I_m, t2-t1+1) # repeat for study period

I_m.df <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) # dataframe to store yearly inputs
headers <- c("Year", "I_m")
colnames(I_m.df) <- headers
I_m.df$Year <- seq(t1, t2, by=1)
I_m.df$I_m <- I_m

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.2.2 Calculate input from bank erosion for polygons WITH HARVESTING 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tipsy$I_m_h <- tipsy$x*tipsy$Vs*tipsy$Pf*tipsy$E_vb # calculate input for harvested poly for all years
I_m_h <- aggregate(I_m_h ~ Year, data=tipsy, FUN=sum) # sum values by year

I_m_all <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) # dataframe to store yearly inputs
headers <- c("Year", "I_bet")
colnames(I_m_all) <- headers
I_m_all$Year = seq(t1, t2, by=1)
I_m_all$I_m<- I_m.df$I_m+I_m_h$I_m_h # sum bank erosion inputs for harvested and unharvested polygons

LW_input <- merge(I_m_all,LW_input, by=names(I_m_all), all = TRUE)
LW_input <- aggregate(LW_input[2:ncol(LW_input)],LW_input['Year'], FUN=sum, na.rm =TRUE)

####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PART II: CALCULATE TRANSPORTED WOOD (T)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

# needs to be fed to monte carlo model? all Dummy values - need to check field data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lw <- 10
Wb <- 15
S_w <- 10 * exp(-3.8*(Lw/Wb))
V_c <- 2 # volume of wood per unit channel area - dunny value
P_m <- 0.5

I_t <- S_w*P_m*V_c
O_t <- S_w*P_m*V_c

####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PART III: CALCULATE OUTPUTTED WOOD (O)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 O_d calculation - all DUMMY VALUES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
V_o <- 50 # initial wood volume from field data
Aw <- 30 # years
k <- 1/Aw
dt <- 30

f <- function(t) {V_o*t*(1-exp(-k*t))}
O_d <- integrate(f, lower = 0, upper = dt)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.2  O_ca calculation - all DUMMY VALUES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
O_ca <- x*B*V_c

####################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PART IV: CALCULATE STORAGE (S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################################
I <- rowSums(LW_input[2:4]) 
Tr <- I_t - O_t
Lx <- sum(O_ca, na.rm = TRUE)

dS <- (I - Lx + Tr - O_d$value)

