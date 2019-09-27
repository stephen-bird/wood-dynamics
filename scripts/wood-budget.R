# Wood Budget Model
# work-in-progress

# By Carina Helm 
# September 26, 2019
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD REQUIRED PACKAGES ---------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("sf")
library("here")
library("dplyr")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ AND WRANGLE THE DATA --------------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load the shapefiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slides <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "slides") # landslides
basin_area <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "basin_area") # basin boundary
streams <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "study_reaches") # Donna Creek - this needs to be replaced with the propr stream sf
VRI <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "VRI_nad83")
VRI <-  st_intersection(VRI, basin_area) # crop VRI data by study area

# VRI data wrangling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VRI_int <- st_intersection(streams, VRI) # extract VRI polygons intersecting the stream
VRI_int <- VRI_int %>% 
        mutate(x = st_length(.) %>% as.numeric()) # length in m of stream in each polygon
VRI_int.df <- as.data.frame(VRI_int) # convert VRI_int to dataframe for data wrangling
ints_nh.df <- subset(VRI_int.df, is.na(HRVSTDT))  # polygons that haven't been harvested
ints_h.df <- subset(VRI_int.df, HRVSTDT > 0)   # polygon that has been harvested

# Forest characteristics 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
V_cwd = 0.54425  # cwd estimate from TIPSY for 100 year old forest stand of LP and WS
B <- 0.001 # bank erosion; m/yr dummy value -- will be calculated from airphotos later
Pf_be <- 1 # set probability of fall from BE to 1 for bank erosion budget term
t1 = 1971
t2 = 2007

# height and diamater estimates for E_Vb calculation
H_sp1 <- ints_nh.df$PROJ_HT_1 # height of the leading species
H_sp2 <- ints_nh.df$PROJ_HT_2 # heights only present for the top two leading species...
dbh <- (ints_nh.df$Q_DIAM_125)/100/2  # convert to radius in metres - this is only present for the leading species

tipsy <- read.csv(here("data/raw/TIPSY_inputs.csv")) # tipsy values for harvested areas

# Probability of tree fall estimates (Pf), based on inverse of maturity for the tree species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ints_nh.df$Pf_SP1 <- ints_nh.df$SPEC_CD_1
ints_nh.df$Pf_SP2 <- ints_nh.df$SPEC_CD_2
ints_nh.df$Pf_SP3 <- ints_nh.df$SPEC_CD_3

# Pf calculation
ints_nh.df[, c("Pf_SP1", "Pf_SP2")][ints_nh.df[, c("Pf_SP1", "Pf_SP2")]=="PL"] <- 1/250 # Lodgepole pine
ints_nh.df[, c("Pf_SP1", "Pf_SP2")][ints_nh.df[, c("Pf_SP1", "Pf_SP2")]=="SW"]<-1/250 # White spruce
ints_nh.df[, c("Pf_SP1", "Pf_SP2")][ints_nh.df[, c("Pf_SP1", "Pf_SP2")]=="AC"]<-1/150 # Balsam poplar
ints_nh.df[, c("Pf_SP1", "Pf_SP2")][ints_nh.df[, c("Pf_SP1", "Pf_SP2")]=="BL"]<- 1/150 # Subalpine fir
ints_nh.df[, c("Pf_SP1", "Pf_SP2")][ints_nh.df[, c("Pf_SP1", "Pf_SP2")]=="SX"]<- 1/250 # Spruce hybrid

Pf <- data.frame(ints_nh.df$Pf_SP1 , ints_nh.df$Pf_SP2, stringsAsFactors=FALSE)
Pf <- as.data.frame(sapply(Pf, as.numeric))
colnames(Pf) <- c("Pf_SP1", "Pf_SP2")

# Volume estimates for each species in the polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LVLSP <- data.frame(cbind(ints_nh.df$LVLSP1_125, ints_nh.df$LVLSP2_125))
colnames(LVLSP) <- c("LVLSP1", "LVLSP2") # there is a 3rd leading species, but the VRI data doesn't have a height or diam for them
LVLSP = LVLSP/10000 # convert from m3/ha to m3/m2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WOOD BUDGET CALCULATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate input from landslides
I_ms <- landslide_input(slides, V_cwd)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate input from bank erosion
E_Vb = cbind(as.numeric(ev2_rnd(H_sp1, 0.1, dbh, w = 15, hd=1.3)), #  summed for the first two leading species for each polygon
             as.numeric(ev2_rnd(H_sp2, 0.1, dbh, w = 15, hd=1.3)))
E_Vb_tips = as.numeric(ev2_rnd(tipsy$height,0.1,((tipsy$dbh)/100/2),w = 15,hd=1.3)) # input from tipys char's for harvest poly

I_be_nh <- bankErosion_input_nh(ints_nh.df, V_cwd, E_Vb, Pf_be, B, t1, t2, LVLSP)
I_be_h <- bankErosion_input_h(ints_h.df, E_Vb_tips, Pf_be, B, tipsy)# check this calculation
I_bet = merge(I_be_nh,I_be_h, by="Year", all = TRUE) # combine input for harvested/non-harvested polygons
I_bet$I_be = I_bet$I_bet+I_bet$I_bet_h
I_bet = I_bet[,-c(3,4)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate input from mortality
I_m_nh <- mortality_input_nh(ints_nh.df, V_cwd, E_Vb, t1, t2, Pf, LVLSP)
I_m_h <- mortality_input_h(ints_h.df, E_Vb_tips, t1, t2, LVLSP, tipsy)
I_m = data.frame(I_m_h$Year, rowSums(cbind(I_m_nh, I_m_h$I_m), na.rm=TRUE)) # combine input for harvested/non-harvested polygons
colnames(I_m) <- c("Year", "I_m")

