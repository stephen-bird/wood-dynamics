# Fuctions that calcualte the volume of logs transfered from 
# bank erosion to the channel. There are two functions:

# I_be_h  -- returns the volume of wood inputted to the channel by year for non-harvested polys
# I_be_nh -- returns the volumne of wood inputted to the channel by year for harvest polys

# by Carina Helm
# 2019-09-14

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required packages for the function-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("sf")
library("here")
library("dplyr")


# erosion_input_nh - a function returns vol. of wood input to the channel by year for non-harvested polys (I_b)
bankErosion_input_nh<- function(slides, VRI, V_cwd, E_Vb, Pf_be, B, t1, t2){
      
        # slides = landslides sf, VRI = VRI sf, V_cwd = tipsy coarse wood est.; Pf_be = 1;, 
        # B = bank erosion rate; t1 = start of study period, t2 = end of study period, E_vb
        
        VRI_int <- st_intersection(streams, VRI)
        VRI_int <- VRI_int %>% 
                mutate(x = st_length(.) %>% as.numeric()) # length in m
        VRI_int.df <- as.data.frame(VRI_int) # convert VRI_int to dataframe for data wrangling
        
        ints_nh.df <- subset(VRI_int.df, is.na(HRVSTDT))  # select all polygons that haven't been harvested
        I_bet <- sum(ints_nh.df$LVLTOT_125*ints_nh.df$x*Pf_be*E_Vb*B, na.rm = TRUE) + sum(V_cwd*ints_nh.df$x*Pf_be*E_Vb*B, na.rm = TRUE) # annual input from bank erosion from all polys
        I_bet <- rep(I_bet , t2-t1+1) # repeat annual input from t1 to t2
        
        I_bet.df <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) # store the results
        headers <- c("Year", "I_bet")
        colnames(I_bet.df) <- headers
        I_bet.df$Year <- seq(t1, t2, by=1)
        I_bet.df$I_bet <- I_bet
        
        return(I_bet.df)
}
        

# erosion_input_h - function returns vol. of wood input to the channel by year for harvested polys (I_b)
bankErosion_input_h<- function(slides, VRI, E_Vb, Pf_be, B, tipsy){
        
        # slides = landslides sf, VRI = VRI sf, E_vb = integral; Pf_be = 1;, 
        # B = bank erosion rate; tipsy = yearly LW volume estimates post harvest
        
        VRI_int <- st_intersection(streams, VRI)
        VRI_int <- VRI_int %>% 
                mutate(x = st_length(.) %>% as.numeric()) # length in m
        VRI_int.df <- as.data.frame(VRI_int) # convert VRI_int to dataframe for data wrangling
        
        ints_h.df <- subset(VRI_int.df, HRVSTDT > 0)  
        x <- aggregate(x ~ FULL_LABEL, data=ints_h.df, FUN=sum) # sum length of stream in each cutblock
        tipsy$x <- x$x[match(tipsy$ID, x$FULL_LABEL)] # add stream lengths/cutblock to dataframe
        
        tipsy$I_bet_h <- (tipsy$x*tipsy$Vs*Pf_be*tipsy$E_vb*B) + (tipsy$x*tipsy$CWD*Pf_be*tipsy$E_vb*B)
        I_bet_h <- aggregate(I_bet_h ~ Year, data=tipsy, FUN=sum) 
        
        return(I_bet_h)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required data and run the function to determine I_b
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

slides <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "slides")
basin_area <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "basin_area")
streams <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "study_reaches")
VRI <- read_sf(here(dsn = "data/raw/shapefiles"), layer = "VRI_nad83")
VRI <-  st_intersection(VRI, basin_area) # crop VRI data by study area
V_cwd = 0.54425  # estimate from TIPSY for 100 year old forest stand of LP and WS
E_Vb <- 0.005 # dummy value to be replaced
B <- 0.01 # m/yr dummy value -- will be calculated from airphotos later
Pf_be <- 1 # set probability of fall from BE to 1
t1 = 1971
t2 = 2007
tipsy <- read.csv(here("data/raw/TIPSY_inputs.csv")) # dummy dataframe of tipsy values for harvested areas


I_be_nh = bankErosion_input_nh(slides, VRI, V_cwd, E_Vb, Pf_be, B, t1, t2)
I_be_h = bankErosion_input_h(slides, VRI, E_Vb, Pf_be, B, tipsy)
        
        