# Fuctions for calculating the large wood input to the channel.
# There are 6 functions needed to do this:

# ev2_rnd  -- calculates the expected volume of a log long enough to span the channel banks given a random tree fall anlgle
# landslide_input -- returns the volumne of wood inputted to the channel by year from landslides
# bankErosion_input_nh -- returns the volumne of wood inputted to the channel by year bank erosion in non harvest areas
# bankErosion_input_h --  returns the volumne of wood inputted to the channel by year from bank erosion in non harvest areas
# mortality_input_nh -- returns the volume of wood inputted to the channel by year from mortality in non harvested areas
# mortality_input_h -- returns the volume of wood inputted to the channel by year from mortality in harvested areas


# by Carina Helm and Stephen Bird
# 2019-09-14
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Expected volume of LW (Vb) entering a channel from a single tree-fall event (E_Vb)
ev2_rnd <- function(h,z,rd,w,hd=1.3){
        
        # Function returns the expected volume, E(V) of a single log transfered
        # to the channel from the riparain area given a random (rnd) tree-fall
        # pattern.
        
        # Volume is computed only for the case where the log can cross both
        # (2) channel banks. The equation is derived from Van Sickle and Gregory
        # (1990) with the intergral solved in Mathematica (work completed in
        # 2004-05). See "Solution 1.1 Input Volume".
        
        # The equation below sums two solved integrals: the first computes the
        # expected volume when the tree crosses both banks, while the second
        # computes the volume when the same tree falls at an oblique angle and
        # only the crown and upper stem enters the channel.
        
        # Inputs include: h = tree height; z = distance from
        # the nearest stream bank; rd = tree radius at breast heigth; w =
        # channel width; hd = breast hieght (defaults to 1.3). Note that h > w +
        # z only (otherwise NaN or Inf will be returned).
        
        ifelse(h <= w + z, "Error in the function ev2_rnd...tree height must be greater than the width plus disntance from the channel bank (h > w + z)", 
               
               -(pi * rd ^ 2 * w * (
                       h ^ 2 * (5 * w ^ 2 + 15 * w * z + 9 * z ^ 2) *
                               sqrt((h ^ 2 - (w + z) ^ 2) / h ^ 2) - (w + z) ^
                               2 *
                               (6 * h ^ 2 + w ^ 2 + 3 * w * z + 3 * z ^
                                        2) *
                               (log(cos(1 / 2 * asin((w + z) / h
                               ))) - log(sin(1 / 2 * asin((w + z) / h
                               ))))
               )) /
                       (6 * (h - hd) ^ 2 * (w + z) ^ 2 * acos((w + z) / h))+(pi * rd ^ 2 * (
                               h ^ 2 * z * (z * sqrt(((
                                       h - w - z
                               ) * (
                                       h + w + z
                               )) / h ^ 2) * (6 * w + 5 * z) - 5 * (w + z) ^ 2 * sqrt(1 - z ^ 2 / h ^ 2)) + (w + z) ^
                                       2 * (
                                               2 * h ^ 3 * asin(z / h) - 2 * h ^ 3 * asin((w + z) / h) + z * (6 * h ^ 2 + z ^
                                                                                                                      2) * (log(cos(1 / 2 * asin(
                                                                                                                              z / h
                                                                                                                      ))) -  log(cos(1 / 2 * asin((w + z) / h
                                                                                                                      ))) - log(sin(1 / 2 * asin(
                                                                                                                              z / h
                                                                                                                      ))) +   log(sin(1 / 2 * asin((w + z) / h
                                                                                                                      ))))
                                       )
                       )) / (6 * (h - hd) ^ 2 * (w + z) ^ 2 * (asin(z / h) - asin((w + z) / h)))
        )
        
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# landslide_input - function that returns the volume of wood inputted to the channel by year (I_ms)
landslide_input <- function(slides, V_cwd){
        
        # Inputs include: slides = slides shapefile
        # V_cwd = volume of CWD in forest (tipsy estimate)
        
        slides <- slides %>%  # area in m2 of the slides
                mutate(Area = st_area(.) %>% as.numeric())
        slides$Area <- slides$Area/10000 # convert from m2 to ha
        slides$I_ms <- slides$Area*(slides$Ds + V_cwd) # I_ms calculation, gully erosion negligible?
        slides.df <- as.data.frame(slides) 
        I_ms <- aggregate(slides.df['I_ms'], by=slides.df['Year'], sum) # sum input from mass movements by year
        return(I_ms)
}

# Note - this equation doesn't depend on species level tree characterstics, so a lumped estimate
# for volume was be used (aka LVLTOT_125 column from the VRI dataframe as opposed to LVLSP1,LVLSP2 etc...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bankErosion_input_nh - function returns vol. of wood input to the channel by year through bank erosion for non-harvested polys (I_b)
bankErosion_input_nh<- function(ints_nh.df, V_cwd, E_Vb, Pf_be, B, t1, t2, LVLSP){
        
        # VRI_int.df = VRI df, V_cwd = tipsy coarse wood est.; Pf_be = prob of tree fall (set to 1);, 
        # B = bank erosion rate; t1 = start of study period, t2 = end of study period,
        # E_vb = expected volume from a single tree-fall event
        # LVLSP = df of vol/ha of top two leading species
        
        # annual input from bank erosion from all polygons
        I_bet <- sum(LVLSP*ints_nh.df$x*Pf_be*E_Vb*B, na.rm = TRUE) + 
                 sum(V_cwd*ints_nh.df$x*B, na.rm = TRUE) 
        I_bet <- rep(I_bet , t2-t1+1) # repeat annual input from start to end time
        
        # store the results
        I_bet.df <- data.frame(seq(t1, t2, by=1), I_bet)
        colnames(I_bet.df) <- c("Year", "I_bet")
        
        return(I_bet.df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bankErosion_input_h - function returns vol. of wood input to the channel through bank erosion by year for harvested polys (I_b)
bankErosion_input_h<- function(ints_h.df, E_Vb_tips, Pf_be, B, tipsy){
        
        # VRI = VRI sf, E_vb_tips = expected volume from a single tree-fall event from tipsy data;
        # Pf_be = 1; B = bank erosion rate; tipsy = df of forest characteristics for harvested area
        
        x <- aggregate(x ~ FULL_LABEL, data=ints_h.df, FUN=sum) # sum length of stream in cutblock
        tipsy$x <- x$x[match(tipsy$ID, x$FULL_LABEL)] # add stream lengths/cutblock to dataframe
        
        # annual input from bank erosion from harvested polygon
        tipsy$I_bet_h <- sum((tipsy$x*tipsy$Vs*Pf_be*E_Vb_tips*B), (tipsy$x*tipsy$CWD*B), na.rm = TRUE)
        I_bet_h <- aggregate(I_bet_h ~ Year, data=tipsy, FUN=sum) 
        
        return(I_bet_h)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# mortality_input_nh- function returns vol. of wood input to the channel by year for non-harvested polys (I_m)
mortality_input_nh <- function(ints_nh.df, V_cwd, E_Vb, t1, t2, Pf, LVLSP){
        
        # ints_nh.df = VRI df; V_cwd = cwd estimate;  E_vb = expected volume from a single tree-fall event; 
        # Pf = probability of a tree falling during ti to ti+1; t1 = start time, t2 = end time
        # B = bank erosion rate; tipsy = df of forest characteristics for harvested area
        # LVLSP = df of vol/ha of top two leading species
        
        # calculate annual input from mortality
        I_m <- sum(LVLSP * ints_nh.df$x * Pf * E_Vb, na.rm = TRUE) 
        I_m <- rep(I_m, t2-t1+1) # repeat for study period
        
        # dataframe to store yearly inputs
        I_m.df <- data.frame(matrix(ncol = 2 , nrow = (t2-t1+1))) 
        headers <- c("Year", "I_m")
        colnames(I_m.df) <- headers
        I_m.df$Year <- seq(t1, t2, by=1)
        I_m.df$I_m <- I_m
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# mortality_input_h- function returns vol. of wood input to the channel by year for non-harvested polys (I_b)
mortality_input_h<- function(ints_h.df, E_Vb_tips, t1, t2, LVLSP, tipsy){
        
        # ints_nh.df = VRI df; V_cwd = cwd estimate;  E_vb_tips = estimate volume from tipsy data
        # Pf = probability of a tree falling during ti to ti+1; tw = start time, t2 = end time
        # B = bank erosion rate; tipsy = df of forest characteristics for harvested area
        # LVLSP = df of vol/ha of top two leading species
        
        x <- aggregate(x ~ FULL_LABEL, data=ints_h.df, FUN=sum) # sum distance of stream in each cutblock
        tipsy$x <- x$x[match(tipsy$ID, x$FULL_LABEL)] # add stream lengths/cutblock to dataframe
        
        # calculate annual input from mortality
        I_m <- (tipsy$x*tipsy$Vs*tipsy$Pf*E_Vb_tips) 
        
        # store the results
        I_m <- data.frame(cbind(seq(t1, t2, by=1), I_m))
        colnames(I_m) <- c("Year", "I_m")
        return(I_m)
}

