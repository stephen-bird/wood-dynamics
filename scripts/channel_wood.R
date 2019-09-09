
# Code to convert field data that measures wood storage per channel interval into volume, length and diameter.
# Work-in-progress

# ------------------------------------------------------------------------------

library(here)
library(zoo)
library(icenReg)
library(dplyr)

# ----- comments from Carina to test commits
# ----- attempt to edit actual channel_wood file and push changes to github


# Read and wrangle data --------------------------------------------------------

# Start with the file with the data from the upper reaches:
wood_dat1 <- read.csv("data/raw/donna_wood_92_fan.csv",skip=18,header=TRUE)
head(wood_dat1)
wood_dat1 <- wood_dat1[,2:11]
names(wood_dat1) <- c("hd","dia","len","amount","orient","wad","pos","ht","func","jam")
head(wood_dat1)
# hd = distance
# dia = diameter class (m)
# len = length (m)
# amount = number of logs
# orient = orientation of logs: parallel (1), diagonal (2), perpendicular (3)
# wad = root wad is present (1) or absent (2)
# pos = psoition: in-channel (1), above one bank (2), above both banks (3)
# func = function: L = lateral scour, U = under scour, P = plunge pool (over-log scour), LJ = log jam, LS = log step
# jam = jam name

# Add reaches
wood_dat1$reach <- ifelse(wood_dat1$hd < 700,1,
                          ifelse(wood_dat1$hd < 1043, 2,
                                 ifelse(wood_dat1$hd < 2036, 3,
                                        ifelse(wood_dat1$hd < 4131, 4, 5))))
                                        
# Now add in the data from the lower reaches (note: there's a big gap between upper and lower reaches...not all reaches were surveyed):
wood_dat2 <- read.csv("data/raw/donna_wood_92_manson.csv",skip=18,header=TRUE)
head(wood_dat2)
wood_dat2 <- wood_dat2[,1:10]
names(wood_dat2) <- c("hd","dia","len","amount","orient","wad","pos","ht","func","jam")
wood_dat2$reach <- 8
wood_dat2$hd <- wood_dat2$hd + 10000 # Make the hd's unique

# Merge df's
wood_dat1$index <- seq_along(wood_dat1$hd) # Create and index
wood_dat2$index <- seq_along(wood_dat2$hd) # # Create and index
wood_dat2$index <- wood_dat2$index + max(wood_dat1$index) # Make the second index follow the first index 
wood_dat <- rbind(wood_dat1,wood_dat2)


# Identify all the wood observations that have a diameter, length, and amount recorded in the field
good <- complete.cases(wood_dat[,2:4]) 
wood_dat <- wood_dat[good,]
# Given the previous two lines of code, any NA in the hd column should have a distance entry that matches the preceeding distance entry
wood_dat$hd2 <- na.locf(wood_dat$hd) # from the Zoo library
wood_dat$pos <- ifelse(is.na(wood_dat$pos),0,wood_dat$pos) # Replace empty "positions" with 0
head(wood_dat)

# Convert field codes into numeric values --------------------------------------

# Dan's coding system:
# Diameter: 1 = 0 - <0.1; 2 = 0.1 - 0.3; 3 = 0.4 - 0.7; 4 = 0.7 - 1.2; 5 = >1.2 
# Length: 1 = 1 - 5; 2 = 5 - 10; 3 = 10 - 15; 4 = 15 - 20; 5 = >20
# Amount: 1 = 1; 2 = 2 - 3; 3 = 4 - 7; 4 = 7 - 12; 5 = >12

# Assumptions re: Dan's system:
# Amount: Upper limit for class 5 is 25 pieces
# Length class 5 = >20 - 34.5 (from Carina and the VRI data)
# Dimater class 5 not observed in the field so we can ignore

hist(wood_dat$dia)
hist(wood_dat$len)
hist(wood_dat$amount)

wood_dat$jam_pa <- as.factor(ifelse(wood_dat$jam=="","no_jam","jam")) # Binary jam identification
wood_dat$amount <- ifelse(wood_dat$amount == 6, 5, wood_dat$amount) # Fix a data entry error

# Create the upper and lower bounds for each interal:
wood_dat$amount_low <- ifelse(wood_dat$amount == 1,1,
                           ifelse(wood_dat$amount == 2,2,
                                  ifelse(wood_dat$amount == 3,4,
                                         ifelse(wood_dat$amount == 4, 7,
                                                ifelse(wood_dat$amount == 5, 12,
                                                       ifelse(wood_dat$amount == 0,0,0))))))

wood_dat$amount_high <- ifelse(wood_dat$amount == 1,1,
                           ifelse(wood_dat$amount == 2,3,
                                  ifelse(wood_dat$amount == 3,7,
                                         ifelse(wood_dat$amount == 4, 12,
                                                ifelse(wood_dat$amount == 5, 25,
                                                       ifelse(wood_dat$amount == 0,0,0))))))


# First, estimate the amount of logs -------------------------------------------

# Fit a non-parametric model to the data:
np_fit_amount = ic_np(cbind(wood_dat$amount_low, wood_dat$amount_high), data = wood_dat)
plot(np_fit_amount)

# Impute data in place of the field codes:
set.seed(42)
np_impute_amount <- imputeCens(np_fit_amount, newdata = NULL, imputeType = "fullSample", samples = 5)
wood_dat$impute_amount <- round(np_impute_amount[,1],0)

# Repliacte each row n times, where n = the amount of logs for a given row
wood_df <- wood_dat # Needs to be done in a new df so we can preserve the orginal data
wood_df$rep <- wood_df$impute_amount # This will be used to replicate the rows as acquired
wood_df$rep <- ifelse(wood_df$rep == 0, 1, wood_df$rep) # If we don't change 0 to 1, the zero rows will be deleted in the next step
wood_df <- as.data.frame(lapply(wood_df, rep, wood_df$rep))
wood_df$amount <- NULL # The "amount" column won't make sense so delete it now
wood_df$rep <- NULL
hist(wood_df$dia)
hist(wood_df$len)
wood_df$index <- seq_along(wood_df$hd2) # Create an index to re-join subset data to wood_df once diamter estimates are complete

# Diameter estimates -----------------------------------------------------------

# Note that class 5 is absent in the data so the values are ignored
wood_df$dia_low <- ifelse(wood_df$dia == 1,0,
                           ifelse(wood_df$dia == 2,0.1,
                                  ifelse(wood_df$dia == 3,0.3,
                                         ifelse(wood_df$dia == 4, 0.7,
                                                ifelse(wood_df$dia == 5, 1.2,NA)))))

wood_df$dia_high <- ifelse(wood_df$dia == 1, 0.1,
                          ifelse(wood_df$dia == 2, 0.3,
                                 ifelse(wood_df$dia == 3, 0.7,
                                        ifelse(wood_df$dia == 4, 1.2,
                                               ifelse(wood_df$dia == 5, 2,NA)))))


wood_dia <- select(wood_df, dia_low, dia_high, index) # select columns we need
good <- complete.cases(wood_dia) # Get rid of the 0 diamters before estimating the distribution
wood_dia <- wood_dia[good,]
np_fit_dia = ic_np(cbind(wood_dia$dia_low, wood_dia$dia_high), data = wood_dia) # Fit a non-parametric model to the data

plot(np_fit_dia)
set.seed(42)
np_impute_dia <- imputeCens(np_fit_dia, newdata = NULL, imputeType = "fullSample", samples = 5)
wood_dia$impute_dia <- np_impute_dia[,1]
wood_df$dia_low <- NULL
wood_df$dia_high <- NULL
wood_df <- left_join(wood_df,wood_dia,by = c("index"))

# Length estimates -------------------------------------------------------------

wood_df$len_low <- ifelse(wood_df$len == 1,1,
                          ifelse(wood_df$len == 2,5,
                                 ifelse(wood_df$len == 3,10,
                                        ifelse(wood_df$len == 4, 15,
                                               ifelse(wood_df$len == 5, 20,NA)))))

wood_df$len_high <- ifelse(wood_df$len == 1, 5,
                           ifelse(wood_df$len == 2, 10,
                                  ifelse(wood_df$len == 3, 15,
                                         ifelse(wood_df$len == 4, 20,
                                                ifelse(wood_df$len == 5, 34.5,NA)))))


wood_len <- select(wood_df, len_low, len_high, index) # select columns we need
good <- complete.cases(wood_len) # Get rid of the 0 lengths before estimating the distribution
wood_len <- wood_len[good,]
np_fit_len = ic_np(cbind(wood_len$len_low, wood_len$len_high), data = wood_len)

plot(np_fit_len)
set.seed(42)
np_impute_len <- imputeCens(np_fit_len, newdata = NULL, imputeType = "fullSample", samples = 5)
wood_len$impute_len <- np_impute_len[,1]
wood_df$len_low <- NULL
wood_df$len_high <- NULL
wood_df <- left_join(wood_df,wood_len,by = c("index"))

# Clean-up the df and estiamte volumes by survey interval-----------------------

wood_df <- select(wood_df,hd2,dia,len,orient,wad,pos,ht,func,jam,jam_pa,impute_amount,impute_dia,impute_len,index,reach) # Subset the data we need
wood_df$vol <- pi * (wood_df$impute_dia / 2)^2 * wood_df$impute_len # Calculate volume of each log
wood_df$vol <- ifelse(is.na(wood_df$vol),0,wood_df$vol) # Replace NA volumes with 0 m^3
wood_inter <- aggregate(vol ~ hd2, data = wood_df, sum)
names(wood_inter) <- c("hd","tot_vol")
hist(wood_inter$tot_vol)
summary(wood_inter$tot_vol)

wood_df$dupes <- duplicated(wood_df$hd2) # Identify duplicates hd's
wood_df$hd <- ifelse(wood_df$dupes == FALSE,wood_df$hd2,NA)
wood_inter <- left_join(wood_inter,wood_df,by = c("hd"))
wood_inter <- select(wood_inter,hd,tot_vol,jam,jam_pa,reach)
interval_dia <- aggregate(impute_dia ~ hd2, data = wood_df, median)
names(interval_dia) <- c("hd","med_dia")
interval_len <- aggregate(impute_len ~ hd2, data = wood_df, median)
names(interval_len)<- c("hd","med_len")

wood_inter <- left_join(wood_inter,interval_dia,by = c("hd"))
wood_inter <- left_join(wood_inter,interval_len,by = c("hd"))

sum(wood_inter$tot_vol)

write.csv(wood_inter,"data/processed/wood_vol_1992.csv")




# Code scraps to ignore for now ------------------------------------------------


# Remove wood above the banks
wood_df$vol_chan <- ifelse(wood_df$pos == 0, wood_df$vol,
                           ifelse(wood_df$pos == 1, wood_df$vol / 2, 0))
wood_df$vol_above <- wood_df$vol - wood_df$vol_chan
summary(wood_df$vol_chan)

# Filter df by orientation, jam or free storage, and position:
para_nojam_in <- filter(wood_df,orient==1 & jam_pa == "no_jam" & pos == 0)
diag_nojam_in <- filter(wood_df,orient==2 & jam_pa == "no_jam" & pos == 0)
perp_nojam_in <- filter(wood_df,orient==3 & jam_pa == "no_jam" & pos == 0)

para_nojam_half <- filter(wood_df,orient==1 & jam_pa == "no_jam" & pos == 1)
diag_nojam_half <- filter(wood_df,orient==2 & jam_pa == "no_jam" & pos == 1)
perp_nojam_half <- filter(wood_df,orient==3 & jam_pa == "no_jam" & pos == 1)

para_nojam_above <- filter(wood_df,orient==1 & jam_pa == "no_jam" & pos == 2)
diag_nojam_above <- filter(wood_df,orient==2 & jam_pa == "no_jam" & pos == 2)
perp_nojam_above <- filter(wood_df,orient==3 & jam_pa == "no_jam" & pos == 2)

para_jam_in <- filter(wood_df,orient==1 & jam_pa == "jam" & pos == 0)
diag_jam_in <- filter(wood_df,orient==2 & jam_pa == "jam" & pos == 0)
perp_jam_in <- filter(wood_df,orient==3 & jam_pa == "jam" & pos == 0)

para_jam_half <- filter(wood_df,orient==1 & jam_pa == "jam" & pos == 1)
diag_jam_half <- filter(wood_df,orient==2 & jam_pa == "jam" & pos == 1)
perp_jam_half <- filter(wood_df,orient==3 & jam_pa == "jam" & pos == 1)

para_jam_above <- filter(wood_df,orient==1 & jam_pa == "jam" & pos == 2)
diag_jam_above <- filter(wood_df,orient==2 & jam_pa == "jam" & pos == 2)
perp_jam_above <- filter(wood_df,orient==3 & jam_pa == "jam" & pos == 2)

# Aggregate data by jam

aggregate(vol ~ jam, data = para_jam, sum)
aggregate(vol_chan ~ jam, data = diag_jam, sum)
aggregate(vol_chan ~ jam, data = perp_jam, sum)

para_jam %>% count(jam)
diag_jam %>% count(jam)
perp_jam %>% count(jam)









# Aggregate logs into survey intervals:
vol <- aggregate(vol ~ hd, data = wood_df, sum)
vol_above <- aggregate(vol_above ~ hd, data = wood_df, sum)
vol_df <- left_join(vol_chan,vol_above)
vol_df$total_vol <- vol_df$vol_chan + vol_df$vol_above
summary(vol_df$total_vol)
hist(vol_df$total_vol)


write.csv(vol,"fan92.csv")


vol






