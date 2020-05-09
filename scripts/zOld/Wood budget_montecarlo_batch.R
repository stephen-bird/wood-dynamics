#######################################################################################
# Wood budget model - 2019 update

# By DR
# Modified from model in Hassan et a. 2016 

# Model purpose:
#
# Taakes inputs and subtracts outputs to calculate a wood storage quantity through time
# requires a lot of input field data
#######################################################################################

# clear workspace
rm(list = ls(all = TRUE))

# load packages
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(plyr)
library(dplyr)

computer= "00dav"

wd = paste("C:/Users/",computer, "/OneDrive/Work/Projects/Carnation/Data/Wood/Budget model/", sep = "")
wd_save = paste("C:/Users/",computer, "/OneDrive/Work/Projects/Carnation/Data/Wood/Budget model/Output data", sep = "")
wd_save_figs = paste("C:/Users/",computer, "/OneDrive/Work/Projects/Carnation/Data/Wood/Budget model/Output figures/", sep = "")


######################################################################################
# define constants (which may be varied for monte carlo)

# these are the lengths, in meters, of the two reaches
reach_lengths = c(1300, 1600)

# An initial guess at storage
V_o = matrix(nrow = 2, ncol = 1)
V_o[,1] = c(1000, 1000)


# Decay constants - determined empirically as 1/mean age of jams
k_val = c(0.020, 0.020)

# date sequence
Dates = seq(1801,2200,1)

# riparian stand density info from VRI map - pre-harvesting conditions
stand_dens1 = 880 # from sample of riparian VIR polygons in catchment

# bank erosion rate (m/yr)
BE_rate = c(0.23,0.28)

# date of logging
logdate = 1978

# parameters for transport, based on analysis of SA9 wood data
P_move = c(0.78, 0.75, 0.47)  # estimated from figure 2 in eaton et a. 2012
P_angle = c(0.87, 0.87, 0.87) # estimated from figure 2 in eaton et a. 2012
Wb_wood = c(16.2, 15.4, 10.4) # these are assumed constant but they are not in reality
Lw_wood = c(5.7, 5.7, 5.7)    # Mean wood length from SA9 dist 2007
Step = c(42.5, 37.7, 15.7)    # these are calculated from Eaton, Davidson, Hassan eq. 7

# upstream unit storage, based on SA9 data from 2007
Uptrans_Dens = 1.1

# CWD volume/ha values from literature
CWD_Param = c(636, 698, 307)

######################################################################################
# lists to save - these are to plot timeseries of the different budget components
# e.g. show change in transport, mortality through time. 
######################################################################################
US_percentile_list = list()
DS_percentile_list = list()
Total_percentile_list = list()

US_Mins_list = list()
DS_Mins_list = list()
Total_Mins_list = list()

# lists of output variables
US_Storage_list = list()
DS_Storage_list = list()
Total_Storage_list = list()

US_Trans_list = list()
DS_Trans_list = list()

US_BE_list = list()
DS_BE_list = list()

US_CA_list = list()
DS_CA_list = list()

US_Mort_list = list()
DS_Mort_list = list()

US_Rip_list = list()
DS_Rip_list = list()

US_Dec_list = list()
DS_Dec_list = list()

US_CWD_list = list()
DS_CWD_list = list()
######################################################################################
# start the main loop
######################################################################################

nbatch = 1
for(h in 1:nbatch){

##################################
# number of simulations to run ### 1000 takes 3 hrs
nsim = 10  ######################
##################################

#####################################################################################
# files to save for output
#####################################################################################
US_Storage = matrix(nrow = length(Dates), ncol = nsim)
DS_Storage = matrix(nrow = length(Dates), ncol = nsim)
Total_Storage = matrix(nrow = length(Dates), ncol = nsim)

# transport data
US_Trans = matrix(nrow = length(Dates), ncol = nsim)
DS_Trans = matrix(nrow = length(Dates), ncol = nsim)

# Bank erosion
US_BE = matrix(nrow = length(Dates), ncol = nsim)
DS_BE = matrix(nrow = length(Dates), ncol = nsim)

# loss from channel abandonment data
US_CA = matrix(nrow = length(Dates), ncol = nsim)
DS_CA = matrix(nrow = length(Dates), ncol = nsim)

# mortality data
US_Mort = matrix(nrow = length(Dates), ncol = nsim)
DS_Mort = matrix(nrow = length(Dates), ncol = nsim)

# Riparian stand density
US_rip = matrix(nrow = length(Dates), ncol = nsim)
DS_rip = matrix(nrow = length(Dates), ncol = nsim)

# Loss from decay
US_Dec = matrix(nrow = length(Dates), ncol = nsim)
DS_Dec = matrix(nrow = length(Dates), ncol = nsim)

# CWD
US_CWD = matrix(nrow = length(Dates), ncol = nsim)
DS_CWD = matrix(nrow = length(Dates), ncol = nsim)


######################################################################################
# Field observational data

# this data is extracted from the profile data worksheets
Obs_wood = as.data.frame(matrix(nrow = 4, ncol = 4))

Obs_wood[,1] = c(1991,1999,2009,2017)
Obs_wood[,2] = c(1157, 2014, 1490, 655)
Obs_wood[,3] = c(2411,2289,881,754)
Obs_wood[,4] = Obs_wood[,2] + Obs_wood[,3]

names(Obs_wood) = c("Date", "DS", "US", "Total")

Obs_Errors = Obs_wood
Obs_Errors[,2:4] = Obs_Errors[,2:4]*0.5

# load SA wood data
SAwood = read.csv(paste(wd, "SAwood_model.csv", sep = ""))

#######################################################################################
# Define recovery growth rate

# output from TIPSY forest regen model
recovery = read.csv(paste(wd, "TIPSYregen.csv", sep = ""))

rip_stand_dens = matrix(nrow = length(Dates), ncol = 2)

CWD_recovery = read.csv(paste(wd, "TIPSYCWD.csv", sep = ""))
CWD_recovery = CWD_recovery[1:(max(Dates)-(logdate-1)),]


######################################################################################
######################################################################################
#####################################################################################
# Begin loop for simulation

for(t in 1:nsim){
  
  # Vary some parameters
  stand_dens1 = rnorm(1, 880, 162)
  
  # crop length of recovery vector to match the simulation length
  target = max(Dates) - (logdate-1)
  
  # vary recovery by multiplyng by a factor which captures range of probable outcomes
  recovery_factor = runif(1, 0.9, 1.11)
  
  recovery = recovery[1:target,]
  
  # multiply by recovery factor
  recovery$Inputs = recovery$Inputs*recovery_factor
  
  # Define baseline stand density
  rip_stand_dens[,1:2] = stand_dens1
  
  # replace the back half for the downstream reach with the recovery data
  rip_stand_dens[(logdate-(Dates[1]-1)):length(Dates),1] = recovery$Inputs
  rip_stand_dens[(logdate-(Dates[1]-1)):length(Dates),2] = recovery$Inputs
  
  # transport step length
  Step[1] = runif(1,38.5, 46.5)
  Step[2] = runif(1,32.5, 42.5)
  Step[3] = runif(1, 7, 20)

  
  # k values to vary
  
 kval= rnorm(1, 0.021, 0.0024)
  k_val = c(kval, kval)
  
  # CWD parameters to calculate - using SD values reported in the logging CWD paper
  cwd1 = rnorm(1, 636, 160)
  cwd2 = rnorm(1, 698, 160)

  
  CWD_Param = c(cwd1, cwd2)
######################################################################################
# external input data

# function to calculate mortality

# should be timeseries of whatever variables are of interest


# dummy mortality input
Mortality = matrix(nrow = length(Dates), ncol = 2)
Mortality[,1] = (reach_lengths[1]/1000)*0.4
Mortality[,2] = (reach_lengths[2]/1000)*0.4
Mortality[178:250,1] = (reach_lengths[2]/1000)*0
Mortality[250:length(Dates),1] = (reach_lengths[1]/1000)*0.2 # values from Riley and Gregory paper
Mortality[250:length(Dates),2] = (reach_lengths[2]/1000)*0.2 # values from Riley and Gregory paper

# dummy bank erosion input
Bank_Erosion = matrix(nrow = length(Dates), ncol = 2)
Bank_Erosion[,1] =  ((reach_lengths[1]*BE_rate[1])/10000)*rip_stand_dens[,1]
Bank_Erosion[,2] =  ((reach_lengths[2]*BE_rate[2])/10000)*rip_stand_dens[,2]

# CWD input, add to BE data
# Based on rates reported in Stevens 1997 report + logging rates - but after clearcutting you get a spike followed by decay

CWD = Bank_Erosion
# for the downstream reach
CWD[,1] = (CWD[,1]*0)+((reach_lengths[1]*(BE_rate[1]))/10000)*CWD_Param[1]

# for upstream reach
CWD[,2] = (CWD[,1]*0)+((reach_lengths[2]*(BE_rate[2]))/10000)*CWD_Param[1]

CWD[(logdate-(Dates[1]-1)),1] = (CWD[logdate-(Dates[1]-1), 1]*0)+((reach_lengths[1]*(BE_rate[1]))/10000)*CWD_Param[2] # pre=harvest conditions
CWD[(logdate-(Dates[1]-1)),2] = (CWD[logdate-(Dates[1]-1), 1]*0)+((reach_lengths[2]*(BE_rate[2]))/10000)*CWD_Param[2] # pre=harvest conditions

#CWD[((logdate+10)-(Dates[1]-1)):length(Dates),2] = (CWD[logdate-(Dates[1]-1), 1]*0)+((reach_lengths[2]*(BE_rate[2]))/10000)*307 # pre=harvest conditions

# small loop to calculate decay of CWD on teh banks
#for(i in (logdate-(Dates[1]-1)):length(Dates)-1){
for(i in 178:399){
  CWD[i,1] = CWD[i-1,1]*exp(-0.027) + ((BE_rate[1]*reach_lengths[1])/(10000))*(CWD_recovery$CWD_vol[i+1-(logdate-min(Dates))])
  CWD[i,2] = CWD[i-1,2]*exp(-0.027) + ((BE_rate[2]*reach_lengths[2])/(10000))*(CWD_recovery$CWD_vol[i+1-(logdate-min(Dates))])
}

CWD = as.data.frame(CWD)

# the cwd recovery from tipsy yields some odd results so clip after it exceeds pre-harvest conditions
CWD$V1[CWD$V1>CWD$V1[2]] =  CWD$V2[2]
CWD$V2[CWD$V2>CWD$V2[2]] =  CWD$V2[2]

# sum inputs into single matrix
Total_Inputs = Mortality + Bank_Erosion + CWD

# store data in matrices for plotting later
# Bank erosion
US_BE[,t] = Bank_Erosion[,2]
DS_BE[,t] = Bank_Erosion[,1]

# CWD
US_CWD[,t] =CWD[,2]
DS_CWD[,t] =CWD[,1]

#mortality
US_Mort[,t] = Mortality[,2]
DS_Mort[,t] = Mortality[,1]


######################################################################################
# calculate storage and transport 

# Diagonalize matrix for some reason
DS_Reach_Stor = as.data.frame(diag(Total_Inputs[,1]))
US_Reach_Stor = as.data.frame(diag(Total_Inputs[,2]))


# add an initial storage value?
DS_Reach_Stor[2,2] = V_o[1]
US_Reach_Stor[2,2] = V_o[2]

# Empty matrix of transport 
Tnet_DS = DS_Reach_Stor*0
Tnet_US = DS_Reach_Stor*0

# some transport parameters
# US_wb
# DS_wb


#######################################################################################
# Run calculation
#######################################################################################


for(i in 2:nrow(DS_Reach_Stor)){
  
    for(j in 2:ncol(DS_Reach_Stor)-1){
      
      if(DS_Reach_Stor[i,j] > 0){ # Identify starting point on diagonal matrix
        
            if(DS_Reach_Stor[i,j-1] == 0){ # calculate transport, add as input. 
      
                  # here, net transport in the reach (in-out) is calculated 
                  # based on the proportion of load that transport composes
                  # in each reach, on average. This was determined using
                  # methods in Eaton et al 2012, and is an approximation of
                  # true wood transport. 
                  
                 # calculate transport effect
              
                  Tnet_US[i,j] = (Uptrans_Dens*Step[3]*(P_move[3]*P_angle[3]) - # incoming transport
                                   ((sum(US_Reach_Stor[,j-1], na.rm = TRUE)/reach_lengths[2]))*Step[2]*(P_move[2]*P_angle[2])) #outgoing
                  
                  Tnet_DS[i,j] = (sum(US_Reach_Stor[,j-1], na.rm = TRUE)/reach_lengths[2])*Step[2]*(P_move[2]*P_angle[2]) -  # incoming transport
                                    ((sum(DS_Reach_Stor[,j-1], na.rm = TRUE)/reach_lengths[1])*Step[1]*(P_move[1]*P_angle[1])) #outgoing
            
                  # add transport to other inputs; this becomes new wood
                  # added each year to each reach
            
                  DS_Reach_Stor[i,j] = DS_Reach_Stor[i,j] + Tnet_DS[i,j] 
                  US_Reach_Stor[i,j] = US_Reach_Stor[i,j] + Tnet_US[i,j]

       
          # end the second conditional  
            }
        
            # track decay of new inputs through time
        DS_Reach_Stor[i,j+1] =  DS_Reach_Stor[i,j]*exp(-k_val[1]) - (DS_Reach_Stor[i,j]*0.01)
        US_Reach_Stor[i,j+1] =  US_Reach_Stor[i,j]*exp(-k_val[2]) - (US_Reach_Stor[i,j]*0.01)
         

      
      # end first conditional
      }
   }
}

##############################################################################
# Organize and rearrange the outputs
##############################################################################
#un-diagonalize transport data so it can be included in error calc
# net transport
US_Trans[,t] = rowSums(Tnet_US)
DS_Trans[,t] = rowSums(Tnet_DS)

Modeled_Storage_DS = matrix(nrow = length(Dates), ncol = 1)
Modeled_Storage_US = Modeled_Storage_DS

# I think this takes the row sums of the diagonal storage matrix to get a single timeseries
Modeled_Storage_DS = colSums(DS_Reach_Stor)
Modeled_Storage_US = colSums(US_Reach_Stor)

# loss from channel abandonment data
US_CA[,t] = Modeled_Storage_US*0.01
DS_CA[,t] = Modeled_Storage_DS*0.01


# totals of both reaches combined
Modeled_Storage_all = Modeled_Storage_DS + Modeled_Storage_US


# Group the mote-carlo output into respective matrices

US_Storage[,t] = Modeled_Storage_US
DS_Storage[,t] = Modeled_Storage_DS

Total_Storage[,t] = Modeled_Storage_all



###############################################################################
#### End simulations
###############################################################################
}

# test plot



#jpeg(paste(wd, "simtest.jpg", sep = ""), width = 500, height = 500, unit = "px" )

# windows()
# plot(Dates, US_Storage[,1], type = "l", xlim = c(1900,2200), ylim = c(0,4000))
# for(w in 1:ncol(US_Storage)){
#   lines(Dates, US_Storage[,w])
# }
# points(Obs_wood$Date, Obs_wood$US, pch = 21, bg = "blue")

##########################################################################################
# Find the minimums of each simulatio
##########################################################################################

Min_US = matrix(nrow = nsim, ncol = 1)
Min_DS = Min_US
Min_All = Min_US


# find minimum values for plotting
for(i in 1:nsim){
Min_US[i,1] = (which(US_Storage[178:length(Dates),i] == min(US_Storage[178:length(Dates),i], na.rm = TRUE), arr.ind = TRUE))+logdate
Min_DS[i,1] = (which(DS_Storage[178:length(Dates),i] == min(DS_Storage[178:length(Dates),i], na.rm = TRUE), arr.ind = TRUE))+logdate
Min_All[i,1] = 1*(which(Total_Storage[178:length(Dates),i] == min(Total_Storage[178:length(Dates),i], na.rm = TRUE), arr.ind = TRUE))+logdate
}


US_Mins = as.data.frame(quantile(Min_US, c(.05,.95, .50), na.rm = TRUE))
DS_Mins = as.data.frame(quantile(Min_DS, c(.05,.95, .50), na.rm = TRUE))
Total_Mins = as.data.frame(quantile(Min_All, c(.05,.95, .50), na.rm = TRUE))

US_Mins = as.data.frame(t(US_Mins))
DS_Mins = as.data.frame(t(DS_Mins))
Total_Mins = as.data.frame(t(Total_Mins))

names(US_Mins) = c("Lower", "Upper", "Median")
names(DS_Mins) = c("Lower", "Upper", "Median")
names(Total_Mins) = c("Lower", "Upper", "Median")
########################################################################################3
# Plot the outputs
#########################################################################################

## find 95% CI

percentiles_US = as.data.frame(matrix(nrow = length(Dates), ncol = 4))
percentiles_DS= percentiles_US
percentiles_Total = percentiles_US

names(percentiles_US) = c("Dates", "Lower", "Upper", "Median")
names(percentiles_DS) =c("Dates", "Lower", "Upper", "Median")
names(percentiles_Total) = c("Dates", "Lower", "Upper", "Median")

percentiles_US[,1] = Dates
percentiles_DS[,1] = Dates
percentiles_Total[,1] = Dates

for(i in 1:length(Dates)){
percentiles_US[i,2:4] = (quantile(US_Storage[i,], c(.05,.95, .50), na.rm = TRUE))/reach_lengths[2]
percentiles_DS[i,2:4] = (quantile(DS_Storage[i,], c(.05,.95, .50), na.rm = TRUE))/reach_lengths[1] 
percentiles_Total[i,2:4] = (quantile(Total_Storage[i,], c(.05,.95, .50), na.rm = TRUE))/(reach_lengths[2]+reach_lengths[1])
}

### save stuff in lists

US_BE1 = rowMeans(US_BE)
DS_BE1 = rowMeans(DS_BE)
Total_BE1 = US_BE+DS_BE

US_CWD1 = rowMeans(US_CWD)
DS_CWD1 = rowMeans(DS_CWD)
Total_CWD1 = US_CWD + DS_CWD

US_Mort1 = rowMeans(US_Mort)
DS_Mort1 = rowMeans(DS_Mort)
Total_Mort1 = US_Mort + DS_Mort

US_Trans1 = rowMeans(US_Trans)
DS_Trans1 = rowMeans(DS_Trans)
Total_Trans1 = US_Trans + DS_Trans

US_CA1 = (rowMeans(US_CA))*-1
DS_CA1 = (rowMeans(DS_CA))*-1
Total_CA1 = US_CA + DS_CA

# model inputs
US_Trans_list[[h]] = US_Trans1
DS_Trans_list[[h]] = DS_Trans1

US_BE_list[[h]] = US_BE1
DS_BE_list[[h]] = DS_BE1

US_CA_list[[h]] = US_CA1
DS_CA_list[[h]] = DS_CA1

US_Mort_list[[h]] = US_Mort1
DS_Mort_list[[h]] = DS_Mort1

US_CWD_list[[h]] = US_CWD1
DS_CWD_list[[h]] = DS_CWD1

#percentiles and mins

US_percentile_list[[h]] = percentiles_US
DS_percentile_list[[h]] = percentiles_DS
Total_percentile_list[[h]] = percentiles_Total

US_Mins_list[[h]] =  US_Mins
DS_Mins_list[[h]] = DS_Mins
Total_Mins_list[[h]] = Total_Mins

###############################################################################################################
# end the main loop here
}
###############################################################################################################

# combine error data for plotting
names(Obs_Errors) = c("Dates1", "DS_error", "US_error", "Total_error")
Obs_wood = cbind(Obs_wood, Obs_Errors)

# scale by length
Obs_wood$DS = Obs_wood$DS/reach_lengths[1] 
Obs_wood$US = Obs_wood$US/reach_lengths[2] 
Obs_wood$Total = Obs_wood$Total/(reach_lengths[2]+reach_lengths[1])

Obs_wood$DS_error = Obs_wood$DS_error/reach_lengths[1] 
Obs_wood$US_error = Obs_wood$US_error/reach_lengths[2] 
Obs_wood$Total_error = Obs_wood$Total_error/(reach_lengths[2]+reach_lengths[1])

# get the rect max value for plotting
US_max = percentiles_US[(US_Mins$Median-min(Dates)),4]
DS_max = percentiles_DS[(DS_Mins$Median-min(Dates)),4]
Total_max = percentiles_Total[(Total_Mins$Median-min(Dates)),4]



#################################################################################3
# take the mean of all the dataframes in the lists
#################################################################################


percentiles_US = Reduce("+", US_percentile_list)/nbatch
percentiles_DS = Reduce("+", DS_percentile_list)/nbatch
percentiles_Total = Reduce("+", Total_percentile_list)/nbatch

US_Mins = Reduce("+", US_Mins_list)/nbatch
DS_Mins = Reduce("+", DS_Mins_list)/nbatch
Total_Mins = Reduce("+", Total_Mins_list)/nbatch
# create matrix of values to show pre-logging conditions plotted behind the main figure

US_Pre = percentiles_US
DS_Pre = percentiles_DS
Total_Pre = percentiles_Total

US_Pre[1:400,] = percentiles_US[176,]
DS_Pre[1:400,] = percentiles_DS[176,]
Total_Pre[1:400,] = percentiles_Total[176,]

US_Pre[,1] = Dates
DS_Pre[,1] = Dates
Total_Pre[,1] = Dates




#########################################################################
# nice plots
#########################################################################







#######################################################################
# the plots

# upstream reach
US_plot= ggplot() + 
  
  # ribbon plot showing best fit for no-model condition
  geom_ribbon(data = US_Pre, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "red", alpha=0.10)+
  geom_line(data = US_Pre, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 2)+  
  
  # rectangle showing range of timescales for main simulation
  geom_rect(data = US_Mins, aes(xmin = Lower, xmax = Upper, ymin = 0, ymax = US_max), fill = "#4682B4", alpha = 0.3)+
  
  geom_segment(data = US_Mins, aes(x = Median, y = 0, xend = Median, yend = US_max), color = "#4682B4", linetype = 2, size = 0.7 )+
  # Ribbon with ranges
  geom_ribbon(data = percentiles_US, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "grey", alpha=0.90)+
  geom_line(data = percentiles_US, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 1)+  
  geom_vline(xintercept = 1978, linetype = "twodash", color= "black", size = 0.7)+
  
  
  # For SA data
  geom_errorbar(data = SAwood, mapping = aes(x = Date, ymin = (Up_scaled - Up_errors), ymax=(Up_scaled + Up_errors)), width=.1,  color = "grey43",
                position=position_dodge(0.05))+
  geom_point(data = SAwood, aes(x = Date, y = Up_scaled, color = "goldenrod"), size = 1)+
  guides(color=FALSE)+
  
  
  # Error bars
   geom_errorbar(data = Obs_wood, mapping = aes(x = Date, ymin=(US-US_error), ymax=(US+US_error)), color = "black", width=1,
                position=position_dodge(0.05))+
  geom_point(data = Obs_wood, aes(x = Date, y = US, color = "orange"), size = 2)+
  
  guides(color=FALSE)+
  
  
  theme_bw()+ theme(legend.position = "none", legend.text=element_text(size=10))+
  xlim(1950,2200)+
  labs(x = "Date", y = expression(paste("Wood storage (m"^"3","m"^"-1",")")), title = "(a)")



# downstream reach
DS_plot= ggplot() + 
  
  # ribbon plot showing best fit for no-model condition
  geom_ribbon(data = DS_Pre, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "red", alpha=0.10)+
  geom_line(data = DS_Pre, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 2)+  
  
  geom_rect(data = DS_Mins, aes(xmin = Lower, xmax = Upper, ymin = 0, ymax = DS_max), fill = "#4682B4", alpha = 0.3)+
  geom_segment(data = DS_Mins, aes(x = Median, y = 0, xend = Median, yend = DS_max), color = "#4682B4", linetype = 2, size = 0.7 )+
  geom_ribbon(data = percentiles_DS, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "grey", alpha=0.90)+
  geom_line(data = percentiles_DS, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 1)+  
  geom_vline(xintercept = 1978, linetype = "twodash", color= "black", size = 0.7)+


  
  # For SA data
  geom_errorbar(data = SAwood, mapping = aes(x = Date, ymin = (Down_scaled - Down_errors), ymax=(Down_scaled + Down_errors)), width=.1,
                color = "grey43", position=position_dodge(0.05))+
  geom_point(data = SAwood, aes(x = Date, y = Down_scaled, color = "goldenrod"), size = 1)+
  guides(color=FALSE)+
  
  
  # for profile data
  geom_errorbar(data = Obs_wood, mapping = aes(x = Date, ymin=(DS-DS_error), ymax=(DS+DS_error)), width=.5,
                position=position_dodge(0.05))+
  geom_point(data = Obs_wood, aes(x = Date, y = DS, color = "orange"), size = 2)+
  guides(color=FALSE)+
  
  theme_bw()+ theme(legend.position = "none", legend.text=element_text(size=10))+
  xlim(1950,2200)+
  labs(x = "Date", y = expression(paste("Wood storage (m"^"3","m"^"-1",")")), title = "(b)")

# data grouped
Total_plot= ggplot() + 
  
  # ribbon plot showing best fit for no-model condition
  geom_ribbon(data = Total_Pre, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "red", alpha=0.10)+
  geom_line(data = Total_Pre, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 2)+  
  
  geom_rect(data = Total_Mins, aes(xmin = Lower, xmax = Upper, ymin = 0, ymax = Total_max), fill = "#4682B4", alpha = 0.3)+
  geom_segment(data = Total_Mins, aes(x = Median, y = 0, xend = Median, yend = Total_max), color = "#4682B4", linetype = 2, size = 0.7 )+
  geom_ribbon(data = percentiles_Total, aes(x=Dates, ymin=Lower, ymax=Upper), fill = "grey", alpha=0.90)+
  geom_line(data = percentiles_Total, aes(x = Dates, y = Median), size = 0.7, color = "#4682B4", linetype = 1)+  
  geom_vline(xintercept = 1978, linetype = "twodash", color= "black", size = 0.7)+

  
  # For SA data
  geom_errorbar(data = SAwood, mapping = aes(x = Date, ymin = (Total_scaled - Total_errors), ymax=(Total_scaled + Total_errors)), color = "grey43",
                width=.1, position=position_dodge(0.05))+
  geom_point(data = SAwood, aes(x = Date, y = Total_scaled, color = "goldenrod"), size = 1)+
  guides(color=FALSE)+
  
  
  geom_errorbar(data = Obs_wood, mapping = aes(x = Date, ymin=(Total-Total_error), ymax=(Total+Total_error)), width=.5,
                position=position_dodge(0.05))+
  geom_point(data = Obs_wood, aes(x = Date, y = Total, color = "orange"), size = 2)+
  guides(color=FALSE)+
  
  
  theme_bw()+ theme(legend.position = "none", legend.text=element_text(size=10))+
  xlim(1950,2200)+
  labs(x = "Date", y = expression(paste("Wood storage (m"^"3","m"^"-1",")")), title = "(c)")


windows()
grid.arrange(US_plot, DS_plot, Total_plot, nrow = 3)

fig_out = grid.arrange(US_plot, DS_plot, Total_plot, nrow = 3)

# ggsave(file = "All_log.pdf", plot = fig_out, device = "pdf", path = wd_save_figs,
#        scale = 1, width = 6.7, height = 8, units = "in",
#        dpi = 900)

######################################################################################################
# rough plot of the budget terms
######################################################################################################
# unlist the budget terms
US_BE = Reduce("+", US_BE_list)/nbatch
DS_BE = Reduce("+", DS_BE_list)/nbatch

US_Mort = Reduce("+", US_Mort_list)/nbatch
DS_Mort = Reduce("+", DS_Mort_list)/nbatch

US_CA = (Reduce("+", US_CA_list)/nbatch)*-1
DS_CA = (Reduce("+", DS_CA_list)/nbatch)*-1

US_CWD = Reduce("+", US_CWD_list)/nbatch
DS_CWD = Reduce("+", DS_CWD_list)/nbatch

US_Trans = Reduce("+", US_Trans_list)/nbatch
DS_Trans = Reduce("+", DS_Trans_list)/nbatch

#US_BE = rowMeans(US_BE)
#DS_BE = rowMeans(DS_BE)
Total_BE = US_BE+DS_BE

#US_CWD = rowMeans(US_CWD)
#DS_CWD = rowMeans(DS_CWD)
Total_CWD = US_CWD + DS_CWD

#US_Mort = rowMeans(US_Mort)
#DS_Mort = rowMeans(DS_Mort)
Total_Mort = US_Mort + DS_Mort

#US_Trans = rowMeans(US_Trans)
#DS_Trans = rowMeans(DS_Trans)
Total_Trans = US_Trans + DS_Trans

#US_CA = (rowMeans(US_CA))*-1
#DS_CA = (rowMeans(DS_CA))*-1
Total_CA = US_CA + DS_CA

US_Dec1 = US_CA
for(i in 2:length(Dates)){
  US_Dec1[i] = US_BE[i] + US_Mort[i] + US_CWD[i]+US_Trans[i]+US_CA[i] - (percentiles_US$Median[i-1] - percentiles_US$Median[i])
}

lty = c(1,1,1,2,2)
cols = c("black", "cadetblue", "goldenrod", "black", "cadetblue4")

# pdf(paste(wd_save_figs,"All_log_Terms.pdf", sep = ""), width=4.5, height=6.5) 
# 
# par(mfrow = c(3,1), mar = c(4,4,2,4))
# plot(Dates, US_BE, type = "l", col = cols[1], lty = lty[1], xlim = c(1900,2200), ylim = c(-50,50), ylab = expression(paste("Wood volume (m"^"3","/yr)")))
# lines(Dates, US_Mort, col = cols[2],lty = lty[2])
# lines(Dates[1:399], US_Trans[1:399], col = cols[3],lty = lty[3])
# lines(Dates, US_CA, col = cols[4],lty = lty[4])
# lines(Dates, US_CWD, col = cols[5],lty = lty[5])
# legend("topleft", bty = "n", legend = "(a)")
# 
# plot(Dates, DS_BE, type = "l", col = cols[1], lty = lty[1], xlim = c(1900,2200), ylim = c(-50,50), ylab = expression(paste("Wood volume (m"^"3","/yr)")))
# lines(Dates, DS_Mort, col = cols[2],lty = lty[2])
# lines(Dates[1:399], DS_Trans[1:399], col = cols[3],lty = lty[3])
# lines(Dates, DS_CA, col = cols[4],lty = lty[4])
# lines(Dates, DS_CWD, col = cols[5],lty = lty[5])
# legend("topleft", bty = "n", legend = "(b)")
# 
# plot(Dates, Total_BE, type = "l", col = cols[1], lty = lty[1], xlim = c(1900,2200), ylim = c(-100,100), ylab = expression(paste("Wood volume (m"^"3","/yr)")))
# lines(Dates, Total_Mort, col = cols[2],lty = lty[2])
# lines(Dates[1:399], Total_Trans[1:399], col = cols[3],lty = lty[3])
# lines(Dates, Total_CA, col = cols[4],lty = lty[4])
# lines(Dates, Total_CWD, col = cols[5],lty = lty[5])
# legend(bty = "n", "bottom", lty = lty, horiz = TRUE, col = cols, legend = c("BE (live)   ", "Mort.", "Trans.", "CA","BE (CWD)"), cex = 0.8)
# legend("topleft", bty = "n", legend = "(c)")
# dev.off()

##################################################################################################
# save the main output data as RDS objects
##################################################################################################

# raw percentiles 
# raw_out_percentiles = list(US_percentile_list, DS_percentile_list, Total_percentile_list)
# raw_out_mins = list(US_Mins_list, DS_Mins_list, Total_Mins_list)
# raw_out_terms = list(US_CWD_list, DS_CWD_list, US_BE_list, DS_BE_list, US_CA_list, DS_CA_list, US_Mort_list, DS_Mort_list, US_Trans_list, DS_Trans_list)
# 
# saveRDS(raw_out_percentiles, file = paste(wd_save, "/", "Percentiles_All_log.rds", sep = ""))
# saveRDS(raw_out_mins, file = paste(wd_save, "/", "Mins_All_log.rds", sep = ""))
# saveRDS(raw_out_terms, file = paste(wd_save, "/", "Terms_All_log.rds", sep = ""))

