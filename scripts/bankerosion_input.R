# Script with fuction that calcualtes the volume of logs transfered from the
# bank erosion to the channel. 

# I_ms returns the volume of wood inputted to the channel by year 

# by Carina Helm
# 2019-09-14

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the required pacakges for the function-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("sf")
library("here")
library("dplyr")


# erosion_input - a function that returns the volume of wood inputted to the channel by year (I_b)
erosion_input <- function(slides, VRI, V_cwd, E_Vb, Pf_be, B, ){
      
        # 