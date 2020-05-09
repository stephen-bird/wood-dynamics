vol_1b# code to calcualte volume, length and diameter of wood input to the channel from the rparian area
# Work-in-progress


# Define functions -------------------------------------------------------------

# Volume of wood in the channel where the log spans the channel and rests upon two banks:
vol_2b <- function(r_bh,w,a,h,z,h_bh){
        
        pi * r_bh ^2 * w * asin(a) * 
                (3 * h^2 - 3* h * (w + 2 * z ) * 
                         asin(a) + (w^2 + 3 * w * z + 3 * z^2) * 
                         asin(a)^2) / (3 * (h - h_bh)^2)
}


# Volume of wood in the channel where the log rests upon one bank:
vol_1b <- function(r_bh,a,h,z,h_bh){
        
        pi * r_bh ^2 * (h - z * asin(a) )^3 / (3 * (h - h_bh)^2)
                
}


# Random tree fall direction

ps <- function(z, h) {
        
        acos(z / h) / pi
}

# Test -------------------------------------------------------------------------

# Dummy values:

r_bh_test <- 1
w_test <- 3
a_test <- 0.5
h_test <- 20
z_test <- 1
h_bh_test <- 1.3


vol_2b(r_bh_test, w_test, a_test, h_test, z_test, h_bh_test)

vol_1b(r_bh_test,a_test,h_test,z_test,h_bh_test)

ps(z_test, h_test)

# Looks good

a_test <- NULL

integrate(vol_2b, lower = 0, upper = Inf)



integrand123 <- function(a) {
        
        (1/(pi - asin(z/h)) - asin(z/h)) *
         
        (pi * r_bh ^2 * w * asin(a) * 
                (3 * h^2 - 3* h * (w + 2 * z ) * 
                         asin(a) + (w^2 + 3 * w * z + 3 * z^2) * 
                         asin(a)^2) / (3 * (h - h_bh)^2))
                }




integrate(integrand123, lower = asin((z + w)/ h), upper = pi - asin((z + w)/ h))
3.14-.08

pi - asin(0.073303828584)

          
          
integrate(integrand123, lower = 3.06, upper = 0.073303828584)




############

h<-10

vol_1b <- function(a){
        
        (1/(pi - asin(z/h)) - asin(z/h)) *
                ((pi * r_bh ^2 * (h - z * asin(a) )^3 / (3 * (h - h_bh)^2)) * (acos(z / h) / pi))
        
}

integrate(vol_1b, lower = -.02, upper = .02)



rd <- 0.25
w <- 10
h <- 20
z <- 2
hd <- 1.3





-(pi*rd^2*w*(h^2*(5*w^2+15*w*z+9*z^2)*sqrt((h^2-(w+z)^2)/h)-(w+z)^2*(6*h^2+w^2+3*w*z+3*z^2)*(log(cos(1/2*asin((w+z)/h)))-log(sin(1/2*asin((w+z)/h))))))/(6 * (h - hd)^2 * (w + z)^2 * acos((w + z)/h))











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








ev2_rnd(9.8,10,.1,1.5)


ev1_rnd <- function(h,z,rd,hd=1.3){
        
        
        # Function returns the expected volume, E(V) of a single log transfered to the channel from the riparain area given a random (rnd) tree-fall pattern.
        # Volume is computed only for the case where the log cross one (1) channel bank.
        # The equation is derived from Van Sickle and Gregory (1990) with the intergral solved in Mathematica (work completed in 2004-05). See "Solution 1.1 Input Volume".
        # Inputs include: h = tree height; z = distance from the nearest stream bank; rd = tree radius at breast heigth; hd = breast hieght (defaults to 1.3).

        1/(6 * (h - hd)^2 * acos(z/h)) * 
                (pi * rd^2 * (h^2 * (h * pi + 5 * z * sqrt(1 - z^2/h^2)) - 2 * h^3 * asin(z/h)
                              - z * (6 * h^2 + z^2) * 
                                      (log(cos(1/2 * asin(z/h))) - log(sin(1/2 * asin(z/h))))))
}
        
ev1_rnd(10,.1,1.5)


ev2b_rnd <- function(h,z,rd,w,hd=1.3){
        
        (pi * rd^2 * (h^2 * z * (z * sqrt(((h - w - z) * (h + w + z))/ h^2) * (6 * w + 5 * z) - 5 * (w + z)^2 * sqrt(1 - z^2/h^2)) + (w + z)^2 * (2 * h^3 * asin(z/h) - 2 * h^3 * asin((w + z)/h) + z * (6 * h^2 + z^2) * (log(cos(1/2 * asin(z/h))) -  log(cos(1/2 * asin((w + z)/h))) - log(sin(1/2 * asin(z/h))) +   log(sin(1/2 * asin((w + z)/h)))))))/(6 * (h - hd)^2 * (w + z)^2 * (asin(z/h) - asin((w + z)/h)))
        
}

h,z,rd,w,hd=1.3


ev2b_rnd(10,1,0.1,1.5)
ev2_rnd(10,1,0.1,20)












