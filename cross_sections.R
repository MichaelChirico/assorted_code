setwd("~/Desktop/")
library(animation)

#Use the 3-d form of a line through two points:
#  x = x_0 + (x_1 - x_0)*t
#  (t = 0 -> x_0, t = 1 -> x_1)
#With this, we solve for when z = lambda, where
#  lambda is a percentage of the way from
#  floor to ceiling.
#Parameterize the floor and ceiling rods to be
#  of length 2, and orient such that the floor
#  rod has endpoints at (-1, 0, 0) and (1, 0, 0)

#parameters
nn <- 30      #nodes on ceiling/floor
nn1 <- nn - 1 #really only use this
th <- pi/6    #angle of intersection between 
              #floor and ceiling segments
n_cross <- 20 #number of cross-sections to examine

saveGIF(sapply(
  #find cross sections evenly
  #  from floor to ceiling
  seq(0, 1, length.out = n_cross),
  function(lam){
    #indices: node numbers on floor (k1)/ceiling (k2)
    plot(outer(0:nn1, 0:nn1, function(k1, k2)
      #x coordinate depends on k1 & k2
      (2*k1/nn1 - 1)*(1-lam) + lam*cos(th)*(2*k2/nn1-1)),
      #y coordinate depends only on k2
      rep(lam*sin(th)*(2*0:nn1/nn1 - 1), each = nn),
      main = "Horizontal Cross Sections",
      xlab = "x", ylab = "y", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
    #floor rod
    segments(-1, 0, 1, 0, lty = 2)
    #ceiling rod
    segments(-cos(th), -sin(th), cos(th), sin(th), lty = 2)
    text(-1, 1, paste0(round(100*lam), "% to the Ceiling"), pos = 4)
  }),
  "cross_sections.gif")

