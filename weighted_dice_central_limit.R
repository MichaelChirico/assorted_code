#This code can be used to show how the distribution
# of sums of dice (numbered sequentially 1 to K)
# quickly become normal as the 
# number of dice increase, as well as
# "tightening" about the population average

#initial distribution of x
#  (could be renormalized to sum to one,
#   but this doesn't matter--this is
#   done below)
x<-c(100,5,9,16,25,36)
K<-length(x)

#How many dice do you want in the final estimate?
#  (expressed in log_2, i.e., if you want
#   2 dice, set NN=1, 4 dice NN=2, 8 dice NN=3, etc.)
NN<-7 #keep in mind this requires exponentially more
      #  memory as NN increases... 7 is already high.

#calculated recursively
for (nn in 1:NN){
  x<-x/sum(x)
  #basically using independence of rolls
  #  to get matrix of joint probability of 
  #  set 1 realizing to ii and set 2 to jj
  y<- cbind(x) %x% rbind(x)
  #total probabilities accumulate along
  #  the fixed-sum diagonals
  #  (a la 2 dice, when the sums are fixed on
  #   all up-right diagonals)
  x<-sapply(2:(2*nrow(y)),function(z)sum(y[row(y)+col(y)==z]))
}

plot(seq(1,K,length.out=length(x)),x,type="l",lwd=3,
     ylab="Density",xlab="Dice Roll",
     main=paste0("Exact Distribution of Sample Mean\nAfter ",2^NN," Rolls"))
