#This code can be used to 

x<-c(1,5,8,9,10,13)
NN<-7 #keep in mind this 
for (nn in 1:7){
  y<- cbind(x) %x% rbind(x)
  x<-sapply(2:(2*nrow(y)),function(z)sum(y[row(y)+col(y)==z]))
}
plot(x,type="l",lwd=3)